package immortan

import immortan.fsm._
import fr.acinq.eclair._
import immortan.Channel._
import fr.acinq.eclair.wire._
import immortan.crypto.Tools._
import immortan.ChannelMaster._
import fr.acinq.eclair.channel._
import scala.concurrent.duration._
import immortan.utils.{PaymentRequestExt, Rx}
import fr.acinq.bitcoin.{ByteVector32, Crypto}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import immortan.ChannelListener.{Malfunction, Transition}
import fr.acinq.eclair.payment.{IncomingPacket, PaymentRequest}
import fr.acinq.eclair.transactions.{LocalFulfill, RemoteFulfill, RemoteReject}
import immortan.fsm.OutgoingPaymentMaster.CMDChanGotOnline
import fr.acinq.eclair.blockchain.TxConfirmedAt
import fr.acinq.eclair.router.RouteCalculation
import java.util.concurrent.atomic.AtomicLong
import com.google.common.cache.LoadingCache
import immortan.crypto.CanBeShutDown
import scala.collection.mutable
import rx.lang.scala.Subject
import scala.util.Try


object ChannelMaster {
  type PreimageTry = Try[ByteVector32]
  type PaymentInfoTry = Try[PaymentInfo]
  type RevealedLocalFulfills = Iterable[LocalFulfill]
  type TxConfirmedAtOpt = Option[TxConfirmedAt]

  type OutgoingAdds = Iterable[UpdateAddHtlc]
  type ReasonableResolutions = Iterable[ReasonableResolution]
  type ReasonableTrampolines = Iterable[ReasonableTrampoline]
  type ReasonableLocals = Iterable[ReasonableLocal]

  final val updateCounter = new AtomicLong(0)
  final val stateUpdateStream: Subject[Long] = Subject[Long]
  final val statusUpdateStream: Subject[Long] = Subject[Long]

  final val payMarketDbStream: Subject[Long] = Subject[Long]
  final val paymentDbStream: Subject[Long] = Subject[Long]
  final val relayDbStream: Subject[Long] = Subject[Long]
  final val txDbStream: Subject[Long] = Subject[Long]

  def next(stream: Subject[Long] = null): Unit = stream.onNext(updateCounter.incrementAndGet)
  final val inFinalized: Subject[IncomingProcessorData] = Subject[IncomingProcessorData]

  final val NO_PREIMAGE = ByteVector32.One
  final val NO_SECRET = ByteVector32.Zeroes

  def initResolve(ext: UpdateAddHtlcExt): IncomingResolution = IncomingPacket.decrypt(ext.theirAdd, ext.remoteInfo.nodeSpecificPrivKey) match {
    case Left(_: BadOnion) => fallbackResolve(secret = LNParams.secret.keys.fakeInvoiceKey(ext.theirAdd.paymentHash), ext.theirAdd)
    case Left(onionFailure) => CMD_FAIL_HTLC(Right(onionFailure), ext.remoteInfo.nodeSpecificPrivKey, ext.theirAdd)
    case Right(packet: IncomingPacket) => defineResolution(ext.remoteInfo.nodeSpecificPrivKey, packet)
  }

  def fallbackResolve(secret: PrivateKey, theirAdd: UpdateAddHtlc): IncomingResolution = IncomingPacket.decrypt(theirAdd, secret) match {
    case Left(failure: BadOnion) => CMD_FAIL_MALFORMED_HTLC(failure.onionHash, failureCode = failure.code, theirAdd)
    case Left(onionFailure) => CMD_FAIL_HTLC(Right(onionFailure), secret, theirAdd)
    case Right(packet: IncomingPacket) => defineResolution(secret, packet)
  }

  // Make sure incoming payment secret is always present
  private def defineResolution(secret: PrivateKey, pkt: IncomingPacket): IncomingResolution = pkt match {
    case packet: IncomingPacket.FinalPacket if packet.payload.paymentSecret != NO_SECRET => ReasonableLocal(packet, secret)
    case packet: IncomingPacket.NodeRelayPacket if packet.outerPayload.paymentSecret != NO_SECRET => ReasonableTrampoline(packet, secret)
    case packet: IncomingPacket.ChannelRelayPacket => CMD_FAIL_HTLC(LNParams.incorrectDetails(packet.add.amountMsat).asRight, secret, packet.add)
    case packet: IncomingPacket.NodeRelayPacket => CMD_FAIL_HTLC(LNParams.incorrectDetails(packet.add.amountMsat).asRight, secret, packet.add)
    case packet: IncomingPacket.FinalPacket => CMD_FAIL_HTLC(LNParams.incorrectDetails(packet.add.amountMsat).asRight, secret, packet.add)
  }

  def dangerousHCRevealed(allRevealed: Map[ByteVector32, RevealedLocalFulfills], tip: Long, hash: ByteVector32): Iterable[LocalFulfill] = {
    // Of all incoming payments inside of HCs for which we have revealed a preimage, find those which are dangerously close to expiration, but not expired yet
    allRevealed.getOrElse(hash, Iterable.empty).filter(tip >= _.theirAdd.cltvExpiry.toLong - LNParams.hcFulfillSafetyBlocks).filter(tip <= _.theirAdd.cltvExpiry.toLong - 3)
  }
}

case class InFlightPayments(out: Map[FullPaymentTag, OutgoingAdds], in: Map[FullPaymentTag, ReasonableResolutions] = Map.empty) {
  // Incoming HTLC tag is extracted from onion, corresponsing outgoing HTLC tag is stored in TLV, this way in/out can be linked
  val allTags: Set[FullPaymentTag] = out.keySet ++ in.keySet
}

class ChannelMaster(val payBag: PaymentBag, val chanBag: ChannelBag, val dataBag: DataBag, val pf: PathFinder) extends ChannelListener with ConnectionListener with CanBeShutDown { me =>
  val getPaymentInfoMemo: LoadingCache[ByteVector32, PaymentInfoTry] = memoize(payBag.getPaymentInfo)
  val initResolveMemo: LoadingCache[UpdateAddHtlcExt, IncomingResolution] = memoize(initResolve)
  val getPreimageMemo: LoadingCache[ByteVector32, PreimageTry] = memoize(payBag.getPreimage)
  val opm: OutgoingPaymentMaster = new OutgoingPaymentMaster(me)

  // This is defined as mutable set so wallet implementation can append listeners of its own at runtime here
  val localPaymentListeners: mutable.Set[OutgoingPaymentListener] = mutable.Set apply new OutgoingPaymentListener {
    override def wholePaymentSucceeded(data: OutgoingPaymentSenderData): Unit = opm process RemoveSenderFSM(data.cmd.fullTag)

    override def wholePaymentFailed(data: OutgoingPaymentSenderData): Unit = chanBag.db txWrap {
      // This method gets called after NO payment parts are left in system, irregardless of restarts
      dataBag.putReport(data.cmd.fullTag.paymentHash, data.failuresAsString)
      payBag.updAbortedOutgoing(data.cmd.fullTag.paymentHash)
      opm process RemoveSenderFSM(data.cmd.fullTag)
    }

    override def gotFirstPreimage(data: OutgoingPaymentSenderData, fulfill: RemoteFulfill): Unit = chanBag.db txWrap {
      // Note that this method MAY get called multiple times for multipart payments if fulfills happen between restarts
      getPaymentInfoMemo.get(fulfill.ourAdd.paymentHash).filter(_.status != PaymentStatus.SUCCEEDED).foreach { paymentInfo =>
        // Persist payment metadata if this is ACTUALLY the first preimage (otherwise payment would be marked as successful)
        payBag.addSearchablePayment(paymentInfo.description.queryText, fulfill.ourAdd.paymentHash)
        payBag.updOkOutgoing(fulfill, data.usedFee)

        if (data.inFlightParts.nonEmpty) {
          // Sender FSM won't have in-flight parts after restart
          dataBag.putReport(fulfill.ourAdd.paymentHash, data.usedRoutesAsString)
          // We only increment scores for normal channels, never for HCs
          data.successfulUpdates.foreach(pf.normalBag.incrementScore)
        }
      }

      payBag.setPreimage(fulfill.ourAdd.paymentHash, fulfill.theirPreimage)
      getPaymentInfoMemo.invalidate(fulfill.ourAdd.paymentHash)
      getPreimageMemo.invalidate(fulfill.ourAdd.paymentHash)
    }
  }

  var all = Map.empty[ByteVector32, Channel]

  var inProcessors = Map.empty[FullPaymentTag, IncomingPaymentProcessor]

  var sendTo: (Any, ByteVector32) => Unit = (change, channelId) => all.get(channelId).foreach(_ process change)

  def finalizeIncoming(data: IncomingProcessorData): Unit = {
    // Let subscribers know after no incoming payment parts are left
    // payment itself may be fulfilled with preimage revealed or failed
    inProcessors -= data.fullTag
    inFinalized.onNext(data)
  }

  // CONNECTION LISTENER

  // Note that this may be sent multiple times after chain wallet reconnects
  override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit =
    allFromNode(worker.info.nodeId).foreach(_.chan process CMD_SOCKET_ONLINE)

  override def onMessage(worker: CommsTower.Worker, message: LightningMessage): Unit = message match {
    case msg: Fail if msg.channelId == ByteVector32.Zeroes => allFromNode(worker.info.nodeId).foreach(_.chan process msg)
    case msg: ChannelUpdate => allFromNode(worker.info.nodeId).foreach(_.chan process msg)
    case msg: HasChannelId => sendTo(msg, msg.channelId)
    case _ => // Do nothing
  }

  override def onHostedMessage(worker: CommsTower.Worker, message: HostedChannelMessage): Unit = message match {
    case msg: HostedChannelBranding => dataBag.putBranding(worker.info.nodeId, msg)
    case _ => hostedFromNode(worker.info.nodeId).foreach(_ process message)
  }

  override def onDisconnect(worker: CommsTower.Worker): Unit = {
    allFromNode(worker.info.nodeId).foreach(_.chan process CMD_SOCKET_OFFLINE)
    Rx.ioQueue.delay(5.seconds).foreach(_ => initConnect)
  }

  // CHANNEL MANAGEMENT

  override def becomeShutDown: Unit = {
    // Outgoing FSMs won't receive anything without channel listeners
    for (channel <- all.values) channel.listeners = Set.empty
    for (fsm <- inProcessors.values) fsm.becomeShutDown
    for (sub <- pf.subscription) sub.unsubscribe
    pf.listeners = Set.empty
  }

  def implantChannel(cs: Commitments, freshChannel: Channel): Unit = {
    // Note that this removes all listeners this channel previously had
    pf process PathFinder.CMDStartPeriodicResync
    all += Tuple2(cs.channelId, freshChannel)
    freshChannel.listeners = Set(me)
    next(statusUpdateStream)
    initConnect
  }

  def initConnect: Unit =
    all.values.flatMap(Channel.chanAndCommitsOpt).foreach { cnc =>
      // Connect to all peers with channels, including CLOSED ones
      CommsTower.listenNative(Set(me), cnc.commits.remoteInfo)
    }

  // Marks as failed those payments which did not make it into channels before an app has been restarted
  def markAsFailed(paymentInfos: Iterable[PaymentInfo], inFlightOutgoing: Map[FullPaymentTag, OutgoingAdds] = Map.empty): Unit = paymentInfos
    .collect { case outgoingPayInfo if !outgoingPayInfo.isIncoming && outgoingPayInfo.status == PaymentStatus.PENDING => outgoingPayInfo.fullTag }
    .collect { case fullTag if fullTag.tag == PaymentTagTlv.LOCALLY_SENT && !inFlightOutgoing.contains(fullTag) => fullTag.paymentHash }
    .foreach(payBag.updAbortedOutgoing)

  def allInChannelOutgoing: Map[FullPaymentTag, OutgoingAdds] = all.values.flatMap(Channel.chanAndCommitsOpt).flatMap(_.commits.allOutgoing).groupBy(_.fullTag)

  def allIncomingRevealed(cs: Iterable[Commitments] = Nil): Map[ByteVector32, RevealedLocalFulfills] = cs.flatMap(_.revealedFulfills).groupBy(_.theirAdd.paymentHash)

  def allFromNode(nodeId: PublicKey): Iterable[ChanAndCommits] = all.values.flatMap(Channel.chanAndCommitsOpt).filter(_.commits.remoteInfo.nodeId == nodeId)

  def allHostedCommits: Iterable[HostedCommits] = all.values.flatMap(Channel.chanAndCommitsOpt).collect { case ChanAndCommits(_, commits: HostedCommits) => commits }

  def hostedFromNode(nodeId: PublicKey): Option[ChannelHosted] = allFromNode(nodeId).collectFirst { case ChanAndCommits(chan: ChannelHosted, _) => chan }

  def allNormal: Iterable[ChannelNormal] = all.values.collect { case chan: ChannelNormal => chan }

  def delayedRefunds: DelayedRefunds = {
    val commitsPublished = all.values.map(_.data).flatMap { case close: DATA_CLOSING => close.forceCloseCommitPublished case _ => None }
    val spentParents = commitsPublished.flatMap(_.irrevocablySpent.values).map(confirmedAt => confirmedAt.tx.txid -> confirmedAt).toMap

    val result = for {
      delayedTx <- commitsPublished.flatMap(_.delayedRefundsLeft)
      parentTxid <- delayedTx.txIn.map(_.outPoint.txid)
      parentHeight = spentParents.get(parentTxid)
    } yield (delayedTx, parentHeight)
    DelayedRefunds(result.toMap)
  }

  // RECEIVE/SEND UTILITIES

  // It is correct to only use availableForReceive for both HC/NC and not take their maxHtlcValueInFlightMsat into account because:
  // - in NC case we always set local NC.maxHtlcValueInFlightMsat to channel capacity so NC.availableForReceive is always less than NC.maxHtlcValueInFlightMsat
  // - in HC case we don't have local HC.maxHtlcValueInFlightMsat at all and only look at HC.availableForReceive

  def operationalCncs(chans: Iterable[Channel] = Nil): Seq[ChanAndCommits] = chans.filter(Channel.isOperational).flatMap(Channel.chanAndCommitsOpt).toList

  def sortedReceivable(chans: Iterable[Channel] = Nil): Seq[ChanAndCommits] = operationalCncs(chans).filter(_.commits.updateOpt.isDefined).sortBy(_.commits.availableForReceive)

  def sortedSendable(chans: Iterable[Channel] = Nil): Seq[ChanAndCommits] = operationalCncs(chans).sortBy(_.commits.availableForSend)

  def maxReceivable(sorted: Seq[ChanAndCommits] = Nil): Option[CommitsAndMax] = {
    // Example: we have (5, 50, 60, 100) chans -> (50, 60, 100), receivable = 50*3 = 150
    // The idea is for smallest remaining operational channel to be able to handle an evenly split amount
    val withoutSmall = sorted.dropWhile(_.commits.availableForReceive * sorted.size < sorted.last.commits.availableForReceive).takeRight(4)
    val candidates = for (cs <- withoutSmall.indices map withoutSmall.drop) yield CommitsAndMax(cs, cs.head.commits.availableForReceive * cs.size)
    if (candidates.isEmpty) None else candidates.maxBy(_.maxReceivable).asSome
  }

  def maxSendable(chans: Iterable[Channel] = Nil): MilliSatoshi = {
    val inPrincipleUsableChans = chans.filter(Channel.isOperational)
    val sendableNoFee = opm.getSendable(inPrincipleUsableChans, maxFee = 0L.msat).values.sum
    val theoreticalMaxFee = LNParams.maxOffChainFeeAboveRatio.max(sendableNoFee * LNParams.maxOffChainFeeRatio)
    // Subtract max theoretical fee from EACH channel since ANY channel MAY use ALL of fee reserve
    opm.getSendable(inPrincipleUsableChans, maxFee = theoreticalMaxFee).values.sum
  }

  def makeSendCmd(prExt: PaymentRequestExt, toSend: MilliSatoshi, allowedChans: Seq[Channel], typicalChainTxFee: MilliSatoshi, capLNFeeToChain: Boolean): SendMultiPart = {
    val fullTag = FullPaymentTag(paymentHash = prExt.pr.paymentHash, paymentSecret = prExt.pr.paymentSecret.get, tag = PaymentTagTlv.LOCALLY_SENT)
    val extraEdges = RouteCalculation.makeExtraEdges(prExt.pr.routingInfo, target = prExt.pr.nodeId)

    val feeReserve = toSend * LNParams.maxOffChainFeeRatio match {
      case percent if percent < LNParams.maxOffChainFeeAboveRatio => LNParams.maxOffChainFeeAboveRatio
      case percent if percent > typicalChainTxFee && capLNFeeToChain => typicalChainTxFee
      case percent => percent
    }

    val split = SplitInfo(totalSum = 0L.msat, toSend)
    // Supply relative cltv expiry in case if we initiate a payment when chain tip is not yet known
    val chainExpiry = Right(prExt.pr.minFinalCltvExpiryDelta getOrElse LNParams.minInvoiceExpiryDelta)
    // An assumption is that toSend is at most maxSendable so max theoretically possible off-chain fee is already counted in, so we can send amount + fee
    SendMultiPart(fullTag, chainExpiry, split, LNParams.routerConf, targetNodeId = prExt.pr.nodeId, feeReserve, allowedChans, fullTag.paymentSecret, extraEdges)
  }

  def makePrExt(toReceive: MilliSatoshi, description: PaymentDescription, allowedChans: Seq[ChanAndCommits], hash: ByteVector32): PaymentRequestExt = {
    val hops = allowedChans.map(_.commits.updateOpt).zip(allowedChans).collect { case Some(update) ~ cnc => update.extraHop(cnc.commits.remoteInfo.nodeId) :: Nil }
    val pr = PaymentRequest(LNParams.chainHash, Some(toReceive), hash, LNParams.secret.keys.fakeInvoiceKey(hash), description.invoiceText, LNParams.incomingFinalCltvExpiry, hops.toList)
    PaymentRequestExt.from(pr)
  }

  def localSend(cmd: SendMultiPart): Unit = {
    opm process CreateSenderFSM(localPaymentListeners, cmd.fullTag)
    opm process ClearFailures
    opm process cmd
  }

  def localSendToSelf(sources: List[Channel], destinations: CommitsAndMax, preimage: ByteVector32, typicalChainTxFee: MilliSatoshi, capLNFeeToChain: Boolean): Unit = {
    val prExt = makePrExt(maxSendable(sources).min(destinations.maxReceivable), PlainDescription(split = None, label = None, invoiceText = new String), destinations.commits, Crypto sha256 preimage)
    val keySendCmd = makeSendCmd(prExt, prExt.pr.amount.get, sources, typicalChainTxFee, capLNFeeToChain).copy(userCustomTlvs = GenericTlv(OnionCodecs.keySendNumber, preimage) :: Nil)
    localSend(keySendCmd)
  }

  def checkIfSendable(paymentHash: ByteVector32): Option[Int] = {
    val isActive = opm.data.payments.values.exists(fsm => fsm.fullTag.tag == PaymentTagTlv.LOCALLY_SENT && fsm.fullTag.paymentHash == paymentHash)
    if (isActive) Some(PaymentInfo.NOT_SENDABLE_IN_FLIGHT) else if (getPreimageMemo.get(paymentHash).isSuccess) Some(PaymentInfo.NOT_SENDABLE_SUCCESS) else None
  }

  // These are executed in Channel context

  override def onException: PartialFunction[Malfunction, Unit] = {
    case (_: ChannelTransitionFail, chan: ChannelNormal, _: HasNormalCommitments) =>
      // Execute immediately in same thread to not let channel get updated
      chan doProcess CMD_CLOSE(scriptPubKey = None, force = true)

    case (_: ChannelTransitionFail, chan: ChannelHosted, hc: HostedCommits) =>
      // Execute immediately in same thread to not let channel get updated
      chan.localSuspend(hc, ErrorCodes.ERR_HOSTED_MANUAL_SUSPEND)
  }

  override def onBecome: PartialFunction[Transition, Unit] = {
    case (_, _, nextNc: DATA_NORMAL, _, _) if nextNc.localShutdown.nonEmpty => next(stateUpdateStream)
    case (_, _: DATA_NORMAL, _: DATA_NEGOTIATING, _, _) => next(stateUpdateStream)
    case (_, _, _, prev, CLOSING) if prev != CLOSING => next(stateUpdateStream)

    case (_, prevHc: HostedCommits, nextHc: HostedCommits, _, _)
      if prevHc.error.isEmpty && nextHc.error.nonEmpty =>
      // Previously operational HC got suspended
      next(stateUpdateStream)

    case (_, prevHc: HostedCommits, nextHc: HostedCommits, _, _)
      if prevHc.error.nonEmpty && nextHc.error.isEmpty =>
      // Previously suspended HC got operational
      opm process CMDChanGotOnline
      next(stateUpdateStream)

    case (_, _, _, prev, SLEEPING) if prev != SLEEPING =>
      // Channel which was not SLEEPING became SLEEPING
      next(statusUpdateStream)

    case (chan, _, _, prev, OPEN) if prev != OPEN =>
      // Channel which was not OPEN became operational and OPEN
      // We may get here after getting fresh feerates so check again
      chan process CMD_CHECK_FEERATE
      opm process CMDChanGotOnline
      next(statusUpdateStream)
  }

  // Used to notify about an existance of preimage BEFORE new state is committed in origin channel
  // should always be followed by real or simulated state update to let incoming FSMs finalize properly
  override def fulfillReceived(fulfill: RemoteFulfill): Unit = opm process fulfill

  // Used to notify about outgoing adds which can not be committed, or not committed any more
  // also contains invariants which instruct outgoing FSM to abort a payment right away
  override def addRejectedLocally(reason: LocalReject): Unit = opm process reason

  // Used to notify about outgoing adds which were failed by peer AFTER new state is committed in origin channel
  // this means it's safe to retry amounts from these failed payments, there will be no cross-signed duplicates
  override def addRejectedRemotely(reason: RemoteReject): Unit = opm process reason

  override def notifyResolvers: Unit = {
    // Used to notify FSMs that we have cross-signed incoming HTLCs which FSMs may somehow act upon
    val allIns = all.values.flatMap(Channel.chanAndCommitsOpt).flatMap(_.commits.crossSignedIncoming).map(initResolveMemo.get)
    allIns.foreach { case finalResolve: FinalResolution => sendTo(finalResolve, finalResolve.theirAdd.channelId) case _ => }
    val reasonableIncoming = allIns.collect { case resolution: ReasonableResolution => resolution }.groupBy(_.fullTag)
    val inFlightsBag = InFlightPayments(allInChannelOutgoing, reasonableIncoming)

    inFlightsBag.allTags.collect {
      case fullTag if PaymentTagTlv.TRAMPLOINE_ROUTED == fullTag.tag && !inProcessors.contains(fullTag) => inProcessors += new TrampolinePaymentRelayer(fullTag, me).tuple
      case fullTag if PaymentTagTlv.FINAL_INCOMING == fullTag.tag && !inProcessors.contains(fullTag) => inProcessors += new IncomingPaymentReceiver(fullTag, me).tuple
      case fullTag if PaymentTagTlv.LOCALLY_SENT == fullTag.tag => opm process CreateSenderFSM(localPaymentListeners, fullTag)
    }

    // FSM exists because there were related HTLCs, none may be left now
    // this change is used by existing FSMs to properly finalize themselves
    for (incomingFSM <- inProcessors.values) incomingFSM doProcess inFlightsBag
    // Sign all fails and fulfills that could have been sent from FSMs above
    for (chan <- all.values) chan process CMD_SIGN
    // Maybe remove successful outgoing FSMs
    opm process inFlightsBag
    next(stateUpdateStream)
  }

  // Mainly to prolong FSM timeouts once another add is seen (but not yet committed)
  override def addReceived(add: UpdateAddHtlcExt): Unit = initResolveMemo.getUnchecked(add) match {
    case resolution: ReasonableResolution => inProcessors.get(resolution.fullTag).foreach(_ doProcess resolution)
    case _ => // Do nothing, invalid add will be failed after it gets committed
  }
}
