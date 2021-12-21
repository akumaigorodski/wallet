package immortan.fsm

import fr.acinq.bitcoin.ByteVector32
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair._
import fr.acinq.eclair.channel.{CMD_FAIL_HTLC, CMD_FULFILL_HTLC, ReasonableLocal, ReasonableTrampoline}
import fr.acinq.eclair.payment.IncomingPaymentPacket
import fr.acinq.eclair.router.RouteCalculation
import fr.acinq.eclair.transactions.RemoteFulfill
import fr.acinq.eclair.wire._
import immortan.ChannelMaster.{ReasonableLocals, ReasonableTrampolines}
import immortan._
import immortan.crypto.Tools._
import immortan.crypto.{CanBeShutDown, StateMachine}
import immortan.fsm.IncomingPaymentProcessor._
import immortan.fsm.PaymentFailure.Failures

import scala.util.Success


object IncomingPaymentProcessor {
  final val CMDTimeout = "cmd-timeout"
  final val CMDReleaseHold = "cmd-release-hold"

  final val SHUTDOWN = 0
  final val FINALIZING = 1
  final val RECEIVING = 2
  final val SENDING = 3
}

sealed trait IncomingPaymentProcessor extends StateMachine[IncomingProcessorData] with CanBeShutDown { me =>
  lazy val tuple: (FullPaymentTag, IncomingPaymentProcessor) = (fullTag, me)
  var lastAmountIn: MilliSatoshi = MilliSatoshi(0L)
  var isHolding: Boolean = false
  val fullTag: FullPaymentTag
}

// LOCAL RECEIVER

sealed trait IncomingProcessorData { val fullTag: FullPaymentTag }

case class IncomingRevealed(preimage: ByteVector32, fullTag: FullPaymentTag) extends IncomingProcessorData
case class IncomingAborted(failure: Option[FailureMessage] = None, fullTag: FullPaymentTag) extends IncomingProcessorData

class IncomingPaymentReceiver(val fullTag: FullPaymentTag, cm: ChannelMaster) extends IncomingPaymentProcessor {
  private val paymentInfoOpt = cm.payBag.getPaymentInfo(fullTag.paymentHash).toOption
  override def becomeShutDown: Unit = become(null, SHUTDOWN)

  require(fullTag.tag == PaymentTagTlv.FINAL_INCOMING)
  delayedCMDWorker.replaceWork(CMDTimeout)
  become(null, RECEIVING)

  def doProcess(msg: Any): Unit = (msg, data, state) match {
    case (inFlight: InFlightPayments, _, RECEIVING | FINALIZING) if !inFlight.in.contains(fullTag) =>
      // We have previously failed or fulfilled an incoming payment and all parts have been cleared
      cm.finalizeIncoming(data)
      becomeShutDown

    case (inFlight: InFlightPayments, null, RECEIVING) =>
      val adds = inFlight.in(fullTag).asInstanceOf[ReasonableLocals]
      // Important: when creating new invoice we SPECIFICALLY DO NOT put a preimage into preimage storage
      // we only do that once we reveal a preimage, thus letting us know that we have already revealed it on restart
      // having PaymentStatus.SUCCEEDED in payment db is not enough because that table does not get included in backup
      lastAmountIn = adds.foldLeft(0L.msat)(_ + _.add.amountMsat)

      paymentInfoOpt match {
        case None => cm.getPreimageMemo.get(fullTag.paymentHash).toOption match {
          case Some(preimage) => becomeRevealed(preimage, fullTag.paymentHash.toHex, adds) // Did not ask but fulfill anyway
          case None => becomeAborted(IncomingAborted(None, fullTag), adds) // Did not ask and there is no preimage
        }

        case Some(info) => info.description.toSelfPreimage match {
          case _ if info.isIncoming && PaymentStatus.SUCCEEDED == info.status => becomeRevealed(info.preimage, info.description.queryText, adds) // Already revealed, but not finalized
          case Some(selfPreimage) if !info.isIncoming && PaymentStatus.SUCCEEDED == info.status => becomeRevealed(selfPreimage, info.description.queryText, adds) // Already revealed, but not finalized
          case _ if adds.exists(_.add.cltvExpiry.underlying < LNParams.blockCount.get + LNParams.cltvRejectThreshold) => becomeAborted(IncomingAborted(None, fullTag), adds) // Not enough time to react if stalls

          case _ if info.isIncoming && info.prExt.isEnough(lastAmountIn) => info.description.holdPeriodSec match {
            case None => becomeRevealed(info.preimage, info.description.queryText, adds)
            case Some(holdPeriodSeconds) if !isHolding => hold(holdPeriodSeconds)
            case _ => // Do nothing, await for timeout or user action
          }

          case Some(selfPreimage) if !info.isIncoming &&info.prExt.isEnough(lastAmountIn) => becomeRevealed(selfPreimage, info.description.queryText, adds) // Got all self-parts
          case _ if adds.size >= cm.all.values.count(Channel.isOperational) * LNParams.maxInChannelHtlcs => becomeAborted(IncomingAborted(None, fullTag), adds) // Ran out of slots
          case None if !info.isIncoming => becomeAborted(IncomingAborted(None, fullTag), adds) // Not an explicit outgoing self-payment
          case _ => // Do nothing, wait for more parts or a timeout
        }
      }

    case (_: ReasonableLocal, null, RECEIVING) =>
      // Just saw another related add so prolong timeout
      delayedCMDWorker.replaceWork(CMDTimeout)

    case (CMDTimeout, null, RECEIVING) =>
      // User is explicitly requesting failing of held payment
      // Or too much time has elapsed since we seen another incoming part
      becomeAborted(IncomingAborted(PaymentTimeout.asSome, fullTag), adds = Nil)
      // Actually request adds
      cm.notifyResolvers

    case (CMDReleaseHold, null, RECEIVING) =>
      paymentInfoOpt.filter(_ => isHolding) match {
        case Some(info) => becomeRevealed(info.preimage, info.description.queryText, adds = Nil)
        case None => becomeAborted(IncomingAborted(PaymentTimeout.asSome, fullTag), adds = Nil)
      }

      // Actually request adds
      cm.notifyResolvers

    case (inFlight: InFlightPayments, revealed: IncomingRevealed, FINALIZING) =>
      val adds = inFlight.in(fullTag).asInstanceOf[ReasonableLocals]
      fulfill(revealed.preimage, adds)

    case (inFlight: InFlightPayments, aborted: IncomingAborted, FINALIZING) =>
      val adds = inFlight.in(fullTag).asInstanceOf[ReasonableLocals]
      abort(aborted, adds)

    case _ =>
  }

  def fulfill(preimage: ByteVector32, adds: ReasonableLocals): Unit =
    for (local <- adds) cm.sendTo(CMD_FULFILL_HTLC(preimage, local.add), local.add.channelId)

  def abort(data1: IncomingAborted, adds: ReasonableLocals): Unit = data1.failure match {
    case None => for (local <- adds) cm.sendTo(CMD_FAIL_HTLC(IncorrectOrUnknownPaymentDetails(local.add.amountMsat, LNParams.blockCount.get).asRight, local.secret, local.add), local.add.channelId)
    case Some(specificLocalFail) => for (local <- adds) cm.sendTo(CMD_FAIL_HTLC(specificLocalFail.asRight, local.secret, local.add), local.add.channelId)
  }

  def hold(holdPeriodSeconds: Long): Unit = {
    // Extend timer to hold timeout instead of revealing
    // actual revealing or failing will happen on user request
    TOTAL_INTERVAL_SECONDS = holdPeriodSeconds
    delayedCMDWorker.replaceWork(CMDTimeout)
    isHolding = true
  }

  def becomeAborted(data1: IncomingAborted, adds: ReasonableLocals): Unit = {
    // Fail parts and retain a failure message to maybe re-fail using the same error
    become(data1, FINALIZING)
    abort(data1, adds)
  }

  def becomeRevealed(preimage: ByteVector32, queryText: String, adds: ReasonableLocals): Unit = cm.chanBag.db txWrap {
    // With final payment we ALREADY know a preimage, but also put it into storage once preimage gets revealed
    // doing so makes it transferrable and fulfillable on a new device as storage db gets included in backup
    cm.payBag.addSearchablePayment(queryText, fullTag.paymentHash)
    cm.payBag.updOkIncoming(lastAmountIn, fullTag.paymentHash)
    cm.payBag.setPreimage(fullTag.paymentHash, preimage)
    cm.getPreimageMemo.invalidate(fullTag.paymentHash)

    // First record things in db, then send out fulfill commands
    become(IncomingRevealed(preimage, fullTag), FINALIZING)
    fulfill(preimage, adds)
  }
}

// TRAMPOLINE RELAYER

case class TrampolineStopping(retry: Boolean, fullTag: FullPaymentTag) extends IncomingProcessorData // SENDING
case class TrampolineProcessing(finalNodeId: PublicKey, fullTag: FullPaymentTag) extends IncomingProcessorData // SENDING
case class TrampolineRevealed(preimage: ByteVector32, senderData: Option[OutgoingPaymentSenderData], fullTag: FullPaymentTag) extends IncomingProcessorData // SENDING | FINALIZING
case class TrampolineAborted(failure: FailureMessage, fullTag: FullPaymentTag) extends IncomingProcessorData // FINALIZING

class TrampolinePaymentRelayer(val fullTag: FullPaymentTag, cm: ChannelMaster) extends IncomingPaymentProcessor with OutgoingPaymentListener { self =>
  // Important: we may have outgoing leftovers on restart, so we always need to create a sender FSM right away, which will be firing events once leftovers get finalized
  override def gotFirstPreimage(data: OutgoingPaymentSenderData, fulfill: RemoteFulfill): Unit = self doProcess TrampolineRevealed(fulfill.theirPreimage, data.asSome, fullTag)
  override def wholePaymentFailed(data: OutgoingPaymentSenderData): Unit = self doProcess data

  def collectedEnough(adds: ReasonableTrampolines): Boolean = firstOpt(adds).exists(lastAmountIn >= _.outerPayload.totalAmount)
  def firstOpt(adds: ReasonableTrampolines): Option[IncomingPaymentPacket.NodeRelayPacket] = adds.headOption.map(_.packet)
  def expiryIn(adds: ReasonableTrampolines): CltvExpiry = adds.map(_.add.cltvExpiry).minBy(_.underlying)

  def validateRelay(params: TrampolineOn, adds: ReasonableTrampolines, blockHeight: Long): Option[FailureMessage] = firstOpt(adds) collectFirst {
    case pkt if pkt.innerPayload.invoiceFeatures.isDefined && pkt.innerPayload.paymentSecret.isEmpty => InvalidOnionPayload(UInt64(8), 0) // We do not serve legacy recepients
    case pkt if params.relayFee(pkt.innerPayload.amountToForward) > lastAmountIn - pkt.innerPayload.amountToForward => TrampolineFeeInsufficient // Proposed trampoline fee is less than required
    case pkt if adds.map(_.packet.innerPayload.amountToForward).toSet.size != 1 => IncorrectOrUnknownPaymentDetails(pkt.add.amountMsat, LNParams.blockCount.get) // amountToForward divergence
    case pkt if adds.map(_.packet.outerPayload.totalAmount).toSet.size != 1 => IncorrectOrUnknownPaymentDetails(pkt.add.amountMsat, LNParams.blockCount.get) // totalAmount divergence
    case pkt if params.cltvExpiryDelta > expiryIn(adds) - pkt.innerPayload.outgoingCltv => TrampolineExpiryTooSoon // Proposed delta is less than required by our node
    case _ if !adds.map(_.add.channelId).flatMap(cm.all.get).forall(Channel.isOperational) => TemporaryNodeFailure // Some channels got into error state, better stop
    case pkt if CltvExpiry(blockHeight) >= pkt.innerPayload.outgoingCltv => TrampolineExpiryTooSoon // Final recepient's CLTV expiry is below current chain height
    case pkt if pkt.innerPayload.amountToForward < params.minMsat => TemporaryNodeFailure // Peer wants to route less than a minimum we have told them about
  }

  def abortedWithError(failures: Failures, finalNodeId: PublicKey): TrampolineAborted = {
    val finalNodeFailure = failures.collectFirst { case remote: RemoteFailure if remote.packet.originNode == finalNodeId => remote.packet.failureMessage }
    val routingNodeFailure = failures.collectFirst { case remote: RemoteFailure if remote.packet.originNode != finalNodeId => remote.packet.failureMessage }
    val localNoRoutesFoundError = failures.collectFirst { case local: LocalFailure if local.status == PaymentFailure.NO_ROUTES_FOUND => TrampolineFeeInsufficient }
    TrampolineAborted(finalNodeFailure orElse routingNodeFailure orElse localNoRoutesFoundError getOrElse TemporaryNodeFailure, fullTag)
  }

  require(fullTag.tag == PaymentTagTlv.TRAMPLOINE_ROUTED)
  cm.opm process CreateSenderFSM(Set(self), fullTag)
  delayedCMDWorker.replaceWork(CMDTimeout)
  become(null, RECEIVING)

  def doProcess(msg: Any): Unit = (msg, data, state) match {
    case (inFlight: InFlightPayments, _, RECEIVING | SENDING | FINALIZING) if !inFlight.allTags.contains(fullTag) =>
      // This happens AFTER we have resolved all outgoing payments and started resolving related incoming payments
      cm.finalizeIncoming(data)
      becomeShutDown

    case (inFlight: InFlightPayments, TrampolineRevealed(preimage, senderData, _), SENDING) =>
      // A special case after we have just received a first preimage and can become revealed
      val ins = inFlight.in.getOrElse(fullTag, Nil).asInstanceOf[ReasonableTrampolines]
      becomeFinalRevealed(preimage, ins)

      firstOpt(ins).foreach { packet =>
        // First, we may not have incoming HTLCs at all in pathological states
        val reserve = packet.outerPayload.totalAmount - packet.innerPayload.amountToForward
        val actualEarnings = senderData.filter(_.inFlightParts.nonEmpty).map(reserve - _.usedFee)
        // Second, used fee in sender data may be incorrect after restart, use fallback in that case
        val finalEarnings = actualEarnings getOrElse LNParams.trampoline.relayFee(packet.innerPayload.amountToForward)
        cm.payBag.addRelayedPreimageInfo(fullTag, preimage, packet.innerPayload.amountToForward, finalEarnings)
      }

    case (revealed: TrampolineRevealed, _: TrampolineAborted, FINALIZING) =>
      // We were winding a relay down but suddenly got a preimage
      becomeInitRevealed(revealed)

    case (revealed: TrampolineRevealed, _, RECEIVING | SENDING) =>
      // This specifically omits (TrampolineRevealed x FINALIZING) state
      // We have outgoing in-flight payments and just got a preimage
      becomeInitRevealed(revealed)

    case (_: OutgoingPaymentSenderData, TrampolineStopping(true, _), SENDING) =>
      // We were waiting for all outgoing parts to fail on app restart, try again
      // Note that senderFSM has removed itself on first failure, so we create it again
      become(null, RECEIVING)
      cm.notifyResolvers

    case (data: OutgoingPaymentSenderData, TrampolineStopping(false, _), SENDING) =>
      // We were waiting for all outgoing parts to fail on app restart, fail incoming
      become(abortedWithError(data.failures, invalidPubKey), FINALIZING)
      cm.notifyResolvers

    case (data: OutgoingPaymentSenderData, processing: TrampolineProcessing, SENDING) =>
      // This was a normal operation where we were trying to deliver a payment to recipient
      become(abortedWithError(data.failures, processing.finalNodeId), FINALIZING)
      cm.notifyResolvers

    case (inFlight: InFlightPayments, null, RECEIVING) =>
      val outs = inFlight.out.getOrElse(fullTag, default = Nil)
      val ins = inFlight.in.getOrElse(fullTag, default = Nil).asInstanceOf[ReasonableTrampolines]
      // We have either just got another incoming notification or restored an app with some parts present
      lastAmountIn = ins.foldLeft(0L.msat)(_ + _.add.amountMsat)

      cm.getPreimageMemo.get(fullTag.paymentHash) match {
        case Success(preimage) => becomeFinalRevealed(preimage, ins)
        case _ if outs.isEmpty && collectedEnough(ins) => becomeSendingOrAborted(ins) // We have collected enough incoming parts: start sending
        case _ if outs.nonEmpty && collectedEnough(ins) => become(TrampolineStopping(retry = true, fullTag), SENDING) // Probably a restart: fail and retry afterwards
        case _ if outs.nonEmpty => become(TrampolineStopping(retry = false, fullTag), SENDING) // Have not collected enough incoming, yet have outgoing: fail without retry
        case _ => // Do nothing, wait for more parts with a timeout
      }

    case (_: ReasonableTrampoline, null, RECEIVING) =>
      // Just saw another related add so prolong timeout
      delayedCMDWorker.replaceWork(CMDTimeout)

    case (CMDTimeout, null, RECEIVING) =>
      // Sender must not have outgoing payments in this state
      become(TrampolineAborted(PaymentTimeout, fullTag), FINALIZING)
      cm.notifyResolvers

    case (inFlight: InFlightPayments, revealed: TrampolineRevealed, FINALIZING) =>
      val ins = inFlight.in.getOrElse(fullTag, Nil).asInstanceOf[ReasonableTrampolines]
      fulfill(revealed.preimage, ins)

    case (inFlight: InFlightPayments, aborted: TrampolineAborted, FINALIZING) =>
      val ins = inFlight.in.getOrElse(fullTag, Nil).asInstanceOf[ReasonableTrampolines]
      abort(aborted, ins)

    case _ =>
  }

  def fulfill(preimage: ByteVector32, adds: ReasonableTrampolines): Unit =
    for (local <- adds) cm.sendTo(CMD_FULFILL_HTLC(preimage, local.add), local.add.channelId)

  def abort(data1: TrampolineAborted, adds: ReasonableTrampolines): Unit =
    for (local <- adds) cm.sendTo(CMD_FAIL_HTLC(data1.failure.asRight, local.secret, local.add), local.add.channelId)

  def becomeSendingOrAborted(adds: ReasonableTrampolines): Unit = {
    require(adds.nonEmpty, "A set of incoming HTLCs must be non-empty here")
    val result = validateRelay(LNParams.trampoline, adds, LNParams.blockCount.get)

    result match {
      case Some(failure) =>
        val data1 = TrampolineAborted(failure, fullTag)
        become(data1, freshState = FINALIZING)
        abort(data1, adds)

      case None =>
        val inner = firstOpt(adds).get.innerPayload
        val extraEdges = RouteCalculation.makeExtraEdges(inner.invoiceRoutingInfo.getOrElse(Nil), inner.outgoingNodeId)
        val totalFeeReserve = lastAmountIn - inner.amountToForward - LNParams.trampoline.relayFee(inner.amountToForward)
        val routerConf = LNParams.routerConf.copy(routeMaxCltv = expiryIn(adds) - inner.outgoingCltv - LNParams.ourRoutingCltvExpiryDelta)

        val splitInfo = SplitInfo(inner.amountToForward, inner.amountToForward)
        // It makes no sense to try to route out a payment through channels used by peer to route it in, this also includes possible unused multiple channels from same peer
        val allowedChans = cm.all -- adds.map(_.add.channelId).flatMap(cm.all.get).flatMap(Channel.chanAndCommitsOpt).map(_.commits.remoteInfo.nodeId).flatMap(cm.allFromNode).map(_.commits.channelId)
        val send = SendMultiPart(fullTag, chainExpiry = Left(inner.outgoingCltv), splitInfo, routerConf, inner.outgoingNodeId, expectedRouteFees = None, prExt = None, totalFeeReserve, allowedChans.values.toSeq)

        become(TrampolineProcessing(inner.outgoingNodeId, fullTag), SENDING)
        // If invoice features are present then sender is asking for non-trampoline relay, it's known that recipient supports MPP
        if (inner.invoiceFeatures.isDefined) cm.opm process send.copy(assistedEdges = extraEdges, outerPaymentSecret = inner.paymentSecret.get)
        else cm.opm process send.copy(onionTlvs = OnionPaymentPayloadTlv.TrampolineOnion(adds.head.packet.nextPacket) :: Nil, outerPaymentSecret = randomBytes32)
    }
  }

  def becomeInitRevealed(revealed: TrampolineRevealed): Unit = {
    // First, unconditionally persist a preimage before doing anything else
    cm.payBag.setPreimage(fullTag.paymentHash, revealed.preimage)
    cm.getPreimageMemo.invalidate(fullTag.paymentHash)
    // Await for subsequent incoming leftovers
    become(revealed, SENDING)
    cm.notifyResolvers
  }

  def becomeFinalRevealed(preimage: ByteVector32, adds: ReasonableTrampolines): Unit = {
    // We might not have enough OR no incoming payments at all in pathological states
    become(TrampolineRevealed(preimage, senderData = None, fullTag), FINALIZING)
    fulfill(preimage, adds)
  }

  override def becomeShutDown: Unit = {
    cm.opm process RemoveSenderFSM(fullTag)
    become(null, SHUTDOWN)
  }
}
