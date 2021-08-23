package immortan.fsm

import immortan._
import fr.acinq.eclair._
import fr.acinq.eclair.wire._
import immortan.crypto.Tools._
import immortan.PaymentStatus._
import immortan.fsm.PaymentFailure._
import fr.acinq.eclair.router.Router._
import immortan.fsm.OutgoingPaymentMaster._
import immortan.crypto.{CanBeRepliedTo, StateMachine}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.eclair.router.{Announcements, ChannelUpdateExt}
import fr.acinq.eclair.router.Graph.GraphStructure.{DescAndCapacity, GraphEdge}
import fr.acinq.eclair.channel.{CMD_ADD_HTLC, ChannelOffline, InPrincipleNotSendable, LocalReject}
import fr.acinq.eclair.transactions.{RemoteFulfill, RemoteReject, RemoteUpdateFail, RemoteUpdateMalform}
import fr.acinq.eclair.crypto.Sphinx.PacketAndSecrets
import fr.acinq.eclair.payment.OutgoingPacket
import fr.acinq.bitcoin.ByteVector32
import fr.acinq.eclair.crypto.Sphinx
import scala.util.Random.shuffle
import scala.collection.mutable
import scodec.bits.ByteVector


object PaymentFailure {
  type Failures = List[PaymentFailure]
  final val NO_ROUTES_FOUND = "no-routes-found"
  final val NOT_ENOUGH_FUNDS = "not-enough-funds"
  final val PAYMENT_NOT_SENDABLE = "payment-not-sendable"
  final val RUN_OUT_OF_RETRY_ATTEMPTS = "run-out-of-retry-attempts"
  final val RUN_OUT_OF_CAPABLE_CHANNELS = "run-out-of-capable-channels"
  final val PEER_COULD_NOT_PARSE_ONION = "peer-could-not-parse-onion"
  final val NOT_RETRYING_NO_DETAILS = "not-retrying-no-details"
  final val TIMED_OUT = "timed-out"
}

sealed trait PaymentFailure {
  def asString: String
}

case class LocalFailure(status: String, amount: MilliSatoshi) extends PaymentFailure {
  override def asString: String = s"- Local failure: $status"
}

case class UnreadableRemoteFailure(route: Route) extends PaymentFailure {
  override def asString: String = s"- Remote failure at unknown channel: ${route.asString}"
}

case class RemoteFailure(packet: Sphinx.DecryptedFailurePacket, route: Route) extends PaymentFailure {
  def originChannelId: String = route.getEdgeForNode(packet.originNode).map(_.updExt.update.shortChannelId.toString).getOrElse("unknown channel")
  override def asString: String = s"- ${packet.failureMessage.message} at $originChannelId: ${route.asString}"
}

// Master commands and data

case object ClearFailures
case class CutIntoHalves(amount: MilliSatoshi)
case class RemoveSenderFSM(fullTag: FullPaymentTag)
case class CreateSenderFSM(listeners: Iterable[OutgoingPaymentListener], fullTag: FullPaymentTag)

case class NodeFailed(failedNodeId: PublicKey, increment: Int)
case class ChannelFailed(failedDescAndCap: DescAndCapacity, increment: Int)
case class StampedChannelFailed(amount: MilliSatoshi, stamp: Long)

case class SplitInfo(totalSum: MilliSatoshi, myPart: MilliSatoshi) {
  val sentRatio: Long = ratio(totalSum, myPart)
}

// For locally initiated payments outerPaymentSecret and fullTag.paymentSecret are same
// For trampoline-routed payments fullTag.paymentSecret is taken from upstream incoming payment
case class SendMultiPart(fullTag: FullPaymentTag, chainExpiry: Either[CltvExpiry, CltvExpiryDelta], split: SplitInfo, routerConf: RouterConf, targetNodeId: PublicKey,
                         totalFeeReserve: MilliSatoshi = MilliSatoshi(0L), allowedChans: Seq[Channel] = Nil, outerPaymentSecret: ByteVector32 = ByteVector32.Zeroes,
                         assistedEdges: Set[GraphEdge] = Set.empty, onionTlvs: Seq[OnionTlv] = Nil, userCustomTlvs: Seq[GenericTlv] = Nil)

case class OutgoingPaymentMasterData(payments: Map[FullPaymentTag, OutgoingPaymentSender],
                                     chanFailedAtAmount: Map[DescAndCapacity, StampedChannelFailed] = Map.empty,
                                     nodeFailedWithUnknownUpdateTimes: Map[PublicKey, Int] = Map.empty,
                                     chanFailedTimes: Map[ChannelDesc, Int] = Map.empty) {

  def withFailuresReduced(stampInFuture: Long): OutgoingPaymentMasterData = {
    val node1 = nodeFailedWithUnknownUpdateTimes.mapValues(_ / 2)
    val chan1 = chanFailedTimes.mapValues(_ / 2)

    val acc = Map.empty[DescAndCapacity, StampedChannelFailed]
    val amount1 = chanFailedAtAmount.foldLeft(acc) { case (acc1, dac ~ failed) =>
      val ratio: Double = (stampInFuture - failed.stamp) / LNParams.failedChanRecoveryMsec
      val failed1 = failed.copy(amount = failed.amount + (dac.capacity - failed.amount) * ratio)
      if (failed1.amount >= dac.capacity) acc1 else acc1.updated(dac, failed1)
    }

    // Reduce failure times to give previously failing channels a chance, failed-at-amount is restored gradually
    copy(nodeFailedWithUnknownUpdateTimes = node1, chanFailedTimes = chan1, chanFailedAtAmount = amount1)
  }
}

// All current outgoing in-flight payments

object OutgoingPaymentMaster {
  type PartIdToAmount = Map[ByteVector, MilliSatoshi]
  final val CMDChanGotOnline = "cmd-chan-got-online"
  final val CMDAskForRoute = "cmd-ask-for-route"
  final val CMDAbort = "cmd-abort"

  final val EXPECTING_PAYMENTS = 0
  final val WAITING_FOR_ROUTE = 1
}

class OutgoingPaymentMaster(val cm: ChannelMaster) extends StateMachine[OutgoingPaymentMasterData] with CanBeRepliedTo { me =>
  def process(change: Any): Unit = scala.concurrent.Future(me doProcess change)(Channel.channelContext)
  become(OutgoingPaymentMasterData(Map.empty), EXPECTING_PAYMENTS)

  def doProcess(change: Any): Unit = (change, state) match {
    case (ClearFailures, EXPECTING_PAYMENTS | WAITING_FOR_ROUTE) =>
      become(data.withFailuresReduced(System.currentTimeMillis), state)

    case (send: SendMultiPart, EXPECTING_PAYMENTS | WAITING_FOR_ROUTE) =>
      for (edge <- send.assistedEdges) cm.pf process edge
      data.payments(send.fullTag) doProcess send
      me process CMDAskForRoute

    case (CMDChanGotOnline, EXPECTING_PAYMENTS | WAITING_FOR_ROUTE) =>
      // Payments may still have awaiting parts due to offline channels
      data.payments.values.foreach(_ doProcess CMDChanGotOnline)
      me process CMDAskForRoute

    case (PathFinder.NotifyOperational, EXPECTING_PAYMENTS) =>
      me process CMDAskForRoute

    case (CMDAskForRoute, EXPECTING_PAYMENTS) =>
      // This is a proxy to always send command in payment master thread
      // IMPLICIT GUARD: this message is ignored in all other states
      data.payments.values.foreach(_ doProcess CMDAskForRoute)

    case (req: RouteRequest, EXPECTING_PAYMENTS) =>
      // IMPLICIT GUARD: this message is ignored in all other states
      val currentUsedCapacities: mutable.Map[DescAndCapacity, MilliSatoshi] = usedCapacities
      val currentUsedDescs = mapKeys[DescAndCapacity, MilliSatoshi, ChannelDesc](currentUsedCapacities, _.desc, defVal = 0L.msat)
      val ignoreChansFailedTimes = data.chanFailedTimes.collect { case (desc, times) if times >= LNParams.routerConf.maxChannelFailures => desc }
      val ignoreChansCanNotHandle = currentUsedCapacities.collect { case (dac, used) if used + req.amount >= dac.capacity - req.amount / 32 => dac.desc }
      val ignoreChansFailedAtAmount = data.chanFailedAtAmount.collect { case (dac, failedAt) if failedAt.amount - currentUsedDescs(dac.desc) - req.amount / 8 <= req.amount => dac.desc }
      val ignoreNodes = data.nodeFailedWithUnknownUpdateTimes.collect { case (nodeId, failTimes) if failTimes >= LNParams.routerConf.maxStrangeNodeFailures => nodeId }
      val req1 = req.copy(ignoreNodes = ignoreNodes.toSet, ignoreChannels = ignoreChansFailedTimes.toSet ++ ignoreChansCanNotHandle ++ ignoreChansFailedAtAmount)
      // Note: we may get many route request messages from payment FSMs with parts waiting for routes
      // so it is important to immediately switch to WAITING_FOR_ROUTE after seeing a first message
      cm.pf process PathFinder.FindRoute(me, req1)
      become(data, WAITING_FOR_ROUTE)

    case (PathFinder.NotifyRejected, WAITING_FOR_ROUTE) =>
      // Pathfinder is not yet ready, switch local state back
      // pathfinder is expected to notify us once it gets ready
      become(data, EXPECTING_PAYMENTS)

    case (response: RouteResponse, EXPECTING_PAYMENTS | WAITING_FOR_ROUTE) =>
      data.payments.get(response.fullTag).foreach(_ doProcess response)
      // Switch state to allow new route requests to come through
      become(data, EXPECTING_PAYMENTS)
      me process CMDAskForRoute

    case (ChannelFailed(descAndCapacity, increment), EXPECTING_PAYMENTS | WAITING_FOR_ROUTE) =>
      // At this point an affected InFlight status IS STILL PRESENT so failedAtAmount1 = usedCapacities = sum(inFlight)
      val amount1 = data.chanFailedAtAmount.get(descAndCapacity).map(_.amount).getOrElse(Long.MaxValue.msat) min usedCapacities(descAndCapacity)
      val times1 = data.chanFailedTimes.updated(descAndCapacity.desc, data.chanFailedTimes.getOrElse(descAndCapacity.desc, 0) + increment)
      val stampedChannelFailed = StampedChannelFailed(amount1, stamp = System.currentTimeMillis)
      val fail1 = data.chanFailedAtAmount.updated(descAndCapacity, stampedChannelFailed)
      become(data.copy(chanFailedAtAmount = fail1, chanFailedTimes = times1), state)

    case (NodeFailed(nodeId, increment), EXPECTING_PAYMENTS | WAITING_FOR_ROUTE) =>
      val newNodeFailedTimes = data.nodeFailedWithUnknownUpdateTimes.getOrElse(nodeId, 0) + increment
      val atTimes1 = data.nodeFailedWithUnknownUpdateTimes.updated(nodeId, newNodeFailedTimes)
      become(data.copy(nodeFailedWithUnknownUpdateTimes = atTimes1), state)

    case (bag: InFlightPayments, EXPECTING_PAYMENTS | WAITING_FOR_ROUTE) =>
      // We need this to issue "wholePaymentSucceeded" AFTER neither in-flight parts nor leftovers in channels are present
      // because FIRST peer sends a preimage (removing in-flight in FSM), THEN peer sends a state update (clearing channel leftovers)
      data.payments.values.foreach(_ doProcess bag)

    case (RemoveSenderFSM(fullTag), EXPECTING_PAYMENTS | WAITING_FOR_ROUTE) if data.payments.contains(fullTag) =>
      // First we get their fail, then stateUpdateStream fires, then we fire it here again if FSM is to be removed
      become(data.copy(payments = data.payments - fullTag), state)
      ChannelMaster.next(ChannelMaster.stateUpdateStream)

    case (CreateSenderFSM(listeners, fullTag), EXPECTING_PAYMENTS | WAITING_FOR_ROUTE) if !data.payments.contains(fullTag) =>
      val data1 = data.payments.updated(value = new OutgoingPaymentSender(fullTag, listeners, me), key = fullTag)
      become(data.copy(payments = data1), state)

    // Following messages expect that target FSM is always present
    // this won't be the case with failed/fulfilled leftovers in channels on app restart
    // so it has to be made sure that all relevalnt FSMs are manually re-initialized on startup

    case (reject: LocalReject, EXPECTING_PAYMENTS | WAITING_FOR_ROUTE) =>
      data.payments.get(reject.localAdd.fullTag).foreach(_ doProcess reject)
      me process CMDAskForRoute

    case (fulfill: RemoteFulfill, EXPECTING_PAYMENTS | WAITING_FOR_ROUTE) =>
      // We may have local and multiple routed outgoing payment sets at once, all of them must be notified
      data.payments.filterKeys(_.paymentHash == fulfill.ourAdd.paymentHash).values.foreach(_ doProcess fulfill)
      me process CMDAskForRoute

    case (remoteReject: RemoteReject, EXPECTING_PAYMENTS | WAITING_FOR_ROUTE) =>
      data.payments.get(remoteReject.ourAdd.fullTag).foreach(_ doProcess remoteReject)
      me process CMDAskForRoute

    case _ =>
  }

  def rightNowSendable(chans: Iterable[Channel], maxFee: MilliSatoshi): mutable.Map[ChanAndCommits, MilliSatoshi] = {
    // This method is supposed to be used to find channels which are able to currently handle a given amount + fee
    // note that it is possible for remaining fee to be disproportionally large relative to payment amount
    getSendable(chans.filter(Channel.isOperationalAndOpen), maxFee)
  }

  // What can be sent through given channels with yet unprocessed parts taken into account
  def getSendable(chans: Iterable[Channel], maxFee: MilliSatoshi): mutable.Map[ChanAndCommits, MilliSatoshi] = {
    // Example 1: chan toLocal=100, 10 in-flight AND IS NOT YET preset in channel yet, resulting sendable = 100 (toLocal) - 10 (in-flight - nothing) = 90
    // Example 2: chan toLocal=100, 10 in-flight AND IS present in channel already, resulting sendable = 90 (toLocal with in-flight) - 0 (in-flight - partId) = 90
    val waitParts = mutable.Map.empty[ByteVector32, PartIdToAmount] withDefaultValue Map.empty
    val finals = mutable.Map.empty[ChanAndCommits, MilliSatoshi] withDefaultValue 0L.msat

    // Wait part may have no route yet (but we expect a route to arrive) or it could be sent to channel but not processed by channel yet
    def waitPartsNotYetInChannel(cnc: ChanAndCommits): PartIdToAmount = waitParts(cnc.commits.channelId) -- cnc.commits.allOutgoing.map(_.partId)
    data.payments.values.flatMap(_.data.parts.values).collect { case wait: WaitForRouteOrInFlight => waitParts(wait.cnc.commits.channelId) += wait.partId -> wait.amount }
    chans.flatMap(Channel.chanAndCommitsOpt).foreach(cnc => finals(cnc) = cnc.commits.maxSendInFlight.min(cnc.commits.availableForSend) - maxFee - waitPartsNotYetInChannel(cnc).values.sum)
    finals.filter { case (cnc, sendable) => sendable >= cnc.commits.minSendable }
  }

  def usedCapacities: mutable.Map[DescAndCapacity, MilliSatoshi] = {
    // This gets supposedly used capacities of external channels in a routing graph
    // we need this to exclude channels which definitely can't route a given amount right now
    val accumulator = mutable.Map.empty[DescAndCapacity, MilliSatoshi] withDefaultValue 0L.msat
    // This may not always be accurate since after restart FSMs will be empty while leftovers may still be in chans
    val descsAndCaps = data.payments.values.flatMap(_.data.inFlightParts).flatMap(_.route.routedPerChannelHop)
    descsAndCaps.foreach { case (amount, chanHop) => accumulator(chanHop.edge.toDescAndCapacity) += amount }
    accumulator
  }
}

// Individual outgoing part status

sealed trait PartStatus { me =>
  final val partId: ByteVector = onionKey.publicKey.value
  def tuple: (ByteVector, PartStatus) = (partId, me)
  def onionKey: PrivateKey
}

case class InFlightInfo(cmd: CMD_ADD_HTLC, route: Route)
case class WaitForChanOnline(onionKey: PrivateKey, amount: MilliSatoshi) extends PartStatus
case class WaitForRouteOrInFlight(onionKey: PrivateKey, amount: MilliSatoshi, cnc: ChanAndCommits,
                                  flight: Option[InFlightInfo] = None, localFailed: List[Channel] = Nil,
                                  remoteAttempts: Int = 0) extends PartStatus {

  def oneMoreRemoteAttempt(cnc1: ChanAndCommits): WaitForRouteOrInFlight = copy(flight = None, remoteAttempts = remoteAttempts + 1, cnc = cnc1)
  def oneMoreLocalAttempt(cnc1: ChanAndCommits): WaitForRouteOrInFlight = copy(flight = None, localFailed = localFailedChans, cnc = cnc1)
  lazy val localFailedChans: List[Channel] = cnc.chan :: localFailed
}

// Individual outgoing payment status

case class OutgoingPaymentSenderData(cmd: SendMultiPart, parts: Map[ByteVector, PartStatus], failures: Failures = Nil) {
  def withLocalFailure(reason: String, amount: MilliSatoshi): OutgoingPaymentSenderData = copy(failures = LocalFailure(reason, amount) +: failures)
  def withoutPartId(failedPartId: ByteVector): OutgoingPaymentSenderData = copy(parts = parts - failedPartId)
  def usedRoutesAsString: String = inFlightParts.map(_.route.asString).mkString("\n\n")

  def failuresAsString: String = {
    val byAmount: Map[MilliSatoshi, Failures] = failures.groupBy {
      case fail: UnreadableRemoteFailure => fail.route.weight.costs.head
      case fail: RemoteFailure => fail.route.weight.costs.head
      case fail: LocalFailure => fail.amount
    }

    def translateFails(failureList: Failures): String = failureList.map(_.asString).mkString("\n\n")
    val bySortedAmount = byAmount.mapValues(translateFails).toSeq.sortBy { case (amount, _) => -amount }
    bySortedAmount.map { case (amount, fails) => s"» $amount:\n\n$fails" }.mkString("\n\n")
  }

  lazy val inFlightParts: Iterable[InFlightInfo] = parts.values.flatMap { case wait: WaitForRouteOrInFlight => wait.flight case _ => None }
  lazy val successfulUpdates: Iterable[ChannelUpdateExt] = inFlightParts.flatMap(_.route.routedPerChannelHop).toMap.values.map(_.edge.updExt)
  lazy val usedFee: MilliSatoshi = inFlightParts.map(_.route.fee).sum
}

trait OutgoingPaymentListener {
  // With local failures this will be the only way to know
  def wholePaymentFailed(data: OutgoingPaymentSenderData): Unit = none
  def wholePaymentSucceeded(data: OutgoingPaymentSenderData): Unit = none
  def gotFirstPreimage(data: OutgoingPaymentSenderData, fulfill: RemoteFulfill): Unit = none
}

class OutgoingPaymentSender(val fullTag: FullPaymentTag, val listeners: Iterable[OutgoingPaymentListener], opm: OutgoingPaymentMaster) extends StateMachine[OutgoingPaymentSenderData] { me =>
  become(OutgoingPaymentSenderData(SendMultiPart(fullTag, Right(LNParams.minInvoiceExpiryDelta), SplitInfo(0L.msat, 0L.msat), LNParams.routerConf, invalidPubKey), Map.empty), INIT)

  def doProcess(msg: Any): Unit = (msg, state) match {
    case (reject: RemoteReject, ABORTED) => me abortMaybeNotify data.withoutPartId(reject.ourAdd.partId)
    case (reject: LocalReject, ABORTED) => me abortMaybeNotify data.withoutPartId(reject.localAdd.partId)
    case (reject: RemoteReject, INIT) => me abortMaybeNotify data.withLocalFailure(NOT_RETRYING_NO_DETAILS, reject.ourAdd.amountMsat)
    case (reject: LocalReject, INIT) => me abortMaybeNotify data.withLocalFailure(NOT_RETRYING_NO_DETAILS, reject.localAdd.amountMsat)
    case (reject: RemoteReject, SUCCEEDED) if reject.ourAdd.paymentHash == fullTag.paymentHash => become(data.withoutPartId(reject.ourAdd.partId), SUCCEEDED)
    case (reject: LocalReject, SUCCEEDED) if reject.localAdd.paymentHash == fullTag.paymentHash => become(data.withoutPartId(reject.localAdd.partId), SUCCEEDED)
    case (fulfill: RemoteFulfill, SUCCEEDED) if fulfill.ourAdd.paymentHash == fullTag.paymentHash => become(data.withoutPartId(fulfill.ourAdd.partId), SUCCEEDED)
    case (bag: InFlightPayments, SUCCEEDED) if data.inFlightParts.isEmpty && !bag.out.contains(fullTag) => for (listener <- listeners) listener.wholePaymentSucceeded(data)

    case (cmd: SendMultiPart, INIT | ABORTED) =>
      val chans = opm.rightNowSendable(cmd.allowedChans, cmd.totalFeeReserve)
      assignToChans(chans, OutgoingPaymentSenderData(cmd, Map.empty), cmd.split.myPart)

    case (CMDAbort, INIT | PENDING) if data.inFlightParts.isEmpty =>
      // When at least some parts get through we can eventaully expect for remote timeout
      // but if ALL parts are still waiting after local timeout then we need to fail a whole payment locally
      me abortMaybeNotify data.copy(parts = Map.empty).withLocalFailure(TIMED_OUT, data.cmd.split.myPart)

    case (fulfill: RemoteFulfill, INIT | PENDING | ABORTED) if fulfill.ourAdd.paymentHash == fullTag.paymentHash =>
      // Provide listener with ORIGINAL data which has all used routes intact
      for (listener <- listeners) listener.gotFirstPreimage(data, fulfill)
      become(data.withoutPartId(fulfill.ourAdd.partId), SUCCEEDED)

    case (CMDChanGotOnline, PENDING) =>
      data.parts.values.collectFirst {
        case wait: WaitForChanOnline =>
          val nowSendable = opm.rightNowSendable(data.cmd.allowedChans, feeLeftover)
          assignToChans(nowSendable, data.withoutPartId(wait.partId), wait.amount)
      }

    case (CMDAskForRoute, PENDING) =>
      data.parts.values.toList.collect { case wait: WaitForRouteOrInFlight if wait.flight.isEmpty => wait }.sortBy(_.amount).lastOption.foreach { wait =>
        val routeParams = RouteParams(feeReserve = feeLeftover, routeMaxLength = data.cmd.routerConf.initRouteMaxLength, routeMaxCltv = data.cmd.routerConf.routeMaxCltv)
        opm process RouteRequest(fullTag, wait.partId, invalidPubKey, data.cmd.targetNodeId, wait.amount, mkFakeLocalEdge(invalidPubKey, wait.cnc.commits.remoteInfo.nodeId), routeParams)
      }

    case (fail: NoRouteAvailable, PENDING) =>
      data.parts.values.collectFirst {
        case wait: WaitForRouteOrInFlight if wait.flight.isEmpty && wait.partId == fail.partId =>
          // localFailedChans includes a channel we just tried because there are no routes starting at this channel
          val singleCapableCncCandidates = opm.rightNowSendable(data.cmd.allowedChans diff wait.localFailedChans, feeLeftover)
          val otherOpt = singleCapableCncCandidates.collectFirst { case (cnc, sendable) if sendable >= wait.amount => cnc }

          otherOpt match {
            case None if canBeSplit(wait.amount) => become(data.withoutPartId(wait.partId), PENDING) doProcess CutIntoHalves(wait.amount)
            case Some(otherCapableCnc) => become(data.copy(parts = data.parts + wait.oneMoreLocalAttempt(otherCapableCnc).tuple), PENDING)
            case None => me abortMaybeNotify data.withoutPartId(wait.partId).withLocalFailure(NO_ROUTES_FOUND, wait.amount)
          }
      }

    case (found: RouteFound, PENDING) =>
      data.parts.values.collectFirst {
        case wait: WaitForRouteOrInFlight if wait.flight.isEmpty && wait.partId == found.partId && shouldStopEarly(wait, found) =>
          me abortMaybeNotify data.withoutPartId(wait.partId).withLocalFailure(NO_ROUTES_FOUND, wait.amount)

        case wait: WaitForRouteOrInFlight if wait.flight.isEmpty && wait.partId == found.partId =>
          val chainExpiry = data.cmd.chainExpiry match { case Right(delta) => delta.toCltvExpiry(LNParams.blockCount.get + 1) case Left(absolute) => absolute }
          val finalPayload = Onion.createMultiPartPayload(wait.amount, data.cmd.split.totalSum, chainExpiry, data.cmd.outerPaymentSecret, data.cmd.onionTlvs, data.cmd.userCustomTlvs)
          val (firstAmount, firstExpiry, onion) = OutgoingPacket.buildPacket(Sphinx.PaymentPacket)(wait.onionKey, fullTag.paymentHash, found.route.hops, finalPayload)
          val cmdAdd = CMD_ADD_HTLC(fullTag, firstAmount, firstExpiry, PacketAndSecrets(onion.packet, onion.sharedSecrets), finalPayload)
          become(data.copy(parts = data.parts + wait.copy(flight = InFlightInfo(cmdAdd, found.route).asSome).tuple), PENDING)
          wait.cnc.chan process cmdAdd
      }

    case (reject: LocalReject, PENDING) =>
      data.parts.values.collectFirst {
        case wait: WaitForRouteOrInFlight if wait.flight.isDefined && wait.partId == reject.localAdd.partId =>
          // localFailedChans includes a channel we just tried because for whatever reason this channel is incapable now
          val singleCapableCncCandidates = opm.rightNowSendable(data.cmd.allowedChans diff wait.localFailedChans, feeLeftover)
          val otherOpt = singleCapableCncCandidates.collectFirst { case (cnc, sendable) if sendable >= wait.amount => cnc }

          otherOpt match {
            case _ if reject.isInstanceOf[InPrincipleNotSendable] => me abortMaybeNotify data.withoutPartId(wait.partId).withLocalFailure(PAYMENT_NOT_SENDABLE, wait.amount)
            case None if reject.isInstanceOf[ChannelOffline] => assignToChans(opm.rightNowSendable(data.cmd.allowedChans, feeLeftover), data.withoutPartId(wait.partId), wait.amount)
            case Some(otherCapableCnc) => become(data.copy(parts = data.parts + wait.oneMoreLocalAttempt(otherCapableCnc).tuple), PENDING)
            case None => me abortMaybeNotify data.withoutPartId(wait.partId).withLocalFailure(RUN_OUT_OF_CAPABLE_CHANNELS, wait.amount)
          }
      }

    case (reject: RemoteUpdateMalform, PENDING) =>
      data.parts.values.collectFirst {
        case wait: WaitForRouteOrInFlight if wait.flight.isDefined && wait.partId == reject.ourAdd.partId =>
          // localFailedChans includes a channel we just tried because it leads to peer which somehow thinks our onion is malformed
          val singleCapableCncCandidates = opm.rightNowSendable(data.cmd.allowedChans diff wait.localFailedChans, feeLeftover)
          val otherOpt = singleCapableCncCandidates.collectFirst { case (cnc, sendable) if sendable >= wait.amount => cnc }

          otherOpt match {
            case Some(otherCapableCnc) => become(data.copy(parts = data.parts + wait.oneMoreLocalAttempt(otherCapableCnc).tuple), PENDING)
            case None => me abortMaybeNotify data.withoutPartId(wait.partId).withLocalFailure(PEER_COULD_NOT_PARSE_ONION, wait.amount)
          }
      }

    case (reject: RemoteUpdateFail, PENDING) =>
      data.parts.values.collectFirst {
        // TODO: this is only viable for base and trampoline-to-legacy MPP
        // TODO: when we send TMPP remote errors from targetNodeId have different meaning (since targetNodeId is not payee)
        // TODO: when we send TMPP remote errors may NOT have a corresponding hop in our route (originated beyond trampoline node)
        case wait: WaitForRouteOrInFlight if wait.flight.isDefined && wait.partId == reject.ourAdd.partId =>
          val InFlightInfo(cmd, route) = wait.flight.get

          Sphinx.FailurePacket.decrypt(reject.fail.reason, cmd.packetAndSecrets.sharedSecrets) map {
            case pkt if pkt.originNode == data.cmd.targetNodeId || PaymentTimeout == pkt.failureMessage =>
              me abortMaybeNotify data.withoutPartId(wait.partId).copy(failures = RemoteFailure(pkt, route) +: data.failures)

            case pkt @ Sphinx.DecryptedFailurePacket(originNodeId, failure: Update) =>
              // Pathfinder channels must be fully loaded from db at this point since we have already used them to construct a route earlier
              val isSignatureFine = opm.cm.pf.nodeIdFromUpdate(failure.update).contains(originNodeId) && Announcements.checkSig(failure.update)(originNodeId)

              if (isSignatureFine) {
                opm.cm.pf process failure.update
                val isEnabled = Announcements.isEnabled(failure.update.channelFlags)
                // If channel is disabled we exclude it for current MPP session, but may reuse in next one
                val channelFailIncrement = if (isEnabled) 1 else data.cmd.routerConf.maxChannelFailures

                route.getEdgeForNode(originNodeId) match {
                  case Some(edge) if edge.updExt.update.shortChannelId != failure.update.shortChannelId =>
                    // This is fine: remote node has used a different channel than the one we have initially requested
                    // But remote node may send such errors infinitely so increment this specific type of failure
                    // Still fail an originally selected channel since it has most likely been tried too
                    opm doProcess ChannelFailed(edge.toDescAndCapacity, channelFailIncrement)
                    opm doProcess NodeFailed(originNodeId, increment = 1)

                  case Some(edge) if edge.updExt.update.core.noPosition == failure.update.core.noPosition =>
                    // Remote node returned EXACTLY same update, this channel is likely imbalanced
                    opm doProcess ChannelFailed(edge.toDescAndCapacity, channelFailIncrement)

                  case _ =>
                    // Something like higher feerates or CLTV, channel is updated in graph and may be chosen once again
                    // But remote node may send oscillating updates infinitely so increment this specific type of failure
                    opm doProcess NodeFailed(originNodeId, channelFailIncrement)
                }
              } else {
                // Invalid sig is a severe violation, ban sender node for 6 subsequent MPP sessions
                opm doProcess NodeFailed(originNodeId, data.cmd.routerConf.maxStrangeNodeFailures * 32)
              }

              // Record a remote error and keep trying the rest
              resolveRemoteFail(RemoteFailure(pkt, route), wait)

            case pkt @ Sphinx.DecryptedFailurePacket(nodeId, _: Node) =>
              // Node may become fine on next payment, but ban it for current attempts
              opm doProcess NodeFailed(nodeId, data.cmd.routerConf.maxStrangeNodeFailures)
              resolveRemoteFail(RemoteFailure(pkt, route), wait)

            case pkt: Sphinx.DecryptedFailurePacket =>
              // This is not an update failure, better to avoid entirely
              route.getEdgeForNode(pkt.originNode).map(_.toDescAndCapacity) match {
                case Some(dnc) => opm doProcess ChannelFailed(dnc, data.cmd.routerConf.maxChannelFailures)
                case None => opm doProcess NodeFailed(pkt.originNode, data.cmd.routerConf.maxStrangeNodeFailures)
              }

              // Record an error and keep trying out the rest
              resolveRemoteFail(RemoteFailure(pkt, route), wait)

          } getOrElse {
            val inBetweenOpt = shuffle(route.hops drop 1 dropRight 1).headOption
            for (hop <- inBetweenOpt) NodeFailed(hop.nodeId, data.cmd.routerConf.maxStrangeNodeFailures)
            // We don't know who is sending garbage, exclude a random unlucky node and keep trying out the rest
            resolveRemoteFail(UnreadableRemoteFailure(route), wait)
          }
      }

    case (cut: CutIntoHalves, PENDING) =>
      val partOne: MilliSatoshi = cut.amount / 2
      val partTwo: MilliSatoshi = cut.amount - partOne
      // Run sequentially as this mutates data, both RightNowSendable and Data are updated
      assignToChans(opm.rightNowSendable(data.cmd.allowedChans, feeLeftover), data, partOne)
      assignToChans(opm.rightNowSendable(data.cmd.allowedChans, feeLeftover), data, partTwo)

    case _ =>
  }

  def feeLeftover: MilliSatoshi = data.cmd.totalFeeReserve - data.usedFee

  def canBeSplit(totalAmount: MilliSatoshi): Boolean = totalAmount / 2 > LNParams.minPayment

  def shouldStopEarly(wait: WaitForRouteOrInFlight, found: RouteFound): Boolean = {
    val avgHopFee = opm.cm.pf.lastAvgHopParams.map(_ avgHopFee data.cmd.split.myPart).getOrElse(1000L.msat)
    // Stop trying if we know we would have to add more parts and this one is already taking too much from fee reserve
    data.inFlightParts.isEmpty && wait.amount < data.cmd.split.myPart && feeLeftover - found.route.fee < avgHopFee
  }

  def assignToChans(sendable: mutable.Map[ChanAndCommits, MilliSatoshi], data1: OutgoingPaymentSenderData, amount: MilliSatoshi): Unit = {
    // This is a terminal method in a sense that it either successfully assigns a given amount to channels or turns a payment into failed state
    val directChansFirst = shuffle(sendable.toSeq) sortBy { case (cnc, _) => if (cnc.commits.remoteInfo.nodeId == data1.cmd.targetNodeId) 0 else 1 }
    // This method always sets a new partId to assigned parts so old payment statuses in data must be cleared before calling it

    directChansFirst.foldLeft(Map.empty[ByteVector, PartStatus] -> amount) {
      case (accumulator ~ leftover, cnc ~ chanSendable) if leftover > 0L.msat =>
        // If leftover becomes less than sendable minimum then we must bump it upwards
        // Example: channel leftover=500, chanSendable=200 -> sending 200
        // Example: channel leftover=300, chanSendable=400 -> sending 300

        val noFeeAmount = leftover.min(chanSendable)
        val wait = WaitForRouteOrInFlight(randomKey, noFeeAmount, cnc)
        (accumulator + wait.tuple, leftover - wait.amount)

      case (collected, _) =>
        // No more amount to assign
        // Propagate what's collected
        collected

    } match {
      case (newParts, rest) if rest <= 0L.msat =>
        // A whole amount has been fully split across our local channels
        // leftover may be slightly negative due to min sendable corrections
        become(data1.copy(parts = data1.parts ++ newParts), PENDING)

      case (_, rest) if opm.getSendable(data1.cmd.allowedChans.filter(Channel.isOperationalAndSleeping), feeLeftover).values.sum >= rest =>
        // Amount has not been fully split, but it is possible to further successfully split it once some SLEEPING channel becomes OPEN
        become(data1.copy(parts = data1.parts + WaitForChanOnline(randomKey, amount).tuple), PENDING)

      case _ =>
        // A positive leftover is present with no more channels left
        // partId should have already been removed from data at this point
        me abortMaybeNotify data1.withLocalFailure(NOT_ENOUGH_FUNDS, amount)
    }

    // It may happen that all chans are to stay offline indefinitely, payment parts will then await indefinitely
    // so set a timer to abort a payment in case if we have no in-flight parts after some reasonable amount of time
    // note that timer gets reset each time this method gets called
    delayedCMDWorker.replaceWork(CMDAbort)
  }

  // Turn in-flight into waiting-for-route and expect for subsequent CMDAskForRoute
  def resolveRemoteFail(failure: PaymentFailure, wait: WaitForRouteOrInFlight): Unit = {
    // Remove pending part from data right away to not interfere with sendable calculations
    become(data.withoutPartId(wait.partId).copy(failures = failure +: data.failures), PENDING)
    val singleCapableCncCandidates = shuffle(opm.rightNowSendable(data.cmd.allowedChans, feeLeftover).toSeq)
    val otherOpt = singleCapableCncCandidates.collectFirst { case (cnc, sendable) if sendable >= wait.amount => cnc }
    val keepSendingAsOnePart = wait.remoteAttempts < data.cmd.routerConf.maxRemoteAttempts

    otherOpt match {
      case Some(otherCapableCnc) if keepSendingAsOnePart => become(data.copy(parts = data.parts + wait.oneMoreRemoteAttempt(otherCapableCnc).tuple), PENDING)
      case _ if canBeSplit(wait.amount) => become(data, PENDING) doProcess CutIntoHalves(wait.amount)
      case _ => me abortMaybeNotify data.withLocalFailure(RUN_OUT_OF_RETRY_ATTEMPTS, wait.amount)
    }
  }

  def abortMaybeNotify(data1: OutgoingPaymentSenderData): Unit = {
    val isFinalized = data1.inFlightParts.isEmpty && !opm.cm.allInChannelOutgoing.contains(fullTag)
    for (listener <- listeners if isFinalized) listener.wholePaymentFailed(data1)
    become(data1, ABORTED)
  }
}
