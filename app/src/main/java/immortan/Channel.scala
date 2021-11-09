package immortan

import java.util.concurrent.Executors

import akka.actor.{Actor, ActorRef, Props}
import fr.acinq.bitcoin.ByteVector32
import fr.acinq.eclair.MilliSatoshi
import fr.acinq.eclair.blockchain.CurrentBlockCount
import fr.acinq.eclair.channel._
import fr.acinq.eclair.transactions.{RemoteFulfill, RemoteReject}
import fr.acinq.eclair.wire.LightningMessage
import immortan.Channel.channelContext
import immortan.crypto.Tools._
import immortan.crypto.{CanBeRepliedTo, StateMachine}

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.util.Failure


object Channel {
  final val WAIT_FOR_INIT = 0
  final val WAIT_FOR_ACCEPT = 1
  final val WAIT_FUNDING_DONE = 2
  final val SLEEPING = 3
  final val CLOSING = 4
  final val OPEN = 5

  // Single stacking thread for all channels, must be used when asking channels for pending payments to avoid race conditions
  implicit val channelContext: ExecutionContextExecutor = scala.concurrent.ExecutionContext fromExecutor Executors.newSingleThreadExecutor

  def load(listeners: Set[ChannelListener], bag: ChannelBag): Map[ByteVector32, Channel] = bag.all.map {
    case data: HasNormalCommitments => data.channelId -> ChannelNormal.make(listeners, data, bag)
    case data: HostedCommits => data.channelId -> ChannelHosted.make(listeners, data, bag)
    case _ => throw new RuntimeException
  }.toMap

  def totalBalance(chans: Iterable[Channel] = Nil): MilliSatoshi =
    chans.collect { case chan if isOperationalOrWaiting(chan) => chan.data }.map {
      case data: HasNormalCommitments => data.commitments.latestReducedRemoteSpec.toRemote
      case data: HostedCommits => data.nextLocalSpec.toLocal
      case _ => MilliSatoshi(0L)
    }.sum

  def chanAndCommitsOpt(chan: Channel): Option[ChanAndCommits] = chan.data match {
    case data: HasNormalCommitments => ChanAndCommits(chan, data.commitments).asSome
    case data: HostedCommits => ChanAndCommits(chan, data).asSome
    case _ => None
  }

  def isOperational(chan: Channel): Boolean = chan.data match {
    case data: DATA_NORMAL => data.localShutdown.isEmpty && data.remoteShutdown.isEmpty
    case hostedCommits: HostedCommits => hostedCommits.error.isEmpty
    case _ => false
  }

  def isWaiting(chan: Channel): Boolean = chan.data match {
    case _: DATA_WAIT_FOR_FUNDING_CONFIRMED => true
    case _: DATA_WAIT_FOR_FUNDING_LOCKED => true
    case _ => false
  }

  def isOperationalOrWaiting(chan: Channel): Boolean = isOperational(chan) || isWaiting(chan)

  def isOperationalAndOpen(chan: Channel): Boolean = isOperational(chan) && OPEN == chan.state

  def isOperationalAndSleeping(chan: Channel): Boolean = isOperational(chan) && SLEEPING == chan.state
}

trait Channel extends StateMachine[ChannelData] with CanBeRepliedTo { me =>
  def process(changeMsg: Any): Unit = Future(me doProcess changeMsg).onComplete {
    case Failure(reason) => events onException Tuple3(reason, me, data)
    case _ => // Do nothing
  }

  def SEND(msg: LightningMessage*): Unit

  def STORE(data: PersistentChannelData): PersistentChannelData

  def BECOME(data1: ChannelData, state1: Int): Unit = {
    // Transition must be defined before vars are updated
    val trans = (me, data, data1, state, state1)
    super.become(data1, state1)
    events.onBecome(trans)
  }

  def StoreBecomeSend(data1: PersistentChannelData, state1: Int, lnMessage: LightningMessage*): Unit = {
    // Storing first to ensure we retain an updated data before revealing it if anything goes wrong

    STORE(data1)
    BECOME(data1, state1)
    SEND(lnMessage:_*)
  }

  var listeners = Set.empty[ChannelListener]

  val events: ChannelListener = new ChannelListener {
    override def onException: PartialFunction[ChannelListener.Malfunction, Unit] = { case tuple => for (lst <- listeners if lst.onException isDefinedAt tuple) lst onException tuple }
    override def onBecome: PartialFunction[ChannelListener.Transition, Unit] = { case tuple => for (lst <- listeners if lst.onBecome isDefinedAt tuple) lst onBecome tuple }
    override def addRejectedRemotely(reason: RemoteReject): Unit = for (lst <- listeners) lst.addRejectedRemotely(reason)
    override def addRejectedLocally(reason: LocalReject): Unit = for (lst <- listeners) lst.addRejectedLocally(reason)
    override def fulfillReceived(fulfill: RemoteFulfill): Unit = for (lst <- listeners) lst.fulfillReceived(fulfill)
    override def addReceived(add: UpdateAddHtlcExt): Unit = for (lst <- listeners) lst.addReceived(add)
    override def notifyResolvers: Unit = for (lst <- listeners) lst.notifyResolvers
  }

  val receiver: ActorRef = LNParams.system actorOf Props(new ActorEventsReceiver)

  class ActorEventsReceiver extends Actor {
    context.system.eventStream.subscribe(channel = classOf[CurrentBlockCount], subscriber = self)

    override def receive: Receive = main(lastSeenBlockCount = None, useDelay = true)

    def main(lastSeenBlockCount: Option[CurrentBlockCount], useDelay: Boolean): Receive = {
      case currentBlockCount: CurrentBlockCount if lastSeenBlockCount.isEmpty && useDelay =>
        context.system.scheduler.scheduleOnce(10.seconds)(self ! "propagate")(LNParams.ec)
        context become main(currentBlockCount.asSome, useDelay = true)

      case currentBlockCount: CurrentBlockCount if lastSeenBlockCount.isDefined && useDelay =>
        // We may get another chain tip while delaying a current one: store a new one then
        context become main(currentBlockCount.asSome, useDelay = true)

      case "propagate" =>
        // Propagate subsequent block counts right away
        context become main(None, useDelay = false)
        // Popagate the last delayed block count
        lastSeenBlockCount.foreach(process)

      case message =>
        process(message)
    }
  }
}

object ChannelListener {
  type Malfunction = (Throwable, Channel, ChannelData)
  type Transition = (Channel, ChannelData, ChannelData, Int, Int)
}

trait ChannelListener {
  def onException: PartialFunction[ChannelListener.Malfunction, Unit] = none
  def onBecome: PartialFunction[ChannelListener.Transition, Unit] = none
  def addRejectedRemotely(reason: RemoteReject): Unit = none
  def addRejectedLocally(reason: LocalReject): Unit = none
  def fulfillReceived(fulfill: RemoteFulfill): Unit = none
  def addReceived(add: UpdateAddHtlcExt): Unit = none
  def notifyResolvers: Unit = none
}

case class ChanAndCommits(chan: Channel, commits: Commitments)
case class CommitsAndMax(commits: Seq[ChanAndCommits], maxReceivable: MilliSatoshi)
