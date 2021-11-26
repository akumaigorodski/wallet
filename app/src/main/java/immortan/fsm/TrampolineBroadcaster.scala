package immortan.fsm

import java.util.concurrent.Executors

import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair.Features.PrivateRouting
import fr.acinq.eclair.wire.{Init, TrampolineOn, TrampolineStatus, TrampolineUndesired}
import immortan.crypto.Tools._
import immortan.crypto.{CanBeShutDown, StateMachine}
import immortan.fsm.TrampolineBroadcaster._
import immortan._
import rx.lang.scala.Observable

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}


object TrampolineBroadcaster {
  final val ROUTING_DISABLED = 0
  final val ROUTING_ENABLED = 1

  val CMDBroadcast = "cmd-broadcast"

  case class LastBroadcast(lastOn: Option[TrampolineOn], info: RemoteNodeInfo, maxRoutableRatio: Double) {
    def updated(usableChannels: Iterable[ChanAndCommits], templateTrampolineOn: TrampolineOn): LastBroadcast = {
      val (peerChannels, otherChannels) = usableChannels.partition(_.commits.remoteInfo.nodeId == info.nodeId)

      val canReceiveFromPeer = peerChannels.map(_.commits.availableForReceive).sum
      val canSendOut = otherChannels.map(_.commits.availableForSend * maxRoutableRatio).sum
      copy(lastOn = templateTrampolineOn.copy(maxMsat = canSendOut min canReceiveFromPeer).asSome)
    }
  }
}

// Staggered broadcast of routing params to each connected peer when they change (other peers connect/disconnect, balances change, user actions)
class TrampolineBroadcaster(cm: ChannelMaster) extends StateMachine[TrampolineStatus] with ConnectionListener with CanBeShutDown { me =>
  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  private val subscription = Observable.interval(5.seconds).subscribe(_ => me process CMDBroadcast)
  var broadcasters: Map[PublicKey, LastBroadcast] = Map.empty

  def doBroadcast(msg: Option[TrampolineStatus], info: RemoteNodeInfo): Unit = CommsTower.sendMany(msg, info.nodeSpecificPair)
  def process(message: Any): Unit = scala.concurrent.Future(me doProcess message)
  override def becomeShutDown: Unit = subscription.unsubscribe
  become(TrampolineUndesired, ROUTING_DISABLED)

  override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = {
    val isPrivateRoutingEnabled = LNParams.isPeerSupports(theirInit)(feature = PrivateRouting)
    if (isPrivateRoutingEnabled) me process LastBroadcast(None, worker.info, maxRoutableRatio = 0.9D)
  }

  override def onDisconnect(worker: CommsTower.Worker): Unit =
    scala.concurrent.Future(broadcasters -= worker.info.nodeId)

  def doProcess(change: Any): Unit = (change, state, data) match {
    case (CMDBroadcast, ROUTING_ENABLED, templateOn: TrampolineOn) =>
      // First make a map with updated values, then broadcast differences, then update map
      val usableChannels = cm.all.values.filter(Channel.isOperationalAndOpen).flatMap(Channel.chanAndCommitsOpt)
      val broadcasters1 = for (Tuple2(nodeId, lastBroadcast) <- broadcasters) yield nodeId -> lastBroadcast.updated(usableChannels, templateOn)

      for {
        Tuple2(nodeId, lastBroadcast) <- broadcasters
        lastOn1 <- broadcasters1.get(nodeId).map(_.lastOn) if lastOn1 != lastBroadcast.lastOn
      } doBroadcast(lastOn1.map(_.finalMessageToSend), lastBroadcast.info)
      broadcasters = broadcasters1

    case (templateOn1: TrampolineOn, ROUTING_DISABLED | ROUTING_ENABLED, _) =>
      become(templateOn1, ROUTING_ENABLED)

    case (TrampolineUndesired, ROUTING_ENABLED, _) =>
      broadcasters = for (Tuple2(nodeId, lastBroadcast) <- broadcasters) yield nodeId -> lastBroadcast.copy(lastOn = None)
      for (lastBroadcast <- broadcasters.values) doBroadcast(TrampolineUndesired.asSome, lastBroadcast.info)
      become(TrampolineUndesired, ROUTING_DISABLED)

    case (lastOn1: LastBroadcast, ROUTING_DISABLED | ROUTING_ENABLED, _) =>
      broadcasters = broadcasters.updated(lastOn1.info.nodeId, lastOn1)

    case _ =>
  }
}