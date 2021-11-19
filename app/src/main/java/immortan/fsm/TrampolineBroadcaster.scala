package immortan.fsm

import java.util.concurrent.Executors

import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair.Features.PrivateRouting
import fr.acinq.eclair.wire.{Init, TrampolineOn, TrampolineStatus, TrampolineUndesired}
import immortan.crypto.{CanBeShutDown, StateMachine}
import immortan.fsm.TrampolineBroadcaster._
import immortan.{Channel, ChannelMaster, CommsTower, ConnectionListener, LNParams, RemoteNodeInfo}
import rx.lang.scala.Observable

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}


object TrampolineBroadcaster {
  final val ROUTING_DISABLED = 0
  final val ROUTING_ENABLED = 1

  val CMDBroadcast = "cmd-broadcast"
}

class TrampolineBroadcastMaster(maxRoutable: Double, cm: ChannelMaster) extends StateMachine[TrampolineStatus] with ConnectionListener with CanBeShutDown { me =>
  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  private val subscription = Observable.interval(5.seconds).subscribe(_ => me process CMDBroadcast)
  private var broadcasters: Map[PublicKey, LastBroadcast] = Map.empty

  def process(message: Any): Unit = scala.concurrent.Future(me doProcess message)
  override def becomeShutDown: Unit = subscription.unsubscribe
  become(TrampolineUndesired, ROUTING_DISABLED)

  override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = {
    val isPrivateRoutingEnabled = LNParams.isPeerSupports(theirInit)(PrivateRouting)
    if (isPrivateRoutingEnabled) me process LastBroadcast(None, worker.info)
  }

  override def onDisconnect(worker: CommsTower.Worker): Unit =
    scala.concurrent.Future(broadcasters -= worker.info.nodeId)

  def doProcess(change: Any): Unit = (change, state, data) match {
    case (CMDBroadcast, ROUTING_ENABLED, trampolineOn: TrampolineOn) =>
      broadcasters = broadcasters.mapValues(_ broadcast trampolineOn)

    case (trampolineOn: TrampolineOn, ROUTING_DISABLED, TrampolineUndesired) =>
      become(trampolineOn, ROUTING_ENABLED)

    case (TrampolineUndesired, ROUTING_ENABLED, _: TrampolineOn) =>
      become(TrampolineUndesired, ROUTING_DISABLED)

    case (noBroadcast: LastBroadcast, ROUTING_DISABLED, TrampolineUndesired) =>
      broadcasters = broadcasters.updated(noBroadcast.info.nodeId, noBroadcast)

    case (noBroadcast: LastBroadcast, ROUTING_ENABLED, trampolineOn: TrampolineOn) =>
      broadcasters = broadcasters.updated(noBroadcast.info.nodeId, noBroadcast broadcast trampolineOn)

    case _ =>
  }

  // select all channels besides ours
  // get their total amount

  case class LastBroadcast(lastOn: Option[TrampolineOn], info: RemoteNodeInfo) {
    def broadcast(templateOn: TrampolineOn): LastBroadcast = {
      val usableChannels = cm.all.values.filter(Channel.isOperationalAndOpen).flatMap(Channel.chanAndCommitsOpt)
      val (peerChannels, otherChannels) = usableChannels.partition(_.commits.remoteInfo.nodeId == info.nodeId)
      val canReceiveFromPeer = peerChannels.map(_.commits.availableForReceive).sum

//      val totalSendable = .filterNot(_.commits.remoteInfo.nodeId == info.nodeId).map(_.commits.availableForSend).sum

      ???
    }
  }


}