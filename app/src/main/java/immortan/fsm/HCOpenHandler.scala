package immortan.fsm

import immortan.{ChannelHosted, ChannelListener, ChannelMaster, CommsTower, ConnectionListener, HostedCommits, RemoteNodeInfo, WaitRemoteHostedReply}
import fr.acinq.eclair.wire.{ChannelUpdate, HasChannelId, HostedChannelBranding, HostedChannelMessage, Init, LightningMessage, LightningMessageCodecs}
import fr.acinq.eclair.channel.{CMD_SOCKET_ONLINE, PersistentChannelData}
import immortan.ChannelListener.{Malfunction, Transition}
import immortan.Channel.{OPEN, WAIT_FOR_ACCEPT}
import fr.acinq.bitcoin.ByteVector32
import scodec.bits.ByteVector
import immortan.crypto.Tools


// Secret and refund pubKey are supplied externally because they may be different depending if we have a chain wallet or not
abstract class HCOpenHandler(info: RemoteNodeInfo, peerSpecificSecret: ByteVector32, peerSpecificRefundPubKey: ByteVector, cm: ChannelMaster) {
  val channelId: ByteVector32 = Tools.hostedChanId(info.nodeSpecificPubKey.value, info.nodeId.value)

  private val freshChannel = new ChannelHosted {
    def SEND(msgs: LightningMessage*): Unit = CommsTower.sendMany(msgs.map(LightningMessageCodecs.prepareNormal), info.nodeSpecificPair)
    def STORE(hostedData: PersistentChannelData): PersistentChannelData = cm.chanBag.put(hostedData)
  }

  def onEstablished(channel: ChannelHosted): Unit
  def onFailure(err: Throwable): Unit

  private val makeChanListener = new ConnectionListener with ChannelListener { me =>
    override def onDisconnect(worker: CommsTower.Worker): Unit = CommsTower.rmListenerNative(info, me)
    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = freshChannel process CMD_SOCKET_ONLINE

    override def onHostedMessage(worker: CommsTower.Worker, message: HostedChannelMessage): Unit = message match {
      case msg: HostedChannelBranding => cm.dataBag.putBranding(worker.info.nodeId, msg)
      case _ => freshChannel process message
    }

    override def onMessage(worker: CommsTower.Worker, message: LightningMessage): Unit = message match {
      case msg: HasChannelId if msg.channelId == channelId => freshChannel process msg
      case msg: ChannelUpdate => freshChannel process msg
      case _ =>
    }

    override def onBecome: PartialFunction[Transition, Unit] = {
      case (_, _, commits: HostedCommits, WAIT_FOR_ACCEPT, OPEN) =>
        cm.implantChannel(commits, freshChannel)
        CommsTower.rmListenerNative(info, me)
        onEstablished(freshChannel)
    }

    override def onException: PartialFunction[Malfunction, Unit] = {
      // Something went wrong while trying to establish a channel

      case (openingPhaseError, _, _) =>
        CommsTower.rmListenerNative(info, me)
        onFailure(openingPhaseError)
    }
  }

  if (cm.hostedFromNode(info.nodeId).isEmpty) {
    freshChannel.listeners = Set(makeChanListener)
    freshChannel doProcess WaitRemoteHostedReply(info, peerSpecificRefundPubKey, peerSpecificSecret)
    CommsTower.listenNative(listeners1 = Set(makeChanListener), remoteInfo = info)
  } else {
    // Only one HC per remote peer is allowed, make sure this condition holds
    val error = new RuntimeException("Hosted channel with peer exists already")
    onFailure(error)
  }
}
