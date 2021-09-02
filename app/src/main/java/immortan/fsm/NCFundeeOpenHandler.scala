package immortan.fsm

import fr.acinq.eclair.channel.{ChannelFeatures, Commitments, DATA_WAIT_FOR_FUNDING_CONFIRMED, INPUT_INIT_FUNDEE, PersistentChannelData}
import immortan.{ChannelListener, ChannelMaster, ChannelNormal, CommsTower, ConnectionListener, LNParams, RemoteNodeInfo}
import fr.acinq.eclair.wire.{HasChannelId, HasTemporaryChannelId, Init, LightningMessage, OpenChannel}
import immortan.Channel.{WAIT_FOR_ACCEPT, WAIT_FUNDING_DONE}
import immortan.ChannelListener.{Malfunction, Transition}


abstract class NCFundeeOpenHandler(info: RemoteNodeInfo, theirOpen: OpenChannel, cm: ChannelMaster) {
  // Important: this must be initiated when chain tip is actually known
  def onEstablished(cs: Commitments, channel: ChannelNormal): Unit
  def onFailure(err: Throwable): Unit

  private val freshChannel = new ChannelNormal(cm.chanBag) {
    def SEND(messages: LightningMessage*): Unit = CommsTower.sendMany(messages, info.nodeSpecificPair)
    def STORE(normalData: PersistentChannelData): PersistentChannelData = cm.chanBag.put(normalData)
    val chainWallet: LNParams.WalletExt = LNParams.chainWallets
  }

  private val makeChanListener = new ConnectionListener with ChannelListener { me =>
    override def onDisconnect(worker: CommsTower.Worker): Unit = CommsTower.rmListenerNative(info, me)

    override def onMessage(worker: CommsTower.Worker, message: LightningMessage): Unit = message match {
      case msg: HasTemporaryChannelId if msg.temporaryChannelId == theirOpen.temporaryChannelId => freshChannel process msg
      case msg: HasChannelId if msg.channelId == theirOpen.temporaryChannelId => freshChannel process msg
      case _ => // Do nothing to avoid conflicts
    }

    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = {
      val stickyChannelFeatures = ChannelFeatures.pickChannelFeatures(LNParams.ourInit.features, theirInit.features)
      val localParams = LNParams.makeChannelParams(freshChannel.chainWallet, isFunder = false, theirOpen.fundingSatoshis)
      freshChannel process INPUT_INIT_FUNDEE(info.safeAlias, localParams, theirInit, stickyChannelFeatures, theirOpen)
    }

    override def onBecome: PartialFunction[Transition, Unit] = {
      case (_, _, data: DATA_WAIT_FOR_FUNDING_CONFIRMED, WAIT_FOR_ACCEPT, WAIT_FUNDING_DONE) =>
        // It is up to NC to store itself and communicate successful opening
        onEstablished(data.commitments, freshChannel)
        CommsTower.rmListenerNative(info, me)
    }

    override def onException: PartialFunction[Malfunction, Unit] = {
      // Something went wrong while trying to establish a channel

      case (openingPhaseError, _, _) =>
        CommsTower.rmListenerNative(info, me)
        onFailure(openingPhaseError)
    }
  }

  freshChannel.listeners = Set(makeChanListener)
  CommsTower.listenNative(Set(makeChanListener), info)
}