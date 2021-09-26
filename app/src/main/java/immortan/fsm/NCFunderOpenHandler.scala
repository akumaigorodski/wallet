package immortan.fsm

import immortan._
import fr.acinq.eclair._
import fr.acinq.eclair.wire._
import fr.acinq.eclair.channel._
import scala.util.{Failure, Success}
import fr.acinq.bitcoin.{ByteVector32, Satoshi, Script}
import immortan.ChannelListener.{Malfunction, Transition}
import immortan.Channel.{WAIT_FOR_ACCEPT, WAIT_FUNDING_DONE}
import fr.acinq.eclair.blockchain.MakeFundingTxResponse
import concurrent.ExecutionContext.Implicits.global
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.transactions.Scripts
import fr.acinq.bitcoin.Crypto.PublicKey
import scala.concurrent.Future


object NCFunderOpenHandler {
  val dummyLocal: PublicKey = randomKey.publicKey
  val dummyRemote: PublicKey = randomKey.publicKey

  def makeFunding(chainWallet: WalletExt, fundingAmount: Satoshi, feeratePerKw: FeeratePerKw, local: PublicKey = dummyLocal, remote: PublicKey = dummyRemote): Future[MakeFundingTxResponse] =
    chainWallet.lnWallet.makeFundingTx(Script.write(Script pay2wsh Scripts.multiSig2of2(local, remote).toList), fundingAmount, feeratePerKw)
}

abstract class NCFunderOpenHandler(info: RemoteNodeInfo, fakeFunding: MakeFundingTxResponse, fundingFeeratePerKw: FeeratePerKw, cw: WalletExt, cm: ChannelMaster) {
  // Important: this must be initiated when chain tip is actually known
  def onEstablished(cs: Commitments, channel: ChannelNormal): Unit
  def onFailure(err: Throwable): Unit

  private val tempChannelId: ByteVector32 = randomBytes32
  private val freshChannel = new ChannelNormal(cm.chanBag) {
    def SEND(messages: LightningMessage*): Unit = CommsTower.sendMany(messages, info.nodeSpecificPair)
    def STORE(normalData: PersistentChannelData): PersistentChannelData = cm.chanBag.put(normalData)
    val chainWallet: WalletExt = cw
  }

  private var assignedChanId = Option.empty[ByteVector32]
  private val makeChanListener = new ConnectionListener with ChannelListener { me =>
    override def onDisconnect(worker: CommsTower.Worker): Unit = CommsTower.rmListenerNative(info, me)

    override def onMessage(worker: CommsTower.Worker, message: LightningMessage): Unit = message match {
      case msg: HasTemporaryChannelId if msg.temporaryChannelId == tempChannelId => freshChannel process msg
      case msg: HasChannelId if assignedChanId.contains(msg.channelId) => freshChannel process msg
      case msg: HasChannelId if msg.channelId == tempChannelId => freshChannel process msg
      case _ =>
    }

    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = {
      val stickyChannelFeatures = ChannelFeatures.pickChannelFeatures(LNParams.ourInit.features, theirInit.features)
      val localParams = LNParams.makeChannelParams(freshChannel.chainWallet, isFunder = true, fakeFunding.fundingAmount)
      val initialFeeratePerKw = LNParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(LNParams.feeRates.info.onChainFeeConf.feeTargets.commitmentBlockTarget)
      val cmd = INPUT_INIT_FUNDER(info.safeAlias, tempChannelId, fakeFunding, 0L.msat, fundingFeeratePerKw, initialFeeratePerKw, localParams, theirInit, 0.toByte, stickyChannelFeatures)
      freshChannel process cmd
    }

    override def onBecome: PartialFunction[Transition, Unit] = {
      case (_, _: DATA_WAIT_FOR_ACCEPT_CHANNEL, data: DATA_WAIT_FOR_FUNDING_INTERNAL, WAIT_FOR_ACCEPT, WAIT_FOR_ACCEPT) =>
        val future = NCFunderOpenHandler.makeFunding(cw, data.initFunder.fakeFunding.fundingAmount, data.initFunder.fundingFeeratePerKw, data.lastSent.fundingPubkey, data.remoteParams.fundingPubKey)
        future onComplete { case Failure(failureReason) => onException(failureReason, freshChannel, data) case Success(realFundingTx) => freshChannel process realFundingTx }

      case (_, _: DATA_WAIT_FOR_FUNDING_INTERNAL, data: DATA_WAIT_FOR_FUNDING_SIGNED, WAIT_FOR_ACCEPT, WAIT_FOR_ACCEPT) =>
        // Once funding tx becomes known peer will start sending messages using a real channel ID, not a temp one
        assignedChanId = Some(data.channelId)

      case (_, _, data: DATA_WAIT_FOR_FUNDING_CONFIRMED, WAIT_FOR_ACCEPT, WAIT_FUNDING_DONE) =>
        // On disconnect we remove this listener from CommsTower, but retain it as channel listener
        // this ensures successful implanting if disconnect happens while funding is being published
        onEstablished(data.commitments, freshChannel)
        CommsTower.rmListenerNative(info, me)
    }

    override def onException: PartialFunction[Malfunction, Unit] = {
      // Something went wrong while trying to establish a new channel

      case (openingPhaseError, _, _) =>
        CommsTower.rmListenerNative(info, me)
        onFailure(openingPhaseError)
    }
  }

  freshChannel.listeners = Set(makeChanListener)
  CommsTower.listenNative(Set(makeChanListener), info)
}
