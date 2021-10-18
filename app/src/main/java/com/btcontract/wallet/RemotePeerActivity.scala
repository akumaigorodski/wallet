package com.btcontract.wallet

import android.os.Bundle
import android.view.View
import android.widget.{LinearLayout, ProgressBar, TextView}
import androidx.appcompat.app.AlertDialog
import com.btcontract.wallet.BaseActivity.StringOps
import com.btcontract.wallet.Colors._
import com.btcontract.wallet.R.string._
import com.ornach.nobobutton.NoboButton
import fr.acinq.bitcoin._
import fr.acinq.eclair.Features._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.GenerateTxResponse
import fr.acinq.eclair.blockchain.fee.FeeratePerByte
import fr.acinq.eclair.channel.Commitments
import fr.acinq.eclair.wire._
import immortan._
import immortan.crypto.Tools._
import immortan.fsm.{HCOpenHandler, NCFundeeOpenHandler, NCFunderOpenHandler}
import immortan.utils._
import rx.lang.scala.Observable

import scala.concurrent.ExecutionContext.Implicits.global


object RemotePeerActivity {
  def implantNewChannel(cs: Commitments, freshChannel: Channel): Unit = {
    // Make an immediate channel backup if anything goes wrong next
    // At this point channel has saved itself in the database
    WalletApp.backupSaveWorker.replaceWork(false)

    LNParams.cm.pf process PathFinder.CMDStartPeriodicResync
    LNParams.cm.all += Tuple2(cs.channelId, freshChannel)
    // This removes all previous channel listeners
    freshChannel.listeners = Set(LNParams.cm)
    LNParams.cm.initConnect

    // Update view on hub activity and finalize local stuff
    ChannelMaster.next(ChannelMaster.statusUpdateStream)
  }
}

class RemotePeerActivity extends ChanErrorHandlerActivity with ExternalDataChecker { me =>
  private[this] lazy val peerNodeKey = findViewById(R.id.peerNodeKey).asInstanceOf[TextView]
  private[this] lazy val peerIpAddress = findViewById(R.id.peerIpAddress).asInstanceOf[TextView]

  private[this] lazy val progressBar = findViewById(R.id.progressBar).asInstanceOf[ProgressBar]
  private[this] lazy val peerDetails = findViewById(R.id.peerDetails).asInstanceOf[LinearLayout]
  private[this] lazy val viewNoFeatureSupport = findViewById(R.id.viewNoFeatureSupport).asInstanceOf[TextView]
  private[this] lazy val viewYesFeatureSupport = findViewById(R.id.viewYesFeatureSupport).asInstanceOf[LinearLayout]
  private[this] lazy val optionHostedChannel = findViewById(R.id.optionHostedChannel).asInstanceOf[NoboButton]

  private[this] lazy val criticalFeatures = Set(BasicMultiPartPayment, OptionDataLossProtect, StaticRemoteKey)

  private[this] lazy val featureTextViewMap = Map(
    ChannelRangeQueriesExtended -> findViewById(R.id.ChannelRangeQueriesExtended).asInstanceOf[TextView],
    OptionDataLossProtect -> findViewById(R.id.OptionDataLossProtect).asInstanceOf[TextView],
    BasicMultiPartPayment -> findViewById(R.id.BasicMultiPartPayment).asInstanceOf[TextView],
    TrampolineRouting -> findViewById(R.id.TrampolineRouting).asInstanceOf[TextView],
    StaticRemoteKey -> findViewById(R.id.StaticRemoteKey).asInstanceOf[TextView],
    HostedChannels -> findViewById(R.id.HostedChannels).asInstanceOf[TextView],
    ChainSwap -> findViewById(R.id.ChainSwap).asInstanceOf[TextView],
    Wumbo -> findViewById(R.id.Wumbo).asInstanceOf[TextView]
  )

  class DisconnectListener extends ConnectionListener {
    override def onDisconnect(worker: CommsTower.Worker): Unit = {
      UITask(WalletApp.app quickToast R.string.rpa_disconnected).run
      disconnectListenersAndFinish
    }
  }

  private lazy val incomingAcceptingListener = new DisconnectListener {
    override def onMessage(worker: CommsTower.Worker, message: LightningMessage): Unit = message match {
      case theirMsg: ChannelReestablish if !LNParams.cm.all.contains(theirMsg.channelId) => chanUnknown(worker, theirMsg)
      case theirMsg: OpenChannel if (theirMsg.channelFlags & 0x01) == 0 => acceptIncomingChannel(theirMsg)
      case _: OpenChannel => UITask(WalletApp.app quickToast error_rejected_incoming_public).run
      case _ => // Do nothing
    }
  }

  private lazy val incomingIgnoringListener = new DisconnectListener

  private lazy val viewUpdatingListener = new ConnectionListener {
    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = UITask {
      val theirInitSupports: Feature => Boolean = LNParams.isPeerSupports(theirInit)
      val criticalSupportAvailable = criticalFeatures.forall(theirInitSupports)

      featureTextViewMap foreach {
        case (feature, view) if theirInitSupports(feature) =>
          view.setBackgroundResource(R.drawable.border_green)
          view.setText(feature.rfcName)

        case (feature, view) if criticalFeatures.contains(feature) =>
          view.setBackgroundResource(R.drawable.border_red)
          view.setText(feature.rfcName)

        case (feature, view) =>
          view.setBackgroundResource(R.drawable.border_gray)
          view.setText(feature.rfcName)
      }

      hasInfo match {
        case nc: NormalChannelRequest if criticalSupportAvailable => nc.requestChannel.foreach(none, revertAndInform)
        case hc: HostedChannelRequest if criticalSupportAvailable && theirInitSupports(HostedChannels) => askHostedChannel(hc.secret)
        case _: HasRemoteInfoWrap => switchView(showProgress = false)
        case _ => whenBackPressed.run
      }

      setVis(isVisible = !criticalSupportAvailable, viewNoFeatureSupport)
      setVis(isVisible = criticalSupportAvailable, viewYesFeatureSupport)
      setVis(isVisible = theirInitSupports(HostedChannels), optionHostedChannel)
    }.run
  }

  private var whenBackPressed: Runnable = UITask(finish)
  private var hasInfo: HasRemoteInfo = _

  def activateInfo(info: HasRemoteInfo): Unit = {
    peerNodeKey.setText(info.remoteInfo.nodeId.toString.take(16).humanFour)
    peerIpAddress.setText(info.remoteInfo.address.toString)
    hasInfo = info

    whenBackPressed = UITask {
      // Disconnect and clear up if anything goes wrong
      CommsTower.disconnectNative(info.remoteInfo)
      info.cancel
    }

    // Try to connect after assigning vars since listener may fire immediately
    val listeners = Set(viewUpdatingListener, incomingAcceptingListener)
    CommsTower.listenNative(listeners, info.remoteInfo)
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case remoteInfo: RemoteNodeInfo => me activateInfo HasRemoteInfoWrap(remoteInfo)
    case normalChannel: NormalChannelRequest => me activateInfo normalChannel
    case hostedChannel: HostedChannelRequest => me activateInfo hostedChannel
    case _ => whenNone.run
  }

  override def onBackPressed: Unit =
    whenBackPressed.run

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_remote_peer)
      checkExternalData(whenBackPressed)
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  // BUTTON ACTIONS

  def acceptIncomingChannel(theirOpen: OpenChannel): Unit = {
    new NCFundeeOpenHandler(hasInfo.remoteInfo, theirOpen, LNParams.cm) {
      override def onEstablished(cs: Commitments, chan: ChannelNormal): Unit = implant(cs, chan)
      override def onFailure(reason: Throwable): Unit = revertAndInform(reason)
    }

    switchView(showProgress = true)
    stopAcceptingIncomingOffers
  }

  def fundNewChannel(view: View): Unit = {
    val sendView = new ChainSendView
    val manager = new RateManager(sendView.body, extraText = None, visHintRes = -1, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
    val canSend = WalletApp.denom.parsedWithSign(LNParams.chainWallets.lnWallet.info.lastBalance.toMilliSatoshi, cardIn, cardZero)
    val canSendFiat = WalletApp.currentMsatInFiatHuman(LNParams.chainWallets.lnWallet.info.lastBalance.toMilliSatoshi)

    def attempt(alert: AlertDialog): Unit = {
      runFutureProcessOnUI(NCFunderOpenHandler.makeFunding(LNParams.chainWallets, manager.resultMsat.truncateToSatoshi, feeView.rate), onFail) { response =>
        sendView.switchToConfirm(alert, response.toTxOut.amount.toMilliSatoshi, response.fee.toMilliSatoshi)
        sendView.confirmEdit setOnClickListener onButtonTap(sendView switchToEdit alert)
        sendView.cancelSend setOnClickListener onButtonTap(alert.dismiss)
        sendView.confirmSend setOnClickListener onButtonTap(proceed)
      }

      def proceed: Unit = {
        NCFunderOpenHandler.makeFunding(LNParams.chainWallets, manager.resultMsat.truncateToSatoshi, feeView.rate) foreach { fakeFunding =>
          new NCFunderOpenHandler(hasInfo.remoteInfo, fakeFunding, feeView.rate, LNParams.chainWallets, LNParams.cm) {
            override def onEstablished(cs: Commitments, chan: ChannelNormal): Unit = implant(cs, chan)
            override def onFailure(reason: Throwable): Unit = revertAndInform(reason)
          }
        }

        switchView(showProgress = true)
        stopAcceptingIncomingOffers
        alert.dismiss
      }
    }

    lazy val alert = {
      def setMax(alert1: AlertDialog): Unit = manager.updateText(LNParams.chainWallets.lnWallet.info.lastBalance.toMilliSatoshi)
      val builder = titleBodyAsViewBuilder(getString(rpa_open_nc).asColoredView(R.color.cardBitcoinModern), manager.content)
      mkCheckFormNeutral(attempt, none, setMax, builder, dialog_ok, dialog_cancel, dialog_max)
    }

    lazy val feeView: FeeView[GenerateTxResponse] = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.body) {
      rate = LNParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(LNParams.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget)

      worker = new ThrottledWork[String, GenerateTxResponse] {
        def work(reason: String): Observable[GenerateTxResponse] = Rx fromFutureOnIo NCFunderOpenHandler.makeFunding(LNParams.chainWallets, manager.resultMsat.truncateToSatoshi, rate)
        def process(reason: String, result: GenerateTxResponse): Unit = update(feeOpt = result.fee.toMilliSatoshi.asSome, showIssue = false)
        override def error(exc: Throwable): Unit = update(feeOpt = None, showIssue = manager.resultMsat >= LNParams.minChanDustLimit)
      }

      override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
        updatePopupButton(getPositiveButton(alert), feeOpt.isDefined)
        super.update(feeOpt, showIssue)
      }.run
    }

    manager.hintDenom.setText(getString(dialog_up_to).format(canSend).html)
    manager.hintFiatDenom.setText(getString(dialog_up_to).format(canSendFiat).html)
    manager.inputAmount addTextChangedListener onTextChange(feeView.worker.addWork)
    feeView.update(feeOpt = None, showIssue = false)
  }

  def sharePeerSpecificNodeId(view: View): Unit = share(hasInfo.remoteInfo.nodeSpecificPubKey.toString)
  def requestHostedChannel(view: View): Unit = askHostedChannel(randomBytes32)

  def askHostedChannel(secret: ByteVector32): Unit = {
    val builder = new AlertDialog.Builder(me).setTitle(rpa_request_hc).setMessage(getString(rpa_hc_warn).html)
    mkCheckForm(doAskHostedChannel, none, builder, dialog_ok, dialog_cancel)

    def doAskHostedChannel(alert: AlertDialog): Unit = {
      // Switch view first since HC may throw immediately
      switchView(showProgress = true)
      stopAcceptingIncomingOffers
      alert.dismiss

      // We only need local params to extract defaultFinalScriptPubKey
      val localParams = LNParams.makeChannelParams(LNParams.chainWallets, isFunder = false, LNParams.minChanDustLimit)
      new HCOpenHandler(hasInfo.remoteInfo, secret, localParams.defaultFinalScriptPubKey, LNParams.cm) {
        def onEstablished(cs: Commitments, channel: ChannelHosted): Unit = implant(cs, channel)
        def onFailure(reason: Throwable): Unit = revertAndInform(reason)
      }
    }
  }

  def switchView(showProgress: Boolean): Unit = UITask {
    setVis(isVisible = !showProgress, peerDetails)
    setVis(isVisible = showProgress, progressBar)
  }.run

  def revertAndInform(reason: Throwable): Unit = {
    // Whatever the reason for this to happen we still may accept new offers
    CommsTower.listenNative(Set(incomingAcceptingListener), hasInfo.remoteInfo)
    switchView(showProgress = false)
    onFail(reason)
  }

  def stopAcceptingIncomingOffers: Unit = {
    CommsTower.listenNative(Set(incomingIgnoringListener), hasInfo.remoteInfo)
    CommsTower.rmListenerNative(hasInfo.remoteInfo, incomingAcceptingListener)
  }

  def implant(cs: Commitments, freshChannel: Channel): Unit = {
    RemotePeerActivity.implantNewChannel(cs, freshChannel)
    disconnectListenersAndFinish
  }

  def disconnectListenersAndFinish: Unit = {
    CommsTower.rmListenerNative(hasInfo.remoteInfo, incomingAcceptingListener)
    CommsTower.rmListenerNative(hasInfo.remoteInfo, incomingIgnoringListener)
    CommsTower.rmListenerNative(hasInfo.remoteInfo, viewUpdatingListener)
    finish
  }
}
