package com.btcontract.wallet

import java.net.InetSocketAddress
import java.util.TimerTask

import android.content.Intent
import android.content.pm.PackageManager
import android.os.Bundle
import android.view.{View, ViewGroup}
import android.widget._
import androidx.appcompat.app.AlertDialog
import androidx.recyclerview.widget.RecyclerView
import com.btcontract.wallet.BaseActivity.StringOps
import com.btcontract.wallet.Colors._
import com.btcontract.wallet.HubActivity._
import com.btcontract.wallet.R.string._
import com.chauthai.swipereveallayout.{SwipeRevealLayout, ViewBinderHelper}
import com.danilomendes.progressbar.InvertedTextProgressbar
import com.github.mmin18.widget.RealtimeBlurView
import com.ornach.nobobutton.NoboButton
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{GenerateTxResponse, RBFResponse, WalletReady}
import fr.acinq.eclair.blockchain.electrum.{ElectrumEclairWallet, ElectrumWallet}
import fr.acinq.eclair.blockchain.fee.FeeratePerByte
import immortan._
import immortan.crypto.Tools._
import immortan.sqlite.DbStreams
import immortan.utils.ImplicitJsonFormats._
import immortan.utils._
import org.apmem.tools.layouts.FlowLayout
import rx.lang.scala.{Observable, Subscription}
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}


object HubActivity {
  var txInfos: Iterable[TxInfo] = Nil
  var allInfos: Seq[TransactionDetails] = Nil
  var instance: HubActivity = _

  def incoming(amount: MilliSatoshi): String =
    WalletApp.denom.directedWithSign(amount, 0L.msat,
      cardOut, cardIn, cardZero, isIncoming = true)
}

class HubActivity extends BaseActivity with ExternalDataChecker { me =>
  private[this] lazy val paymentTypeIconIds = List(R.id.btcIncoming, R.id.btcInBoosted, R.id.btcOutBoosted, R.id.btcOutCancelled, R.id.btcOutgoing)
  private[this] lazy val bottomBlurringArea = findViewById(R.id.bottomBlurringArea).asInstanceOf[RealtimeBlurView]
  private[this] lazy val bottomActionBar = findViewById(R.id.bottomActionBar).asInstanceOf[LinearLayout]
  private[this] lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]
  private[this] lazy val itemsList = findViewById(R.id.itemsList).asInstanceOf[ListView]

  lazy val walletCards = new WalletCardsViewHolder
  private[this] val viewBinderHelper = new ViewBinderHelper
  var openListItems = Set.empty[String]

  // PAYMENT LIST

  def loadRecentTxInfos: Unit = txInfos = WalletApp.txDataBag.listRecentTxs(50).map(WalletApp.txDataBag.toTxInfo)
  def loadSearchedTxInfos(query: String): Unit = txInfos = WalletApp.txDataBag.searchTransactions(query).map(WalletApp.txDataBag.toTxInfo)
  def fillAllInfos: Unit = allInfos = SemanticOrder.makeSemanticOrder(WalletApp.txInfos.values.toSeq ++ txInfos)

  def loadRecent: Unit = {
    loadRecentTxInfos
    fillAllInfos
  }

  def loadSearch(query: String): Unit = {
    loadSearchedTxInfos(query)
    fillAllInfos
  }

  val searchWorker: ThrottledWork[String, Unit] = new ThrottledWork[String, Unit] {
    def work(query: String): Observable[Unit] = Rx.ioQueue.map(_ => if (query.nonEmpty) loadSearch(query) else loadRecent)
    def process(userTypedQuery: String, searchLoadResultEffect: Unit): Unit = paymentAdapterDataChanged.run
  }

  val paymentsAdapter: BaseAdapter = new BaseAdapter {
    override def getItem(pos: Int): TransactionDetails = allInfos(pos)
    override def getItemId(position: Int): Long = position
    override def getCount: Int = allInfos.size

    override def getView(position: Int, savedView: View, parent: ViewGroup): View = getItem(position) match { case item =>
      val view = if (null == savedView) getLayoutInflater.inflate(R.layout.frag_payment_line, null) else savedView.asInstanceOf[View]
      val holder = if (null == view.getTag) new PaymentLineViewHolder(view) else view.getTag.asInstanceOf[PaymentLineViewHolder]
      // At first we always reset these properties, each component may later change them as it sees fit
      viewBinderHelper.bind(holder.swipeWrap, item.identity)
      holder.swipeWrap.setLockDrag(true)
      view.setAlpha(1F)

      if (openListItems contains item.identity) holder.expand(item) else holder.collapse(item)
      setVisMany(item.isExpandedItem -> holder.spacer, !item.isExpandedItem -> holder.spacer1)
      holder.currentDetails = item
      holder.updateDetails
      view
    }
  }

  class PaymentLineViewHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) { self =>
    val extraInfo: FlowLayout = itemView.findViewById(R.id.extraInfo).asInstanceOf[FlowLayout]
    val swipeWrap: SwipeRevealLayout = itemView.asInstanceOf[SwipeRevealLayout]

    val spacer: View = swipeWrap.findViewById(R.id.spacer)
    val spacer1: View = swipeWrap.findViewById(R.id.spacer1)

    val paymentCardContainer: View = swipeWrap.findViewById(R.id.paymentCardContainer)
    val setItemLabel: NoboButton = swipeWrap.findViewById(R.id.setItemLabel).asInstanceOf[NoboButton]
    val shareItem: NoboButton = swipeWrap.findViewById(R.id.shareItem).asInstanceOf[NoboButton]

    val nonLinkContainer: LinearLayout = swipeWrap.findViewById(R.id.nonLinkContainer).asInstanceOf[LinearLayout]
    val detailsAndStatus: RelativeLayout = swipeWrap.findViewById(R.id.detailsAndStatus).asInstanceOf[RelativeLayout]
    val description: TextView = swipeWrap.findViewById(R.id.description).asInstanceOf[TextView]
    val statusIcon: ImageView = swipeWrap.findViewById(R.id.statusIcon).asInstanceOf[ImageView]
    val labelIcon: ImageView = swipeWrap.findViewById(R.id.labelIcon).asInstanceOf[ImageView]
    val amount: TextView = swipeWrap.findViewById(R.id.amount).asInstanceOf[TextView]
    val meta: TextView = swipeWrap.findViewById(R.id.meta).asInstanceOf[TextView]

    spacer1.setZ(Float.MaxValue)
    itemView.setTag(this)

    val paymentTypeIconViews: List[View] = paymentTypeIconIds.map(swipeWrap.findViewById)
    val iconMap: Map[Int, View] = paymentTypeIconIds.zip(paymentTypeIconViews).toMap
    var currentDetails: TransactionDetails = _
    var lastVisibleIconId: Int = -1

    paymentCardContainer setOnClickListener onButtonTap(ractOnTap)
    setItemLabel setOnClickListener onButtonTap(doSetItemLabel)
    shareItem setOnClickListener onButtonTap(doShareItem)

    // MENU BUTTONS

    def doSetItemLabel: Unit = {
      val (container, extraInputLayout, extraInput) = singleInputPopup
      val builder = titleBodyAsViewBuilder(title = null, body = container)
      mkCheckForm(proceed, none, builder, dialog_ok, dialog_cancel)
      extraInputLayout.setHint(dialog_set_label)
      showKeys(extraInput)

      def proceed(alert: AlertDialog): Unit = runAnd(alert.dismiss) {
        Some(currentDetails).collectFirst { case chainTxInfo: TxInfo =>
          val optionalInput = Option(extraInput.getText.toString).map(trimmed).filter(_.nonEmpty)
          val description = chainTxInfo.description.withNewLabel(optionalInput)
          WalletApp.txDataBag.updDescription(description, chainTxInfo.txid)
        }
      }
    }

    def doShareItem: Unit = Some(currentDetails).collectFirst { case chainTxInfo: TxInfo =>
      me share getString(share_chain_tx).format(chainTxInfo.txString)
    }

    def ractOnTap: Unit = {
        val isVisible = extraInfo.getVisibility == View.VISIBLE
        if (isVisible) collapse(currentDetails) else expand(currentDetails)
    }

    // CPFP / RBF

    def boostCPFP(info: TxInfo): Unit = WalletParams.chainWallets.findByPubKey(info.pubKey) match {
      case None => snack(contentWindow, getString(error_btc_no_wallet).html, dialog_ok, _.dismiss)
      case Some(fromWallet) => doBoostCPFP(fromWallet, info)
    }

    def doBoostCPFP(fromWallet: ElectrumEclairWallet, info: TxInfo): Unit = {
      val fromOutPoints = for (txOutputIndex <- info.tx.txOut.indices) yield OutPoint(info.tx.hash, txOutputIndex)
      val chainAddress = Await.result(fromWallet.getReceiveAddresses, 30.seconds).firstAccountAddress
      val chainPubKeyScript = WalletParams.addressToPubKeyScript(chainAddress)
      val receivedMsat = info.receivedSat.toMilliSatoshi

      val sendView = new ChainSendView(fromWallet, badge = None, visibilityRes = -1)
      val blockTarget = WalletParams.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val target = WalletParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(target), sendView.cpfpView.host) {
        rate = target

        worker = new ThrottledWork[String, GenerateTxResponse] {
          def work(reason: String): Observable[GenerateTxResponse] = Rx fromFutureOnIo fromWallet.makeCPFP(fromOutPoints.toSet, chainPubKeyScript, rate)
          def process(reason: String, response: GenerateTxResponse): Unit = update(feeOpt = response.fee.toMilliSatoshi.asSome, showIssue = false)
          override def error(exc: Throwable): Unit = update(feeOpt = None, showIssue = true)
        }

        override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
          val currentAmount = WalletApp.denom.directedWithSign(incoming = receivedMsat, outgoing = 0L.msat, cardOut, cardIn, cardZero, isIncoming = true)
          val afterAmount = WalletApp.denom.directedWithSign(feeOpt.map(receivedMsat.-).getOrElse(receivedMsat), 0L.msat, cardOut, cardIn, cardZero, isIncoming = true)
          sendView.cpfpView.cpfpCurrent.secondItem.setText(currentAmount.html)
          sendView.cpfpView.cpfpAfter.secondItem.setText(afterAmount.html)
          updatePopupButton(getPositiveButton(alert), feeOpt.isDefined)
          super.update(feeOpt, showIssue)
        }.run
      }

      def attempt(alert: AlertDialog): Unit = {
        val cpfpBumpOrder = SemanticOrder(info.txid.toHex, System.currentTimeMillis)
        // Only update parent semantic order if it does not already have one, record it BEFORE sending CPFP
        val parentDescWithOrder = info.description.withNewOrderCond(cpfpBumpOrder.copy(order = Long.MinValue).asSome)
        WalletApp.txDataBag.updDescription(parentDescWithOrder, info.txid)

        for {
          check <- fromWallet.doubleSpent(info.tx) if check.depth < 1 && !check.isDoubleSpent
        } runFutureProcessOnUI(fromWallet.makeCPFP(fromOutPoints.toSet, chainPubKeyScript, feeView.rate), onFail) { response =>
          // At this point we have received some response, in this case it can not be a failure but then maybe hardware wallet

          proceedWithoutConfirm(fromWallet, sendView, alert, response) { signedTx =>
            val desc = PlainTxDescription(chainAddress :: Nil, label = None, cpfpBumpOrder.asSome, cpfpBy = None, cpfpOf = info.txid.asSome)
            val isSent = Await.result(broadcastTx(fromWallet, desc, signedTx, response.transferred, 0.sat, response.fee), 30.seconds)

            if (isSent) {
              // Parent semantic order has already been updated, now we also must update CPFP parent info
              WalletApp.txDataBag.updDescription(parentDescWithOrder.withNewCPFPBy(signedTx.txid), info.txid)
            } else {
              // We revert the whole description back since CPFP has failed
              WalletApp.txDataBag.updDescription(info.description, info.txid)
              cleanFailedBroadcast(signedTx.txid)
            }
          }
        }
      }

      lazy val alert = {
        val title = getString(tx_cpfp_explain)
        val builder = titleBodyAsViewBuilder(title.asDefView, sendView.body)
        mkCheckForm(attempt, none, builder, dialog_ok, dialog_cancel)
      }

      feeView.update(feeOpt = None, showIssue = false)
      feeView.customFeerateOption.performClick
      sendView.defaultView = sendView.cpfpView
      sendView.switchToDefault(alert)
    }

    def boostRBF(info: TxInfo): Unit = WalletParams.chainWallets.findByPubKey(info.pubKey) match {
      case None => snack(contentWindow, getString(error_btc_no_wallet).html, dialog_ok, _.dismiss)
      case Some(fromWallet) => doBoostRBF(fromWallet, info)
    }

    def doBoostRBF(fromWallet: ElectrumEclairWallet, info: TxInfo): Unit = {
      val currentFee = WalletApp.denom.parsedWithSign(info.feeSat.toMilliSatoshi, cardOut, cardIn)

      val sendView = new ChainSendView(fromWallet, badge = None, visibilityRes = -1)
      val blockTarget = WalletParams.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val target = WalletParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView: FeeView[RBFResponse] = new FeeView[RBFResponse](FeeratePerByte(target), sendView.rbfView.host) {
        rate = target

        worker = new ThrottledWork[String, RBFResponse] {
          def process(reason: String, response: RBFResponse): Unit = response.result match {
            case Left(ElectrumWallet.PARENTS_MISSING) => showRbfErrorDesc(tx_rbf_err_parents_missing)
            case Left(ElectrumWallet.FOREIGN_INPUTS) => showRbfErrorDesc(tx_rbf_err_foreign_inputs)
            case Left(ElectrumWallet.RBF_DISABLED) => showRbfErrorDesc(tx_rbf_err_rbf_disabled)
            case Right(res) => update(res.fee.toMilliSatoshi.asSome, showIssue = false)
            case _ => error(new RuntimeException)
          }

          def work(reason: String): Observable[RBFResponse] = Rx fromFutureOnIo fromWallet.makeRBFBump(info.tx, rate)
          override def error(exc: Throwable): Unit = update(feeOpt = None, showIssue = true)
        }

        private def showRbfErrorDesc(descRes: Int): Unit = UITask {
          updatePopupButton(getPositiveButton(alert), isEnabled = false)
          super.update(feeOpt = Option.empty, showIssue = false)
          setVis(isVisible = true, sendView.rbfView.rbfIssue)
          sendView.rbfView.rbfIssue.setText(descRes)
        }.run

        override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
          updatePopupButton(getPositiveButton(alert), isEnabled = feeOpt.isDefined)
          setVis(isVisible = false, sendView.rbfView.rbfIssue)
          super.update(feeOpt, showIssue)
        }.run
      }

      def attempt(alert: AlertDialog): Unit = {
        val rbfParams = RBFParams(info.txid, TxDescription.RBF_BOOST)
        val rbfBumpOrder = SemanticOrder(info.txid.toHex, -System.currentTimeMillis)

        for {
          check <- fromWallet.doubleSpent(info.tx) if check.depth < 1 && !check.isDoubleSpent
        } runFutureProcessOnUI(fromWallet.makeRBFBump(info.tx, feeView.rate), onFail) { responseWrap =>
          // At this point we have received some response, it may be a failre and then maybe hardware wallet

          responseWrap.result.right.toOption.foreach { response =>
            proceedWithoutConfirm(fromWallet, sendView, alert, response) { signedTx =>
              val desc = PlainTxDescription(Nil, label = None, rbfBumpOrder.asSome, cpfpBy = None, cpfpOf = None, rbfParams.asSome)
              val isSent = Await.result(broadcastTx(fromWallet, desc, signedTx, 0.sat, info.sentSat, response.fee), 30.seconds)

              if (isSent) {
                val parentLowestOrder = rbfBumpOrder.copy(order = Long.MaxValue)
                val parentDesc = info.description.withNewOrderCond(parentLowestOrder.asSome)
                WalletApp.txDataBag.updDescription(parentDesc, info.txid)
              } else cleanFailedBroadcast(signedTx.txid)
            }
          }
        }
      }

      lazy val alert = {
        val title = getString(tx_rbf_boost_explain)
        val builder = titleBodyAsViewBuilder(title.asDefView, sendView.body)
        mkCheckForm(attempt, none, builder, dialog_ok, dialog_cancel)
      }

      sendView.rbfView.rbfCurrent.secondItem.setText(currentFee.html)
      feeView.update(feeOpt = Option.empty, showIssue = false)
      feeView.customFeerateOption.performClick
      sendView.defaultView = sendView.rbfView
      sendView.switchToDefault(alert)
    }

    def cancelRBF(info: TxInfo): Unit = WalletParams.chainWallets.findByPubKey(info.pubKey) match {
      case None => snack(contentWindow, getString(error_btc_no_wallet).html, dialog_ok, _.dismiss)
      case Some(fromWallet) => doCancelRBF(fromWallet, info)
    }

    def doCancelRBF(fromWallet: ElectrumEclairWallet, info: TxInfo): Unit = {
      val changeKey = Await.result(fromWallet.getReceiveAddresses, 30.seconds)
      val changePks = WalletParams.addressToPubKeyScript(changeKey.changeAddress)
      val currentFee = WalletApp.denom.parsedWithSign(info.feeSat.toMilliSatoshi, cardOut, cardIn)

      val sendView = new ChainSendView(fromWallet, badge = None, visibilityRes = -1)
      val blockTarget = WalletParams.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val target = WalletParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView: FeeView[RBFResponse] = new FeeView[RBFResponse](FeeratePerByte(target), sendView.rbfView.host) {
        rate = target

        worker = new ThrottledWork[String, RBFResponse] {
          def process(reason: String, response: RBFResponse): Unit = response.result match {
            case Left(ElectrumWallet.PARENTS_MISSING) => showRbfErrorDesc(tx_rbf_err_parents_missing)
            case Left(ElectrumWallet.FOREIGN_INPUTS) => showRbfErrorDesc(tx_rbf_err_foreign_inputs)
            case Left(ElectrumWallet.RBF_DISABLED) => showRbfErrorDesc(tx_rbf_err_rbf_disabled)
            case Right(res) => update(res.fee.toMilliSatoshi.asSome, showIssue = false)
            case _ => error(new RuntimeException)
          }

          def work(reason: String): Observable[RBFResponse] = Rx fromFutureOnIo fromWallet.makeRBFReroute(info.tx, rate, changePks)
          override def error(exception: Throwable): Unit = update(feeOpt = None, showIssue = true)
        }

        private def showRbfErrorDesc(descRes: Int): Unit = UITask {
          updatePopupButton(getPositiveButton(alert), isEnabled = false)
          super.update(feeOpt = Option.empty, showIssue = false)
          setVis(isVisible = true, sendView.rbfView.rbfIssue)
          sendView.rbfView.rbfIssue.setText(descRes)
        }.run

        override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
          updatePopupButton(getPositiveButton(alert), isEnabled = feeOpt.isDefined)
          setVis(isVisible = false, sendView.rbfView.rbfIssue)
          super.update(feeOpt, showIssue)
        }.run
      }

      def attempt(alert: AlertDialog): Unit = {
        val rbfParams = RBFParams(info.txid, TxDescription.RBF_CANCEL)
        val rbfBumpOrder = SemanticOrder(info.txid.toHex, -System.currentTimeMillis)

        for {
          check <- fromWallet.doubleSpent(info.tx) if check.depth < 1 && !check.isDoubleSpent
        } runFutureProcessOnUI(fromWallet.makeRBFReroute(info.tx, feeView.rate, changePks), onFail) { responseWrap =>
          // At this point we have received some response, it may be a failre and then maybe hardware wallet

          responseWrap.result.right.toOption.foreach { response =>
            proceedWithoutConfirm(fromWallet, sendView, alert, response) { signedTx =>
              val desc = PlainTxDescription(addresses = Nil, None, rbfBumpOrder.asSome, None, None, rbfParams.asSome)
              val isSent = Await.result(broadcastTx(fromWallet, desc, signedTx, 0.sat, response.fee, response.fee), 30.seconds)

              if (isSent) {
                val parentLowestOrder = rbfBumpOrder.copy(order = Long.MaxValue)
                val parentDesc = info.description.withNewOrderCond(parentLowestOrder.asSome)
                WalletApp.txDataBag.updDescription(parentDesc, info.txid)
              } else cleanFailedBroadcast(signedTx.txid)
            }
          }
        }
      }

      lazy val alert = {
        val title = getString(tx_rbf_cancel_explain)
        val builder = titleBodyAsViewBuilder(title.asDefView, sendView.body)
        mkCheckForm(attempt, none, builder, dialog_ok, dialog_cancel)
      }

      sendView.rbfView.rbfCurrent.secondItem.setText(currentFee.html)
      feeView.update(feeOpt = Option.empty, showIssue = false)
      feeView.customFeerateOption.performClick
      sendView.defaultView = sendView.rbfView
      sendView.switchToDefault(alert)
    }

    // VIEW RELATED

    def collapse[T <: TransactionDetails](item: T): Unit = {
      setVis(isVisible = false, extraInfo)
      extraInfo.removeAllViewsInLayout
      openListItems -= item.identity
      description.setMaxLines(1)
    }

    def expand[T <: TransactionDetails](item: T): Unit = {
      setVis(isVisible = true, extraInfo)
      extraInfo.removeAllViewsInLayout
      openListItems += item.identity
      description.setMaxLines(3)

      item match {
        case info: TxInfo =>
          val amount = if (info.isIncoming) info.receivedSat.toMilliSatoshi else info.sentSat.toMilliSatoshi
          val wallet = WalletParams.chainWallets.findByPubKey(info.pubKey).find(wallet => wallet.isSigning || wallet.info.core.masterFingerprint.nonEmpty)
          val canRBF = !info.isIncoming && !info.isDoubleSpent && info.depth < 1 && info.description.rbf.isEmpty && info.description.cpfpOf.isEmpty
          val canCPFP = info.isIncoming && !info.isDoubleSpent && info.depth < 1 && info.description.rbf.isEmpty && info.description.canBeCPFPd
          val isRbfCancel = info.description.rbf.exists(_.mode == TxDescription.RBF_CANCEL)

          val fee = WalletApp.denom.directedWithSign(0L.msat, info.feeSat.toMilliSatoshi, cardOut, cardIn, cardZero, isIncoming = false)
          val fiatNow = WalletApp.msatInFiatHuman(WalletParams.fiatRates.info.rates, WalletApp.fiatCode, amount, Denomination.formatFiat)
          val fiatThen = WalletApp.msatInFiatHuman(info.fiatRateSnapshot, WalletApp.fiatCode, amount, Denomination.formatFiat)

          wallet match {
            case Some(w) if w.info.core.attachedMaster.isDefined => addFlowChip(extraInfo, getString(attached_wallet), R.drawable.border_gray)
            case Some(w) if w.info.core.masterFingerprint.nonEmpty => addFlowChip(extraInfo, getString(hardware_wallet), R.drawable.border_gray)
            case Some(w) if !w.isSigning => addFlowChip(extraInfo, getString(watching_wallet), R.drawable.border_gray)
            case _ =>
          }

          addFlowChip(extraInfo, getString(popup_txid) format info.txidString.short, R.drawable.border_green, info.txidString.asSome)
          for (address <- info.description.toAddress) addFlowChip(extraInfo, getString(popup_to_address) format address.short, R.drawable.border_yellow, address.asSome)

          addFlowChip(extraInfo, getString(popup_fiat).format(s"<font color=$cardIn>$fiatNow</font>", fiatThen), R.drawable.border_gray)
          if (!info.isIncoming || isRbfCancel || info.description.cpfpOf.isDefined) addFlowChip(extraInfo, getString(popup_chain_fee) format fee, R.drawable.border_gray)

          if (wallet.isDefined) {
            if (canCPFP) addFlowChip(extraInfo, getString(dialog_boost), R.drawable.border_yellow, _ => self boostCPFP info)
            if (canRBF) addFlowChip(extraInfo, getString(dialog_cancel), R.drawable.border_yellow, _ => self cancelRBF info)
            if (canRBF) addFlowChip(extraInfo, getString(dialog_boost), R.drawable.border_yellow, _ => self boostRBF info)
          }
      }
    }

    def updateDetails: Unit = currentDetails match {
      // Reusing the same view to speed the list up

      case info: TxInfo =>
        // We are not sure if this one has been broadcasted yet
        val isEphemeral = WalletApp.txInfos.contains(info.txid)
        if (!isEphemeral) swipeWrap.setLockDrag(false)
        if (isEphemeral) itemView.setAlpha(0.7F)

        statusIcon setImageResource txStatusIcon(info)
        nonLinkContainer setBackgroundResource R.drawable.border_gray
        setVisMany(info.description.label.isDefined -> labelIcon, true -> detailsAndStatus, true -> nonLinkContainer)
        amount.setText(WalletApp.denom.directedWithSign(info.receivedSat.toMilliSatoshi, info.sentSat.toMilliSatoshi, cardOut, cardIn, cardZero, info.isIncoming).html)
        description.setText(info.description.label getOrElse txDescription(info).html)
        setTxTypeIcon(info)
        setTxMeta(info)
      }

    def setVisibleIcon(id: Int): Unit = if (lastVisibleIconId != id) {
      iconMap.get(lastVisibleIconId).foreach(_ setVisibility View.GONE)
      iconMap.get(id).foreach(_ setVisibility View.VISIBLE)
      lastVisibleIconId = id
    }

    // TX helpers

    def txDescription(transactionInfo: TxInfo): String = transactionInfo.description match {
      case _ if transactionInfo.description.cpfpOf.isDefined => getString(tx_description_cpfp)
      case _ if transactionInfo.description.rbf.exists(_.mode == TxDescription.RBF_BOOST) => getString(tx_description_rbf_boost)
      case _ if transactionInfo.description.rbf.exists(_.mode == TxDescription.RBF_CANCEL) => getString(tx_description_rbf_cancel)
      case plain: PlainTxDescription => plain.toAddress.map(_.short) getOrElse getString(tx_btc)
    }

    def setTxTypeIcon(info: TxInfo): Unit = info.description match {
      case _ if info.description.cpfpOf.isDefined => setVisibleIcon(id = R.id.btcInBoosted)
      case _ if info.description.rbf.exists(_.mode == TxDescription.RBF_BOOST) => setVisibleIcon(id = R.id.btcOutBoosted)
      case _ if info.description.rbf.exists(_.mode == TxDescription.RBF_CANCEL) => setVisibleIcon(id = R.id.btcOutCancelled)
      case _: PlainTxDescription if info.isIncoming => setVisibleIcon(id = R.id.btcIncoming)
      case _ => setVisibleIcon(id = R.id.btcOutgoing)
    }

    def txStatusIcon(info: TxInfo): Int = {
      if (info.isConfirmed) R.drawable.baseline_done_24
      else if (info.isDoubleSpent) R.drawable.baseline_block_24
      else R.drawable.baseline_hourglass_empty_24
    }

    def setTxMeta(info: TxInfo): Unit = {
      if (info.isDoubleSpent) meta setText getString(tx_state_double_spent).html
      else if (info.depth > 0) meta setText WalletApp.app.when(info.date, WalletApp.app.dateFormat).html
      else meta setText getString(tx_state_pending).html
    }
  }

  // LIST CAPTION CLASS

  class WalletCardsViewHolder {
    val view: LinearLayout = getLayoutInflater.inflate(R.layout.frag_wallet_cards, null).asInstanceOf[LinearLayout]
    val fiatUnitPriceAndChange: TextView = view.findViewById(R.id.fiatUnitPriceAndChange).asInstanceOf[TextView]
    val recoveryPhraseWarning: TextView = view.findViewById(R.id.recoveryPhraseWarning).asInstanceOf[TextView]
    val defaultHeader: LinearLayout = view.findViewById(R.id.defaultHeader).asInstanceOf[LinearLayout]

    val offlineIndicator: TextView = view.findViewById(R.id.offlineIndicator).asInstanceOf[TextView]
    val chainSyncIndicator: InvertedTextProgressbar = view.findViewById(R.id.chainSyncIndicator).asInstanceOf[InvertedTextProgressbar]
    val torIndicator: TextView = view.findViewById(R.id.torIndicator).asInstanceOf[TextView]
    val searchField: EditText = view.findViewById(R.id.searchField).asInstanceOf[EditText]
    // This means search is off at start
    searchField.setTag(false)

    val chainCards: ChainWalletCards = new ChainWalletCards(me) {
      val holder: LinearLayout = view.findViewById(R.id.chainCardsContainer).asInstanceOf[LinearLayout]
      override def onCoinControlTap(wallet: ElectrumEclairWallet): Unit = goToWithValue(ClassNames.coinControlActivityClass, wallet)
      override def onWalletTap(wallet: ElectrumEclairWallet): Unit = goToWithValue(ClassNames.qrChainActivityClass, wallet)

      override def onLabelTap(wallet: ElectrumEclairWallet): Unit = {
        val (container, extraInputLayout, extraInput) = singleInputPopup
        val builder = titleBodyAsViewBuilder(title = null, body = container)
        mkCheckForm(proceed, none, builder, dialog_ok, dialog_cancel)
        extraInputLayout.setHint(dialog_set_label)
        showKeys(extraInput)

        def proceed(alert: AlertDialog): Unit = runAnd(alert.dismiss) {
          val withnewLabel: ElectrumEclairWallet => WalletExt = WalletParams.chainWallets.withNewLabel(extraInput.getText.toString)
          WalletParams.chainWallets.findByPubKey(wallet.ewt.xPub.publicKey).map(withnewLabel).foreach(resetChainCards)
        }
      }

      override def onRemoveTap(wallet: ElectrumEclairWallet): Unit = {
        def proceed: Unit = WalletParams.chainWallets.findByPubKey(wallet.ewt.xPub.publicKey).map(WalletParams.chainWallets.withoutWallet).foreach(resetChainCards)
        mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, new AlertDialog.Builder(me).setMessage(confirm_remove_item), dialog_ok, dialog_cancel)
      }
    }

    def resetChainCards(ext1: WalletExt): Unit = {
      // Remove all existing cards and place new ones
      WalletParams.synchronized(WalletParams.chainWallets = ext1)
      chainCards.holder.removeAllViewsInLayout
      chainCards.init(ext1.wallets)
      updateView
    }

    def updateView: Unit = {
      androidx.transition.TransitionManager.beginDelayedTransition(defaultHeader)
      val change = WalletParams.fiatRates.info.pctDifference(WalletApp.fiatCode).getOrElse(new String)
      val unitRate = WalletApp.msatInFiatHuman(WalletParams.fiatRates.info.rates, WalletApp.fiatCode, 100000000000L.msat, Denomination.formatFiatShort)
      fiatUnitPriceAndChange.setText(s"$unitRate $change".html)
      chainCards.update(WalletParams.chainWallets.wallets)
    }
  }

  // LISTENERS

  private var stateSubscription = Option.empty[Subscription]

  private val chainListener = new WalletEventsListener {
    override def onChainMasterSelected(event: InetSocketAddress): Unit = UITask {
      androidx.transition.TransitionManager.beginDelayedTransition(walletCards.defaultHeader)
      setVis(isVisible = false, walletCards.offlineIndicator)
    }.run

    override def onChainDisconnected: Unit = UITask {
      setVis(isVisible = true, walletCards.offlineIndicator)
    }.run

    override def onChainSyncing(start: Int, now: Int, max: Int): Unit = UITask {
      walletCards.chainSyncIndicator.setMaxProgress(max - start).setProgress(now - start)
      setVis(isVisible = max - now > 2016 * 4, walletCards.chainSyncIndicator)
    }.run

    override def onChainSyncEnded(localTip: Int): Unit = UITask {
      setVis(isVisible = false, walletCards.chainSyncIndicator)
    }.run

    override def onWalletReady(event: WalletReady): Unit =
      DbStreams.next(DbStreams.txDbStream)
  }

  private val fiatRatesListener = new FiatRatesListener {
    def onFiatRates(rates: FiatRatesInfo): Unit =
      UITask(walletCards.updateView).run
  }

  // Lifecycle methods

  override def onNewIntent(intent: Intent): Unit = {
    super.onNewIntent(intent)
    setIntent(intent)
  }

  override def onResume: Unit = runAnd(super.onResume) {
    try WalletParams.connectionProvider.notifyAppAvailable catch none
    val dataOpt = Seq(getIntent.getDataString, getIntent getStringExtra Intent.EXTRA_TEXT).find(externalData => null != externalData)
    runInFutureProcessOnUI(dataOpt.foreach(InputParser.recordValue), none)(_ => try checkExternalData(noneRunnable) catch none)
    setIntent(new Intent)
  }

  override def onDestroy: Unit = {
    try WalletParams.chainWallets.catcher ! WalletEventsCatcher.Remove(chainListener) catch none
    try WalletParams.fiatRates.listeners -= fiatRatesListener catch none
    stateSubscription.foreach(_.unsubscribe)
    super.onDestroy
  }

  override def onBackPressed: Unit = if (isSearchOn) rmSearch(null) else super.onBackPressed

  type GrantResults = Array[Int]
  override def onRequestPermissionsResult(reqCode: Int, permissions: Array[String], results: GrantResults): Unit = {
    if (reqCode == scannerRequestCode && results.nonEmpty && results.head == PackageManager.PERMISSION_GRANTED) bringScanner(null)
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case bitcoinUri: BitcoinUri if Try(WalletParams addressToPubKeyScript bitcoinUri.address).isSuccess =>

      if (WalletParams.chainWallets.usableWallets.size == 1) {
        bringSendBitcoinPopup(bitcoinUri, WalletParams.chainWallets.usableWallets.head)
      } else bringChainWalletChooser(me titleViewFromUri bitcoinUri) { wallet =>
        // We have wallet candidates to spend from here
        bringSendBitcoinPopup(bitcoinUri, wallet)
      }

    case a2a: MultiAddressParser.AddressToAmount =>
      val dustAmount = a2a.values.secondItems.find(amount => WalletParams.chainWallets.params.dustLimit > amount)
      val badAddress = a2a.values.firstItems.find(address => Try(WalletParams addressToPubKeyScript address).isFailure)

      if (dustAmount.nonEmpty) {
        val minimum = WalletParams.chainWallets.params.dustLimit.toLong
        onFail(s"Incorrect amount=${dustAmount.get.toLong}, minimum=$minimum")
      } else if (badAddress.nonEmpty) {
        onFail(s"Incorrect Bitcoin address=${badAddress.get}")
      } else if (WalletParams.chainWallets.usableWallets.size == 1) {
        bringSendMultiBitcoinPopup(a2a, WalletParams.chainWallets.usableWallets.head)
      } else bringChainWalletChooser(me getString dialog_send_btc_many) { wallet =>
        // We have wallet candidates to spend from here
        bringSendMultiBitcoinPopup(a2a, wallet)
      }

    case _ =>
      whenNone.run
  }

  def isSearchOn: Boolean = walletCards.searchField.getTag.asInstanceOf[Boolean]

  override def START(state: Bundle): Unit =
    WalletApp.isAlive match {
      case true if WalletParams.isOperational =>
        setContentView(com.btcontract.wallet.R.layout.activity_hub)
        WalletParams.fiatRates.listeners += fiatRatesListener
        WalletParams.chainWallets.catcher ! chainListener
        instance = me

        bottomActionBar post UITask {
          bottomBlurringArea.setHeightTo(bottomActionBar)
          itemsList.setPadding(0, 0, 0, bottomActionBar.getHeight)
        }

        // LIST

        itemsList.addHeaderView(walletCards.view)
        itemsList.setAdapter(paymentsAdapter)
        itemsList.setDividerHeight(0)
        itemsList.setDivider(null)

        // Fill wallet list with wallet card views here
        walletCards.recoveryPhraseWarning setOnClickListener onButtonTap(viewRecoveryCode)
        walletCards.chainCards.init(WalletParams.chainWallets.wallets)
        walletCards.updateView

        runInFutureProcessOnUI(loadRecent, none) { _ =>
          // User may kill an activity but not an app and on getting back there won't be a chain listener event, so check connectivity once again here
          setVisMany(WalletApp.ensureTor -> walletCards.torIndicator, WalletApp.currentChainNode.isEmpty -> walletCards.offlineIndicator)
          walletCards.searchField addTextChangedListener onTextChange(searchWorker.addWork)
          paymentAdapterDataChanged.run
        }

        // STREAMS

        val window = 500.millis
        timer.scheduleAtFixedRate(paymentAdapterDataChanged, 20000, 20000)
        stateSubscription = Rx.uniqueFirstAndLastWithinWindow(DbStreams.txDbStream, window).subscribe { _ =>
          // After each delayed update we check if pending txs got confirmed or double-spent
          // do this check specifically after updating txInfos with new items
          loadRecent

          for {
            txInfo <- txInfos if !txInfo.isDoubleSpent && !txInfo.isConfirmed
            relatedChainWallet <- WalletParams.chainWallets.findByPubKey(txInfo.pubKey)
            res <- relatedChainWallet.doubleSpent(txInfo.tx) if res.depth != txInfo.depth || res.isDoubleSpent != txInfo.isDoubleSpent
          } WalletApp.txDataBag.updStatus(txInfo.txid, res.depth, updatedStamp = res.stamp, res.isDoubleSpent)

          UITask(walletCards.updateView).run
          paymentAdapterDataChanged.run
        }.asSome

      case true =>
        WalletApp.extDataBag.tryGetSecret match {
          case Failure(_: android.database.CursorIndexOutOfBoundsException) =>
            // Record is not present at all, this is probaby a fresh wallet
            me exitTo classOf[SetupActivity]

          case Failure(reason) =>
            // Notify user about it
            throw reason

          case Success(secret) =>
            WalletApp.makeOperational(secret)
            START(state)
        }

      case false =>
        WalletApp.makeAlive
        START(state)
  }

  // VIEW HANDLERS

  def bringSearch(view: View): Unit = {
    walletCards.searchField.setTag(true)
    androidx.transition.TransitionManager.beginDelayedTransition(contentWindow)
    setVisMany(false -> walletCards.defaultHeader, true -> walletCards.searchField)
    showKeys(walletCards.searchField)
  }

  def rmSearch(view: View): Unit = {
    walletCards.searchField.setTag(false)
    walletCards.searchField.setText(new String)
    androidx.transition.TransitionManager.beginDelayedTransition(contentWindow)
    setVisMany(true -> walletCards.defaultHeader, false -> walletCards.searchField)
    WalletApp.app.hideKeys(walletCards.searchField)
  }

  def bringSendOptions(view: View): Unit = {
    def doBringSendInputWithOptionalScan: Unit = {
      val (container, extraInputLayout, extraInput) = singleInputPopup
      val builder = titleBodyAsViewBuilder(title = null, body = container)
      def switchToScanner(alert: AlertDialog): Unit = runAnd(alert.dismiss)(me bringScanner null)
      mkCheckFormNeutral(proceed, none, switchToScanner, builder, dialog_ok, dialog_cancel, dialog_scan)
      extraInputLayout.setHint(typing_hints)
      showKeys(extraInput)

      def proceed(alert: AlertDialog): Unit = runAnd(alert.dismiss) {
        runInFutureProcessOnUI(InputParser recordValue extraInput.getText.toString, onFail) { _ =>
          def attemptProcessInput: Unit = runAnd(doBringSendInputWithOptionalScan)(nothingUsefulTask.run)
          me checkExternalData UITask(attemptProcessInput)
        }
      }
    }

    doBringSendInputWithOptionalScan
  }

  def bringScanner(view: View): Unit = {
    val onScan = UITask(me checkExternalData nothingUsefulTask)
    val sheet = new sheets.OnceBottomSheet(me, getString(typing_hints).asSome, onScan)
    callScanner(sheet)
  }

  def bringBitcoinSpecificScanner(fromWallet: ElectrumEclairWallet): Unit = {
    def resolveLegacyWalletBtcAddressQr: Unit = InputParser.checkAndMaybeErase {
      case uri: BitcoinUri if Try(WalletParams addressToPubKeyScript uri.address).isSuccess =>
        bringSendBitcoinPopup(uri, fromWallet)
      case _ => nothingUsefulTask.run
    }

    val instruction = getString(scan_btc_address).asSome
    def onData: Runnable = UITask(resolveLegacyWalletBtcAddressQr)
    val sheet = new sheets.OnceBottomSheet(me, instruction, onData)
    callScanner(sheet)
  }

  def gotoReceivePage(view: View): Unit = WalletParams.chainWallets.usableWallets.collectFirst {
    case wallet if !wallet.info.core.isRemovable => goToWithValue(ClassNames.qrChainActivityClass, wallet)
  }

  def goToSettingsPage(view: View): Unit = goTo(ClassNames.settingsActivityClass)

  def bringSendBitcoinPopup(uri: BitcoinUri, fromWallet: ElectrumEclairWallet): Unit = {
    val sendView = new ChainSendView(fromWallet, getString(dialog_set_label).asSome, dialog_visibility_private)
    val chainPubKeyScript = WalletParams.addressToPubKeyScript(uri.address)

    def attempt(alert: AlertDialog): Unit =
      runFutureProcessOnUI(fromWallet.makeTx(chainPubKeyScript, sendView.manager.resultMsat.truncateToSatoshi, Map.empty, feeView.rate), onFail) { response =>
        // This may be a signing or a hardware wallet, in case if it's a hardware wallet we need additional UI action so we use this method here

        proceedConfirm(fromWallet, sendView, alert, response) { signedTx =>
          val desc = PlainTxDescription(uri.address :: Nil, sendView.manager.resultExtraInput orElse uri.label orElse uri.message)
          val isSent = Await.result(broadcastTx(fromWallet, desc, signedTx, 0.sat, response.transferred, response.fee), 30.seconds)
          if (!isSent) cleanFailedBroadcast(signedTx.txid)
          alert.dismiss
        }
      }

    lazy val alert = {
      val title = titleViewFromUri(uri)
      val neutralRes = if (uri.amount.isDefined) -1 else dialog_max
      val builder = titleBodyAsViewBuilder(title.asColoredView(me chainWalletBackground fromWallet), sendView.body)
      def useMax(alert: AlertDialog): Unit = sendView.manager.updateText(fromWallet.info.lastBalance.toMilliSatoshi)
      addFlowChip(title.flow, getString(dialog_send_btc_from).format(fromWallet.info.label), R.drawable.border_yellow)
      mkCheckFormNeutral(attempt, none, useMax, builder, dialog_ok, dialog_cancel, neutralRes)
    }

    lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.chainEditView.host) {
      rate = WalletParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(WalletParams.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget)

      worker = new ThrottledWork[String, GenerateTxResponse] {
        // This is a generic sending facility which may send to non-segwit, so always use a safer high dust threshold
        override def error(exception: Throwable): Unit = update(feeOpt = None, showIssue = sendView.manager.resultMsat >= WalletParams.chainWallets.params.dustLimit)
        def work(reason: String): Observable[GenerateTxResponse] = Rx fromFutureOnIo fromWallet.makeTx(chainPubKeyScript, sendView.manager.resultMsat.truncateToSatoshi, Map.empty, rate)
        def process(reason: String, response: GenerateTxResponse): Unit = update(feeOpt = response.fee.toMilliSatoshi.asSome, showIssue = false)
      }

      override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
        updatePopupButton(getPositiveButton(alert), feeOpt.isDefined)
        super.update(feeOpt, showIssue)
      }.run
    }

    // Automatically update a candidate transaction each time user changes amount value
    sendView.manager.inputAmount addTextChangedListener onTextChange(feeView.worker.addWork)
    feeView.update(feeOpt = None, showIssue = false)

    uri.amount foreach { asked =>
      sendView.manager.updateText(value = asked)
      sendView.manager.inputAmount.setEnabled(false)
      sendView.manager.fiatInputAmount.setEnabled(false)
    }
  }

  def bringSendMultiBitcoinPopup(addressToAmount: MultiAddressParser.AddressToAmount, fromWallet: ElectrumEclairWallet): Unit = {
    val scriptToAmount = addressToAmount.values.firstItems.map(WalletParams.addressToPubKeyScript).zip(addressToAmount.values.secondItems).toMap
    val sendView = new ChainSendView(fromWallet, badge = None, visibilityRes = -1)

    def attempt(alert: AlertDialog): Unit =
      runFutureProcessOnUI(fromWallet.makeBatchTx(scriptToAmount, feeView.rate), onFail) { response =>
        // This may be a signing or a hardware wallet just like with a single transaction case

        proceedConfirm(fromWallet, sendView, alert, response) { signedTx =>
          val desc = PlainTxDescription(addresses = addressToAmount.values.firstItems.toList)
          val isSent = Await.result(broadcastTx(fromWallet, desc, signedTx, 0.sat, response.transferred, response.fee), 30.seconds)
          if (!isSent) cleanFailedBroadcast(signedTx.txid)
          alert.dismiss
        }
      }

    lazy val alert = {
      val title = new TitleView(me getString dialog_send_btc_many)
      val builder = titleBodyAsViewBuilder(title.asColoredView(me chainWalletBackground fromWallet), sendView.body)
      addFlowChip(title.flow, getString(dialog_send_btc_from).format(fromWallet.info.label), R.drawable.border_yellow)
      mkCheckForm(attempt, none, builder, dialog_ok, dialog_cancel)
    }

    lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.chainEditView.host) {
      rate = WalletParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(WalletParams.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget)

      worker = new ThrottledWork[String, GenerateTxResponse] {
        def process(reason: String, response: GenerateTxResponse): Unit = update(feeOpt = response.fee.toMilliSatoshi.asSome, showIssue = false)
        def work(reason: String): Observable[GenerateTxResponse] = Rx fromFutureOnIo fromWallet.makeBatchTx(scriptToAmount, rate)
        override def error(exception: Throwable): Unit = update(feeOpt = None, showIssue = true)
      }

      override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
        updatePopupButton(getPositiveButton(alert), feeOpt.isDefined)
        super.update(feeOpt, showIssue)
      }.run
    }

    for (address ~ amount <- addressToAmount.values.reverse) {
      val humanAmount = WalletApp.denom.parsedWithSign(amount.toMilliSatoshi, cardIn, cardZero)
      val parent = getLayoutInflater.inflate(R.layout.frag_two_sided_item, null)
      new TwoSidedItem(parent, address.short.html, humanAmount.html)
      sendView.chainEditView.host.addView(parent, 0)
    }

    // Hide address facility, we display a list of addresses instead
    setVis(isVisible = false, sendView.chainEditView.inputChain)
    feeView.update(feeOpt = None, showIssue = false)
    feeView.worker addWork "MULTI-SEND-INIT-CALL"
  }

  def paymentAdapterDataChanged: TimerTask = UITask {
    setVis(allInfos.isEmpty, walletCards.recoveryPhraseWarning)
    paymentsAdapter.notifyDataSetChanged
  }

  def proceedConfirm(wallet: ElectrumEclairWallet, sendView: ChainSendView, alert: AlertDialog, response: GenerateTxResponse)(process: Transaction => Unit): Unit = {
    // This is used after user decided to send a transaction, if the wallet happens to be a hardware one then generated response is fake and we got stuff to do
    val finalSendButton = sendView.chainConfirmView.chainButtonsView.chainNextButton

    if (wallet.isSigning) {
      // This is a signing wallet so signed response tx is a final one, we use it
      finalSendButton setOnClickListener onButtonTap(process apply response.tx)
      sendView.switchToConfirm(alert, response)
    } else if (wallet.info.core.masterFingerprint.nonEmpty) {
      sendView.chainReaderView.onSignedTx = signedTx => UITask {
        if (signedTx.txOut.toSet != response.tx.txOut.toSet) alert.dismiss
        finalSendButton setOnClickListener onButtonTap(process apply signedTx)
        sendView.switchToConfirm(alert, response)
      }.run

      val masterFingerprint = wallet.info.core.masterFingerprint.get
      val psbt = prepareBip84Psbt(response, masterFingerprint)
      sendView.switchToHardwareOutgoing(alert, psbt)
    } else alert.dismiss
  }

  def proceedWithoutConfirm(wallet: ElectrumEclairWallet, sendView: ChainSendView, alert: AlertDialog, response: GenerateTxResponse)(process: Transaction => Unit): Unit = {
    // This is used after user decided to send CPFP/RBF a transaction, if the wallet happens to be a hardware one then generated response is fake and we got stuff to do

    if (wallet.isSigning) {
      process(response.tx)
      alert.dismiss
    } else if (wallet.info.core.masterFingerprint.nonEmpty) {
      sendView.chainReaderView.onSignedTx = signedTx => UITask {
        if (signedTx.txOut.toSet == response.tx.txOut.toSet) process(signedTx)
        alert.dismiss
      }.run

      val masterFingerprint = wallet.info.core.masterFingerprint.get
      val psbt = prepareBip84Psbt(response, masterFingerprint)
      sendView.switchToHardwareOutgoing(alert, psbt)
    } else alert.dismiss
  }

  def broadcastTx(fromWallet: ElectrumEclairWallet, desc: TxDescription, finalTx: Transaction, received: Satoshi, sent: Satoshi, fee: Satoshi): Future[Boolean] = {
    WalletApp.txInfos(finalTx.txid) = TxInfo(finalTx.toString, finalTx.txid.toHex, invalidPubKey.toString, depth = 0, received, sent, fee, seenAt = System.currentTimeMillis,
      System.currentTimeMillis, desc, BaseActivity.totalBalance, WalletParams.fiatRates.info.rates.toJson.compactPrint, incoming = if (received > sent) 1 else 0, doubleSpent = 0)

    DbStreams.next(DbStreams.txDbStream)
    fromWallet.broadcast(finalTx)
  }

  def cleanFailedBroadcast(failedTxid: ByteVector32): Unit = {
    onFail(me getString error_btc_broadcast_fail)
    WalletApp.txInfos.remove(key = failedTxid)
    DbStreams.next(DbStreams.txDbStream)
  }
}
