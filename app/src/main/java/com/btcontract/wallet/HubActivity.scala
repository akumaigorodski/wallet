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
import com.btcontract.wallet.utils._
import com.chauthai.swipereveallayout.{SwipeRevealLayout, ViewBinderHelper}
import com.danilomendes.progressbar.InvertedTextProgressbar
import com.ornach.nobobutton.NoboButton
import com.sparrowwallet.drongo
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{GenerateTxResponse, OkOrError, RBFResponse, WalletReady}
import fr.acinq.eclair.blockchain.electrum.{ElectrumWallet, WalletSpec}
import fr.acinq.eclair.blockchain.fee.FeeratePerByte
import immortan._
import immortan.crypto.Tools._
import immortan.sqlite.DbStreams
import immortan.utils.ImplicitJsonFormats._
import immortan.utils._
import org.apmem.tools.layouts.FlowLayout
import rx.lang.scala.Subscription
import scodec.bits.ByteVector
import spray.json._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


object HubActivity {
  case class AddressSpec(amounts: Map[ExtendedPublicKey, Satoshi], addresses: List[AddressInfo] = Nil) {
    def withBalance: List[AddressInfo] = addresses.filter(address => amounts contains address.pubKey)
  }

  var displayFullIxInfoHistory: Boolean = false
  var alreadySeenTxids: Set[ByteVector32] = Set.empty[ByteVector32]
  case class Accumulator(txids: Set[ByteVector32], txinfos: Vector[TxInfo] = Vector.empty) {
    def withInfo(info: TxInfo): Accumulator = Accumulator(txids ++ info.relatedTxids, txinfos :+ info)
  }

  var txInfos: Iterable[TxInfo] = Nil
  var recentTxInfos: Iterable[TxInfo] = Nil

  var addressSpec: AddressSpec = _
  var filteredAddressInfos: Iterable[AddressInfo] = Nil

  var allInfos: Seq[ItemDetails] = Nil
  var instance: HubActivity = _
}

class HubActivity extends BaseActivity with ExternalDataChecker { me =>
  private[this] lazy val paymentTypeIconIds = List(R.id.btcAddress, R.id.btcIncoming,
    R.id.btcInBoosted, R.id.btcOutBoosted, R.id.btcOutCancelled, R.id.btcOutgoing)

  private[this] lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]
  private[this] lazy val itemsList = findViewById(R.id.itemsList).asInstanceOf[ListView]

  private[this] lazy val expandContainer = getLayoutInflater.inflate(R.layout.frag_expand, null, false)
  private[this] lazy val expand = expandContainer.findViewById(R.id.expand).asInstanceOf[ImageButton]

  private[this] val viewBinderHelper = new ViewBinderHelper
  lazy val walletCards = new WalletCardsViewHolder
  var openListItems = Set.empty[String]

  // PAYMENT LIST

  def loadRecentTxInfos: Unit = {
    filteredAddressInfos = Nil

    if (displayFullIxInfoHistory) {
      // Use wishes to see all txs, we still limit by some sensible number to now slow UI down
      recentTxInfos = WalletApp.txDataBag.listRecentTxs(500).map(WalletApp.txDataBag.toTxInfo)
      txInfos = recentTxInfos
    } else {
      val initialAccumulator = Accumulator(alreadySeenTxids)
      recentTxInfos = WalletApp.txDataBag.listRecentTxs(10).map(WalletApp.txDataBag.toTxInfo)
      val accumulator2 = recentTxInfos.take(3).foldLeft(initialAccumulator)(_ withInfo _)

      val accumulator3 = recentTxInfos.drop(3).foldLeft(accumulator2) {
        case (acc, info) if acc.txids.intersect(info.relatedTxids).nonEmpty => acc.withInfo(info)
        case (acc, info) if !info.isConfirmed && !info.isDoubleSpent => acc.withInfo(info)
        case (acc, info) if alreadySeenTxids.contains(info.txid) => acc.withInfo(info)
        case (acc, _) => acc
      }

      txInfos = accumulator3.txinfos
      alreadySeenTxids ++= accumulator3.txids
    }
  }

  def loadSearchedTxInfos(query: String): Unit = {
    val query1 = query.replaceAll("\\s", "").toLowerCase
    filteredAddressInfos = addressSpec.addresses.filter(_.identity.toLowerCase contains query1)
    txInfos = WalletApp.txDataBag.searchTransactions(query1).map(WalletApp.txDataBag.toTxInfo)
  }

  def fillAllInfos: Unit = {
    val pending = WalletApp.pendingTxInfos.values.toList
    val displayed = pending ++ txInfos ++ filteredAddressInfos
    allInfos = SemanticOrder.makeSemanticOrder(displayed)
  }

  def loadRecent: Unit = {
    loadRecentTxInfos
    fillAllInfos
  }

  def loadSearch(query: String): Unit = {
    loadSearchedTxInfos(query)
    fillAllInfos
  }

  val searchWorker: ThrottledWork[String, Unit] = new ThrottledWork[String, Unit] {
    override def work(query: String): Unit = if (query.nonEmpty) loadSearch(query) else loadRecent
    override def process(query: String, searchResultEffect: Unit): Unit = paymentAdapterDataChanged.run
  }

  val paymentsAdapter: BaseAdapter = new BaseAdapter {
    override def getItem(pos: Int): ItemDetails = allInfos(pos)
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
    val amountAndMeta: RelativeLayout = swipeWrap.findViewById(R.id.amountAndMeta).asInstanceOf[RelativeLayout]
    val description: TextView = swipeWrap.findViewById(R.id.description).asInstanceOf[TextView]
    val statusIcon: ImageView = swipeWrap.findViewById(R.id.statusIcon).asInstanceOf[ImageView]
    val statusText: TextView = swipeWrap.findViewById(R.id.statusText).asInstanceOf[TextView]
    val labelIcon: ImageView = swipeWrap.findViewById(R.id.labelIcon).asInstanceOf[ImageView]
    val amount: TextView = swipeWrap.findViewById(R.id.amount).asInstanceOf[TextView]
    val meta: TextView = swipeWrap.findViewById(R.id.meta).asInstanceOf[TextView]

    spacer1.setZ(Float.MaxValue)
    itemView.setTag(this)

    val paymentTypeIconViews: List[View] = paymentTypeIconIds.map(swipeWrap.findViewById)
    val iconMap: Map[Int, View] = paymentTypeIconIds.zip(paymentTypeIconViews).toMap
    var currentDetails: ItemDetails = _
    var lastVisibleIconId: Int = -1

    paymentCardContainer setOnClickListener onButtonTap(ractOnTap)
    setItemLabel setOnClickListener onButtonTap(doSetItemLabel)
    shareItem setOnClickListener onButtonTap(doShareItem)

    // MENU BUTTONS

    def doSetItemLabel: Unit = {
      val (container, extraInputLayout, extraInput, _, _) = singleInputPopup
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, titleBodyAsViewBuilder(null, container), dialog_ok, dialog_cancel)
      extraInputLayout.setHint(dialog_set_label)
      showKeys(extraInput)

      def proceed: Unit = Some(currentDetails).collectFirst { case info: TxInfo =>
        val optionalInput = Option(extraInput.getText.toString).map(trimmed).filter(_.nonEmpty)
        val description = info.description.withNewLabel(optionalInput)
        WalletApp.txDataBag.updDescription(description, info.txid)
      }
    }

    def doShareItem: Unit = Some(currentDetails).collectFirst {
      case info: TxInfo => me share getString(share_chain_tx).format(info.txString)
      case info: AddressInfo => me share info.identity
    }

    def ractOnTap: Unit = {
      val isVisible = extraInfo.getVisibility == View.VISIBLE
      if (isVisible) collapse(currentDetails) else expand(currentDetails)
    }

    // CPFP / RBF

    def boostCPFP(info: TxInfo): Unit = info.extPubs.flatMap(ElectrumWallet.specs.get) match {
      case Nil => snack(contentWindow, getString(error_btc_no_wallet).html, dialog_ok, _.dismiss)
      case wallets => doBoostCPFP(wallets, info)
    }

    def doBoostCPFP(specs: Seq[WalletSpec], info: TxInfo): Unit = {
      val changeSpec = ElectrumWallet.orderByImportance(candidates = specs).head
      val address = changeSpec.data.keys.ewt.textAddress(changeSpec.data.changePubKey)
      val ourPubKeyScript = ElectrumWallet.addressToPubKeyScript(address)

      val fromOutPoints = for (idx <- info.tx.txOut.indices) yield OutPoint(info.tx.hash, idx)
      val receivedMsat = info.receivedSat.toMilliSatoshi

      val sendView = new ChainSendView(specs, badge = None, visibilityRes = -1)
      val blockTarget = WalletApp.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val target = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(target), sendView.cpfpView.host) {
        rate = target

        worker = new ThrottledWork[String, GenerateTxResponse] {
          override def work(reason: String): GenerateTxResponse = ElectrumWallet.makeCPFP(specs, fromOutPoints.toSet, ourPubKeyScript, rate)
          override def process(reason: String, response: GenerateTxResponse): Unit = update(response.fee.toMilliSatoshi.asSome, showIssue = false)
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
        // Transaction could have gotten a confirmation while user was filling a form
        val sanityCheck = ElectrumWallet.doubleSpent(specs.head.data.keys.ewt.xPub, info.tx)
        if (sanityCheck.depth > 0 || sanityCheck.isDoubleSpent) return

        val cpfpBumpOrder = SemanticOrder(info.txid.toHex, System.currentTimeMillis)
        // Only update parent semantic order if it does not already have one, record it BEFORE sending CPFP
        val parentDescWithOrder = info.description.withNewOrderCond(cpfpBumpOrder.copy(order = Long.MinValue).asSome)
        WalletApp.txDataBag.updDescription(parentDescWithOrder, info.txid)

        runInFutureProcessOnUI(ElectrumWallet.makeCPFP(specs, fromOutPoints.toSet, ourPubKeyScript, feeView.rate), onFail) { response =>
          // At this point we have received some response, in this case it can not be failure but then we maybe have a hardware wallet

          proceedWithoutConfirm(sendView, alert, response) { signedTx =>
            val desc = PlainTxDescription(address :: Nil, label = None, cpfpBumpOrder.asSome, cpfpBy = None, cpfpOf = info.txid.asSome)
            runFutureProcessOnUI(broadcastTx(desc, signedTx, response.transferred, sent = Satoshi(0L), response.fee, incoming = 1), onFail) {

              case Some(error) =>
                // We revert the whole description back since CPFP has failed
                WalletApp.txDataBag.updDescription(info.description, info.txid)
                cleanFailedBroadcast(signedTx.txid, error.message)

              case None =>
                // Parent semantic order has already been updated, now we also must update CPFP parent info
                WalletApp.txDataBag.updDescription(parentDescWithOrder.withNewCPFPBy(signedTx.txid), info.txid)
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

    def boostRBF(info: TxInfo): Unit = info.extPubs.flatMap(ElectrumWallet.specs.get) match {
      // This is a special case where we must make sure that we are going to use exactly as many wallets as we used originally
      case res if res.size < info.extPubs.size => snack(contentWindow, getString(error_btc_no_wallet).html, dialog_ok, _.dismiss)
      case specs => doBoostRBF(specs, info)
    }

    def doBoostRBF(specs: Seq[WalletSpec], info: TxInfo): Unit = {
      val changeTo = ElectrumWallet.orderByImportance(candidates = specs).head
      val currentFee = WalletApp.denom.parsedWithSignTT(info.feeSat.toMilliSatoshi, cardOut, cardIn)

      val sendView = new ChainSendView(specs, badge = None, visibilityRes = -1)
      val blockTarget = WalletApp.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val target = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView: FeeView[RBFResponse] = new FeeView[RBFResponse](FeeratePerByte(target), sendView.rbfView.host) {
        rate = target

        worker = new ThrottledWork[String, RBFResponse] {
          override def process(reason: String, response: RBFResponse): Unit = response.result match {
            case Left(ElectrumWallet.PARENTS_MISSING) => showRbfErrorDesc(tx_rbf_err_parents_missing)
            case Left(ElectrumWallet.FOREIGN_INPUTS) => showRbfErrorDesc(tx_rbf_err_foreign_inputs)
            case Right(res) => update(res.fee.toMilliSatoshi.asSome, showIssue = false)
            case _ => error(new RuntimeException)
          }

          override def work(reason: String): RBFResponse = ElectrumWallet.rbfBump(specs, changeTo, info.tx, rate)
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
        // Transaction could have gotten a confirmation while user was filling a form
        val sanityCheck = ElectrumWallet.doubleSpent(specs.head.data.keys.ewt.xPub, info.tx)
        if (sanityCheck.depth > 0 || sanityCheck.isDoubleSpent) return

        val rbfParams = RBFParams(info.txid, TxDescription.RBF_BOOST)
        val ofOriginalTxid = info.description.rbf.map(_.ofTxid).getOrElse(info.txid).toHex
        val rbfBumpOrder = SemanticOrder(ofOriginalTxid, -System.currentTimeMillis)

        runInFutureProcessOnUI(ElectrumWallet.rbfBump(specs, changeTo, info.tx, feeView.rate), onFail) { responseWrap =>
          // At this point we have received some response, in this case it can not be failure but then we maybe have a hardware wallet
          val response = responseWrap.result.right.get

          proceedWithoutConfirm(sendView, alert, response) { signedTx =>
            val desc = PlainTxDescription(Nil, label = None, rbfBumpOrder.asSome, cpfpBy = None, cpfpOf = None, rbfParams.asSome)
            runFutureProcessOnUI(broadcastTx(desc, signedTx, received = Satoshi(0L), info.sentSat, response.fee, incoming = 0), onFail) {

              case Some(error) =>
                // We revert the whole description back since CPFP has failed
                WalletApp.txDataBag.updDescription(info.description, info.txid)
                cleanFailedBroadcast(signedTx.txid, error.message)

              case None =>
                val parentLowestOrder = rbfBumpOrder.copy(order = Long.MaxValue)
                val parentDesc = info.description.withNewOrderCond(parentLowestOrder.asSome)
                WalletApp.txDataBag.updDescription(parentDesc, info.txid)
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

    def cancelRBF(info: TxInfo): Unit = info.extPubs.flatMap(ElectrumWallet.specs.get) match {
      case Nil => snack(contentWindow, getString(error_btc_no_wallet).html, dialog_ok, _.dismiss)
      case specs => doCancelRBF(specs, info)
    }

    def doCancelRBF(specs: Seq[WalletSpec], info: TxInfo): Unit = {
      val changeSpec = ElectrumWallet.orderByImportance(candidates = specs).head
      val address = changeSpec.data.keys.ewt.textAddress(changeSpec.data.changePubKey)
      val ourPubKeyScript = ElectrumWallet.addressToPubKeyScript(address)

      val sendView = new ChainSendView(specs, badge = None, visibilityRes = -1)
      val blockTarget = WalletApp.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val currentFee = WalletApp.denom.parsedWithSignTT(info.feeSat.toMilliSatoshi, cardOut, cardIn)
      val target = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView: FeeView[RBFResponse] = new FeeView[RBFResponse](FeeratePerByte(target), sendView.rbfView.host) {
        rate = target

        worker = new ThrottledWork[String, RBFResponse] {
          override def process(reason: String, response: RBFResponse): Unit = response.result match {
            case Left(ElectrumWallet.PARENTS_MISSING) => showRbfErrorDesc(tx_rbf_err_parents_missing)
            case Left(ElectrumWallet.FOREIGN_INPUTS) => showRbfErrorDesc(tx_rbf_err_foreign_inputs)
            case Right(res) => update(res.fee.toMilliSatoshi.asSome, showIssue = false)
            case _ => error(new RuntimeException)
          }

          override def work(reason: String): RBFResponse = ElectrumWallet.rbfReroute(specs, info.tx, rate, ourPubKeyScript)
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
        // Transaction could have gotten a confirmation while user was filling a form
        val sanityCheck = ElectrumWallet.doubleSpent(specs.head.data.keys.ewt.xPub, info.tx)
        if (sanityCheck.depth > 0 || sanityCheck.isDoubleSpent) return

        val rbfParams = RBFParams(info.txid, TxDescription.RBF_CANCEL)
        val ofOriginalTxid = info.description.rbf.map(_.ofTxid).getOrElse(info.txid).toHex
        val rbfBumpOrder = SemanticOrder(ofOriginalTxid, -System.currentTimeMillis)

        runInFutureProcessOnUI(ElectrumWallet.rbfReroute(specs, info.tx, feeView.rate, ourPubKeyScript), onFail) { responseWrap =>
          // At this point we have received some response, in this case it can not be failure but then we maybe have a hardware wallet
          val response = responseWrap.result.right.get

          proceedWithoutConfirm(sendView, alert, response) { signedTx =>
            val desc = PlainTxDescription(addresses = Nil, label = None, rbfBumpOrder.asSome, None, None, rbfParams.asSome)
            runFutureProcessOnUI(broadcastTx(desc, signedTx, info.sentSat - response.fee, sent = Satoshi(0L), response.fee, incoming = 1), onFail) {

              case Some(error) =>
                // We revert the whole description back since CPFP has failed
                WalletApp.txDataBag.updDescription(info.description, info.txid)
                cleanFailedBroadcast(signedTx.txid, error.message)

              case None =>
                val parentLowestOrder = rbfBumpOrder.copy(order = Long.MaxValue)
                val parentDesc = info.description.withNewOrderCond(parentLowestOrder.asSome)
                WalletApp.txDataBag.updDescription(parentDesc, info.txid)
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

    def collapse[T <: ItemDetails](item: T): Unit = {
      setVis(isVisible = false, extraInfo)
      extraInfo.removeAllViewsInLayout
      openListItems -= item.identity
      description.setMaxLines(1)
    }

    def expand[T <: ItemDetails](item: T): Unit = {
      setVis(isVisible = true, extraInfo)
      extraInfo.removeAllViewsInLayout
      openListItems += item.identity
      description.setMaxLines(3)

      item match {
        case info: TxInfo =>
          val amount = if (info.isIncoming) info.receivedSat.toMilliSatoshi else info.sentSat.toMilliSatoshi
          val canRBF = !info.isIncoming && !info.isDoubleSpent && !info.isConfirmed && info.description.cpfpOf.isEmpty
          val canCPFP = info.isIncoming && !info.isDoubleSpent && !info.isConfirmed && info.description.rbf.isEmpty && info.description.canBeCPFPd
          val fiatThen = WalletApp.msatInFiatHuman(info.fiatRateSnapshot, WalletApp.fiatCode, amount, Denomination.formatFiat)

          if (ElectrumWallet.specs.size > 1)
            for (wallet <- info.extPubs flatMap ElectrumWallet.specs.get)
              addFlowChip(extraInfo, wallet.info.label, R.drawable.border_gray)

          addFlowChip(extraInfo, getString(popup_fiat).format(s"<font color=$cardIn>$fiatThen</font>"), R.drawable.border_gray)
          addFlowChip(extraInfo, chipText = getString(popup_txid) format info.txidString.short, R.drawable.border_green, info.txidString.asSome)
          for (adr <- info.description.addresses) addFlowChip(extraInfo, getString(popup_to_address) format adr.short, R.drawable.border_yellow, adr.asSome)

          if (info.feeSat > 0L.sat) {
            val fee = WalletApp.denom.directedWithSign(0L.msat, info.feeSat.toMilliSatoshi, cardOut, cardIn, cardZero, isIncoming = false)
            addFlowChip(extraInfo, chipText = getString(popup_chain_fee) format fee, R.drawable.border_gray)
          }

          for (addressInfo <- findTxInputAddress(info.tx).headOption if addressInfo.ewt.secrets.nonEmpty) {
            def doSign: Unit = bringSignDialog(getString(sign_sign_message_tx_title).format(addressInfo.identity.short, info.txidString.humanFour).asDefView, addressInfo)
            addFlowChip(extraInfo, getString(sign_sign_message), R.drawable.border_yellow, _ => doSign)
          }

          if (canCPFP) addFlowChip(extraInfo, getString(dialog_boost), R.drawable.border_yellow, _ => self boostCPFP info)
          if (canRBF) addFlowChip(extraInfo, getString(dialog_cancel), R.drawable.border_yellow, _ => self cancelRBF info)
          if (canRBF) addFlowChip(extraInfo, getString(dialog_boost), R.drawable.border_yellow, _ => self boostRBF info)

        case info: AddressInfo =>
          addFlowChip(extraInfo, "Copy address", R.drawable.border_yellow, _ => WalletApp.app copy info.identity)
          def doSign: Unit = bringSignDialog(getString(sign_sign_message_title).format(info.identity.short).asDefView, info)
          if (info.ewt.secrets.nonEmpty) addFlowChip(extraInfo, getString(sign_sign_message), R.drawable.border_yellow, _ => doSign)

          addressSpec.amounts.get(info.pubKey).map(_.toMilliSatoshi) foreach { balance =>
            val amount = WalletApp.denom.parsedWithSign(balance, cardIn, cardZero)
            addFlowChip(extraInfo, s"balance $amount", R.drawable.border_gray)
          }
      }
    }

    def updateDetails: Unit = currentDetails match {
      // Reusing the same view to speed the list up

      case info: TxInfo =>
        // We are not sure if this one has been broadcasted yet
        val isEphemeral = WalletApp.pendingTxInfos.contains(info.txid)
        if (isEphemeral) itemView.setAlpha(0.6F)
        swipeWrap.setLockDrag(isEphemeral)

        statusIcon setImageResource txStatusIcon(info)
        nonLinkContainer setBackgroundResource R.drawable.border_gray
        setVisMany(info.description.label.isDefined -> labelIcon, true -> nonLinkContainer, true -> amountAndMeta, true -> statusIcon, false -> statusText, true -> setItemLabel)
        amount.setText(WalletApp.denom.directedWithSign(info.receivedSat.toMilliSatoshi, info.sentSat.toMilliSatoshi, cardOut, cardIn, cardZero, info.isIncoming).html)
        description.setText(info.description.label getOrElse txDescription(info).html)
        setTxTypeIcon(info)
        setTxMeta(info)

      case info: AddressInfo =>
        swipeWrap.setLockDrag(false)
        setVisibleIcon(id = R.id.btcAddress)
        description.setText(info.identity.short.html)
        statusText.setText(info.description.label getOrElse "?")
        nonLinkContainer setBackgroundResource R.drawable.border_gray
        setVisMany(false -> labelIcon, true -> nonLinkContainer, false -> amountAndMeta, false -> statusIcon, true -> statusText, false -> setItemLabel)
        if (addressSpec.amounts contains info.pubKey) expand(info)
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
      case _ => transactionInfo.description.addresses.headOption.map(_.short) getOrElse getString(tx_btc)
    }

    def setTxTypeIcon(info: TxInfo): Unit = info.description match {
      case _ if info.description.cpfpOf.isDefined => setVisibleIcon(id = R.id.btcInBoosted)
      case _ if info.description.rbf.exists(_.mode == TxDescription.RBF_BOOST) => setVisibleIcon(id = R.id.btcOutBoosted)
      case _ if info.description.rbf.exists(_.mode == TxDescription.RBF_CANCEL) => setVisibleIcon(id = R.id.btcOutCancelled)
      case _ if info.isIncoming => setVisibleIcon(id = R.id.btcIncoming)
      case _ => setVisibleIcon(id = R.id.btcOutgoing)
    }

    def txStatusIcon(info: TxInfo): Int = {
      // Ephemeral tx has no connected wallet while it's being broadcasted
      // User may remove a wallet while related transactions are getting confirmed
      val hasNoWallets = info.extPubs.flatMap(ElectrumWallet.specs.get).isEmpty

      if (info.isConfirmed) R.drawable.baseline_done_24
      else if (info.isDoubleSpent) R.drawable.baseline_block_24
      else if (hasNoWallets) R.drawable.baseline_question_24
      else R.drawable.baseline_hourglass_empty_24
    }

    def setTxMeta(info: TxInfo): Unit = {
      if (info.isDoubleSpent) meta setText getString(tx_state_double_spent).html
      else meta setText WalletApp.app.when(info.date, WalletApp.app.dateFormat).html
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
      override def onCoinControlTap(key: ExtendedPublicKey): Unit = goToWithValue(ClassNames.coinControlActivityClass, key)
      override def onWalletTap(key: ExtendedPublicKey): Unit = goToWithValue(ClassNames.qrChainActivityClass, key)

      override def onLabelTap(key: ExtendedPublicKey): Unit = {
        val (container, extraInputLayout, extraInput, _, _) = singleInputPopup
        mkCheckForm(proceed, none, titleBodyAsViewBuilder(null, container), dialog_ok, dialog_cancel)
        extraInputLayout.setHint(dialog_set_label)
        showKeys(extraInput)

        def proceed(alert: AlertDialog): Unit = runAnd(alert.dismiss) {
          ElectrumWallet.setLabel(extraInput.getText.toString)(key)
          resetChainCards
        }
      }

      override def onRemoveTap(key: ExtendedPublicKey): Unit = {
        val builder = new AlertDialog.Builder(me).setMessage(confirm_remove_item)
        mkCheckForm(proceed, none, builder, dialog_ok, dialog_cancel)

        def proceed(alert: AlertDialog): Unit = {
          ElectrumWallet.removeWallet(key)
          resetChainCards
          alert.dismiss
        }
      }
    }

    def resetChainCards: Unit = {
      chainCards.holder.removeAllViewsInLayout
      chainCards.init(ElectrumWallet.specs.size)
      updateView
    }

    def updateView: Unit = {
      androidx.transition.TransitionManager.beginDelayedTransition(defaultHeader)
      val change = WalletApp.fiatRates.info.pctDifference(code = WalletApp.fiatCode).getOrElse(default = new String)
      val unitRate = WalletApp.msatInFiatHuman(WalletApp.fiatRates.info.rates, WalletApp.fiatCode, 100000000000L.msat, Denomination.formatFiatShort)
      fiatUnitPriceAndChange.setText(s"$unitRate $change".html)
      chainCards.update(ElectrumWallet.specs.values)
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
    def onFiatRates(rates: FiatRatesInfo): Unit = UITask {
      walletCards.fiatUnitPriceAndChange.setAlpha(1F)
      walletCards.updateView
    }.run
  }

  // Lifecycle methods

  override def onNewIntent(intent: Intent): Unit = {
    super.onNewIntent(intent)
    setIntent(intent)
  }

  override def onResume: Unit = runAnd(super.onResume) {
    try ElectrumWallet.connectionProvider.notifyAppAvailable catch none
    val dataOpt = Seq(getIntent.getDataString, getIntent getStringExtra Intent.EXTRA_TEXT).find(externalData => null != externalData)
    runInFutureProcessOnUI(dataOpt foreach InputParser.recordValue, none)(_ => try checkExternalData(noneRunnable) catch none)
    setIntent(new Intent)
  }

  override def onDestroy: Unit = {
    try ElectrumWallet.catcher ! WalletEventsCatcher.Remove(chainListener) catch none
    try WalletApp.fiatRates.listeners -= fiatRatesListener catch none
    stateSubscription.foreach(_.unsubscribe)
    super.onDestroy
  }

  type GrantResults = Array[Int]
  override def onRequestPermissionsResult(reqCode: Int, permissions: Array[String], results: GrantResults): Unit = {
    if (reqCode == scannerRequestCode && results.nonEmpty && results.head == PackageManager.PERMISSION_GRANTED) bringScanner(null)
  }

  override def checkExternalData(whenNone: Runnable): Unit = {
    val spendable = ElectrumWallet.specs.values.filter(_.spendable).toList
    val usable = ElectrumWallet.specs.values.filter(_.usable).toList

    def bringSingleAddressSelector(bitcoinUri: BitcoinUri) = new WalletSelector(me titleViewFromUri bitcoinUri) {
      def onOk: Unit = bringSendBitcoinPopup(chooser.selected.values.toList, bitcoinUri)
    }

    def bringMultiAddressSelector(a2a: MultiAddressParser.AddressToAmount) = new WalletSelector(me getString dialog_send_btc_many) {
      def onOk: Unit = bringSendMultiBitcoinPopup(chooser.selected.values.toList, a2a)
    }

    InputParser.checkAndMaybeErase {
      case bitcoinUri: BitcoinUri if Try(ElectrumWallet addressToPubKeyScript bitcoinUri.address).isSuccess =>
        if (spendable.size == 1) bringSendBitcoinPopup(spendable, bitcoinUri)
        else if (usable.size == 1) bringSendBitcoinPopup(usable, bitcoinUri)
        else bringSingleAddressSelector(bitcoinUri)

      case a2a: MultiAddressParser.AddressToAmount =>
        val dustAmount = a2a.values.secondItems.find(amount => ElectrumWallet.params.dustLimit > amount)
        val badAddress = a2a.values.firstItems.find(address => Try(ElectrumWallet addressToPubKeyScript address).isFailure)

        if (badAddress.nonEmpty) onFail(s"Incorrect address=${badAddress.get}")
        else if (dustAmount.nonEmpty) onFail(s"Low amount=${dustAmount.get.toLong}")
        else if (spendable.size == 1) bringSendMultiBitcoinPopup(spendable, a2a)
        else if (usable.size == 1) bringSendMultiBitcoinPopup(usable, a2a)
        else bringMultiAddressSelector(a2a)

      case lnUrl: LNUrl if lnUrl.isAuth =>
        showAuthForm(lnUrl)

      case data: BIP322VerifyData =>
        val address = drongo.address.Address.fromString(drongoNetwork, data.address)
        val verifies = try drongo.crypto.Bip322.verifyHashBip322(address.getScriptType, address, data.messageHash.toArray, data.signature64) catch { case _: Throwable => false }
        val isSignatureLegit = verifies && data.message.map(drongo.crypto.Bip322.getBip322MessageHash).map(ByteVector.view).forall(messageHash => data.messageHash == messageHash)
        val title = if (isSignatureLegit) new TitleView(me getString verify_ok).asColoredView(R.color.buttonGreen) else new TitleView(me getString verify_no).asColoredView(R.color.buttonRed)
        val bld = new AlertDialog.Builder(me).setCustomTitle(title).setMessage(getString(verify_details).format(data.address.humanFour, data.messageHash.toHex.humanFour, data.message getOrElse "?").html)
        mkCheckForm(_.dismiss, share(data.serialize), bld, dialog_ok, dialog_share)

      case data: BIP32SignData =>
        runInFutureProcessOnUI(getAddressSpec.addresses.find(addressInfo => data.address == addressInfo.identity), onFail) {
          case None => snack(contentWindow, getString(sign_address_not_found).format(data.address.short).html, dialog_ok, _.dismiss)
          case Some(info) => bringSignDialog(getString(sign_sign_message_title).format(info.identity.short).asDefView, info).setText(data.message)
        }

      case _ =>
        whenNone.run
    }
  }

  override def onBackPressed: Unit = {
    if (isSearchOn) rmSearch(view = null)
    else if (displayFullIxInfoHistory) {
      displayFullIxInfoHistory = false
      allInfos = allInfos.take(n = 3)
      paymentAdapterDataChanged.run
    } else super.onBackPressed
  }

  def isSearchOn: Boolean = {
    walletCards.searchField.getTag.asInstanceOf[Boolean]
  }

  override def START(state: Bundle): Unit =
    WalletApp.isAlive match {
      case true if WalletApp.isOperational =>
        setContentView(com.btcontract.wallet.R.layout.activity_hub)
        WalletApp.fiatRates.listeners += fiatRatesListener
        ElectrumWallet.catcher ! chainListener
        instance = me

        itemsList.addFooterView(expandContainer)
        itemsList.addHeaderView(walletCards.view)
        itemsList.setAdapter(paymentsAdapter)
        itemsList.setDividerHeight(0)
        itemsList.setDivider(null)

        expand setOnClickListener onButtonTap {
          androidx.transition.TransitionManager.beginDelayedTransition(contentWindow)
          runAnd(displayFullIxInfoHistory = true)(loadRecent)
          paymentAdapterDataChanged.run
        }

        // Fill wallet list with wallet card views here
        walletCards.recoveryPhraseWarning setOnClickListener onButtonTap(viewRecoveryCode)
        walletCards.chainCards.init(ElectrumWallet.specs.size)
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
          if (!isSearchOn) loadRecent

          for {
            txInfo <- recentTxInfos if !txInfo.isDoubleSpent && !txInfo.isConfirmed
            relatedSpec <- txInfo.extPubs.flatMap(ElectrumWallet.specs.get).headOption
            doubleSpent = ElectrumWallet.doubleSpent(relatedSpec.data.keys.ewt.xPub, txInfo.tx)
            if doubleSpent.depth != txInfo.depth || doubleSpent.isDoubleSpent != txInfo.isDoubleSpent
          } WalletApp.txDataBag.updStatus(txInfo.txid, doubleSpent.depth, doubleSpent.stamp, doubleSpent.isDoubleSpent)

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

    runInFutureProcessOnUI(getAddressSpec, none) { spec =>
      filteredAddressInfos = spec.withBalance
      txInfos = recentTxInfos.take(3)
      addressSpec = spec
      fillAllInfos

      // Update view after filling
      paymentAdapterDataChanged.run
    }
  }

  def rmSearch(view: View): Unit = {
    walletCards.searchField.setTag(false)
    walletCards.searchField.setText(new String)
    androidx.transition.TransitionManager.beginDelayedTransition(contentWindow)
    setVisMany(true -> walletCards.defaultHeader, false -> walletCards.searchField)
    WalletApp.app.hideKeys(walletCards.searchField)
  }

  def bringPasteAddressDialog: Unit = {
    def doBringPasteAddressDialog: Unit = {
      val (container, extraInputLayout, extraInput, _, _) = singleInputPopup
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, titleBodyAsViewBuilder(null, container), dialog_ok, dialog_cancel)
      extraInputLayout.setHint(typing_hints)
      showKeys(extraInput)

      def proceed: Unit = runInFutureProcessOnUI(InputParser recordValue extraInput.getText.toString, onFail) { _ =>
        def attemptProcessInput: Unit = runAnd(doBringPasteAddressDialog)(nothingUsefulTask.run)
        me checkExternalData UITask(attemptProcessInput)
      }
    }

    doBringPasteAddressDialog
  }

  def bringScanner(view: View): Unit = {
    def onCreated(sheet: sheets.OnceBottomSheet) = {
      sheet.altAction setOnClickListener onButtonTap {
        timer.schedule(UITask(bringPasteAddressDialog), 225)
        sheet.dismiss
      }

      setVisMany(true -> sheet.instruction, true -> sheet.altAction)
      sheet.instruction.setText(typing_hints)
      sheet.altAction.setText(dialog_paste)
    }

    val onScan = UITask(me checkExternalData nothingUsefulTask)
    val sheet = new sheets.OnceBottomSheet(me, onCreated, onScan)
    callScanner(sheet)
  }

  def goToReceivePage(view: View): Unit = if (ElectrumWallet.specs.nonEmpty) {
    val specs = ElectrumWallet.orderByImportance(ElectrumWallet.specs.values.toList)
    goToWithValue(ClassNames.qrChainActivityClass, specs.head.data.keys.ewt.xPub)
  } else {
    val message = getString(error_btc_no_wallet).html
    snack(contentWindow, message, dialog_ok, _.dismiss)
  }

  def goToSettingsPage(view: View): Unit = {
    goTo(ClassNames.settingsActivityClass)
  }

  def bringSendBitcoinPopup(specs: Seq[WalletSpec], uri: BitcoinUri): Unit = {
    val sendView = new ChainSendView(specs, getString(dialog_set_label).asSome, dialog_visibility_private)
    val pubKeyScript = ElectrumWallet.addressToPubKeyScript(uri.address)
    val changeTo = ElectrumWallet.orderByImportance(specs).head

    def attempt(alert: AlertDialog): Unit =
      runInFutureProcessOnUI(ElectrumWallet.makeTx(specs, changeTo, pubKeyScript, sendView.manager.resultMsat.truncateToSatoshi, Map.empty, feeView.rate), onFail) { response =>
        // This may be a signing or a hardware wallet, in case if it's a hardware wallet we need additional UI action so we use this proxy method here

        proceedConfirm(sendView, alert, response) { signedTx =>
          val desc = PlainTxDescription(uri.address :: Nil, sendView.manager.resultExtraInput orElse uri.label orElse uri.message)
          val broadcastFuture = broadcastTx(desc, signedTx, received = Satoshi(0L), sent = response.transferred, response.fee, incoming = 0)
          runFutureProcessOnUI(broadcastFuture, onFail) { case Some(error) => cleanFailedBroadcast(signedTx.txid, error.message) case None => }
          alert.dismiss
        }
      }

    lazy val alert = {
      val title = titleViewFromUri(uri)
      val neutralRes = if (uri.amount.isDefined) -1 else dialog_max
      val builder = titleBodyAsViewBuilder(title.asColoredView(me walletBackground specs), sendView.body)
      def useMax(alert: AlertDialog): Unit = sendView.manager.updateText(specs.map(_.info.lastBalance).sum.toMilliSatoshi)
      for (usedSpec <- specs) addFlowChip(title.flow, usedSpec.info.label, R.drawable.border_yellow)
      mkCheckFormNeutral(attempt, none, useMax, builder, dialog_ok, dialog_cancel, neutralRes)
    }

    lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.chainEditView.host) {
      rate = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(WalletApp.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget)

      worker = new ThrottledWork[String, GenerateTxResponse] {
        // This is a generic sending facility which may send to non-segwit, so always use a safer high dust threshold
        override def work(reason: String): GenerateTxResponse = ElectrumWallet.makeTx(specs, changeTo, pubKeyScript, sendView.manager.resultMsat.truncateToSatoshi, Map.empty, rate)
        override def error(exception: Throwable): Unit = update(feeOpt = None, showIssue = sendView.manager.resultMsat >= ElectrumWallet.params.dustLimit)
        override def process(reason: String, response: GenerateTxResponse): Unit = update(response.fee.toMilliSatoshi.asSome, showIssue = false)
      }

      override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
        updatePopupButton(getPositiveButton(alert), isEnabled = feeOpt.isDefined)
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

  def bringSendMultiBitcoinPopup(specs: Seq[WalletSpec], addressToAmount: MultiAddressParser.AddressToAmount): Unit = {
    val scriptToAmount = addressToAmount.values.firstItems.map(ElectrumWallet.addressToPubKeyScript).zip(addressToAmount.values.secondItems).toMap
    val sendView = new ChainSendView(specs, badge = None, visibilityRes = -1)
    val changeTo = ElectrumWallet.orderByImportance(specs).head

    def attempt(alert: AlertDialog): Unit =
      runInFutureProcessOnUI(ElectrumWallet.makeBatchTx(specs, changeTo, scriptToAmount, feeView.rate), onFail) { response =>
        // This may be a signing or a hardware wallet, in case if it's a hardware wallet we need additional UI action so we use this method here

        proceedConfirm(sendView, alert, response) { signedTx =>
          val desc = PlainTxDescription(addressToAmount.values.firstItems.toList)
          val broadcastFuture = broadcastTx(desc, signedTx, received = Satoshi(0L), sent = response.transferred, response.fee, incoming = 0)
          runFutureProcessOnUI(broadcastFuture, onFail) { case Some(error) => cleanFailedBroadcast(signedTx.txid, error.message) case None => }
          alert.dismiss
        }
      }

    lazy val alert = {
      val title = new TitleView(me getString dialog_send_btc_many)
      val builder = titleBodyAsViewBuilder(title.asColoredView(me walletBackground specs), sendView.body)
      for (usedSpec <- specs) addFlowChip(title.flow, usedSpec.info.label, R.drawable.border_yellow)
      mkCheckForm(attempt, none, builder, dialog_ok, dialog_cancel)
    }

    lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.chainEditView.host) {
      rate = WalletApp.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(WalletApp.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget)

      worker = new ThrottledWork[String, GenerateTxResponse] {
        override def work(reason: String): GenerateTxResponse = ElectrumWallet.makeBatchTx(specs, changeTo, scriptToAmount, rate)
        override def process(reason: String, response: GenerateTxResponse): Unit = update(response.fee.toMilliSatoshi.asSome, showIssue = false)
        override def error(exception: Throwable): Unit = update(feeOpt = None, showIssue = true)
      }

      override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
        updatePopupButton(getPositiveButton(alert), isEnabled = feeOpt.isDefined)
        super.update(feeOpt, showIssue)
      }.run
    }

    for (address \ amount <- addressToAmount.values.reverse) {
      val humanAmount = WalletApp.denom.parsedWithSignTT(amount.toMilliSatoshi, cardIn, cardZero)
      val parent = getLayoutInflater.inflate(R.layout.frag_two_sided_item, null)
      new TwoSidedItem(parent, address.short.html, humanAmount.html)
      sendView.chainEditView.host.addView(parent, 0)
    }

    // Hide address facility, we display a list of addresses instead
    setVis(isVisible = false, sendView.chainEditView.inputChain)
    feeView.update(feeOpt = None, showIssue = false)
    feeView.worker addWork "MULTI-SEND-INIT-CALL"
  }

  def bringSignDialog(title: LinearLayout, info: AddressInfo): EditText = {
    val (container, extraInputLayout, extraInput, extraOption, extraOptionText) = singleInputPopup
    mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, titleBodyAsViewBuilder(title, container), sign_sign, dialog_cancel)
    extraInputLayout.setHint(sign_message)
    showKeys(extraInput)

    setVisMany(true -> extraOptionText, true -> extraOption)
    extraOptionText.setText(sign_sig_only_info)
    extraOption.setText(sign_sig_only)

    def proceed: Unit = {
      val message = extraInput.getText.toString.trim
      val hash = drongo.crypto.Bip322.getBip322MessageHash(message)
      val messageOpt = if (extraOption.isChecked) None else Some(message)
      val scriptType = drongo.address.Address.fromString(drongoNetwork, info.identity).getScriptType
      val ecKey = drongo.crypto.ECKey.fromPrivate(info.ewt.extPrivKeyFromPub(info.pubKey).privateKey.value.toArray)
      val data = BIP322VerifyData(info.identity, ByteVector.view(hash), drongo.crypto.Bip322.signMessageBip322(scriptType, message, ecKey), messageOpt)
      goToWithValue(classOf[QRSigActivity], data)
    }

    extraInput
  }

  def paymentAdapterDataChanged: TimerTask = UITask {
    val expandHideCases = displayFullIxInfoHistory || isSearchOn || recentTxInfos.size <= allInfos.size
    setVisMany(allInfos.isEmpty -> walletCards.recoveryPhraseWarning, !expandHideCases -> expandContainer)
    paymentsAdapter.notifyDataSetChanged
  }

  def proceedConfirm(sendView: ChainSendView, alert: AlertDialog, response: GenerateTxResponse)(process: Transaction => Unit): Unit =
    sendView.specs.find(spec => spec.info.core.masterFingerprint.nonEmpty && spec.info.core.walletType == ElectrumWallet.BIP84) match {

      case Some(hwSpec) =>
        val psbt = prepareBip84Psbt(response, hwSpec)
        sendView.switchToHardwareOutgoing(alert, psbt)
        sendView.chainReaderView.onPsbt = postSignedPsbt => UITask {
          postSignedPsbt.extract orElse extractTx(response, hwSpec, postSignedPsbt) match {
            case Success(postSignedTx) if postSignedTx.txOut.toSet == response.tx.txOut.toSet =>
              val finalSendButton = sendView.chainConfirmView.chainButtonsView.chainNextButton
              finalSendButton setOnClickListener onButtonTap(process apply postSignedTx)
              sendView.switchToConfirm(alert, response)

            case Failure(failureReason) =>
              onFail(failureReason)
              alert.dismiss

            case _ =>
              onFail("Outputs mismatch")
              alert.dismiss
          }
        }

      case None =>
        // This is a signing wallet so signed response tx is a final one, we use it
        val finalSendButton = sendView.chainConfirmView.chainButtonsView.chainNextButton
        finalSendButton setOnClickListener onButtonTap(process apply response.tx)
        sendView.switchToConfirm(alert, response)
    }

  def proceedWithoutConfirm(sendView: ChainSendView, alert: AlertDialog, response: GenerateTxResponse)(process: Transaction => Unit): Unit =
    sendView.specs.find(spec => spec.info.core.masterFingerprint.nonEmpty && spec.info.core.walletType == ElectrumWallet.BIP84) match {

      case Some(hwSpec) =>
        val psbt = prepareBip84Psbt(response, hwSpec)
        sendView.switchToHardwareOutgoing(alert, psbt)
        sendView.chainReaderView.onPsbt = postSignedPsbt => UITask {
          postSignedPsbt.extract orElse extractTx(response, hwSpec, postSignedPsbt) match {
            case Success(signedTx) if signedTx.txOut.toSet == response.tx.txOut.toSet => process(signedTx)
            case Failure(failureReason) => onFail(failureReason)
            case _ => onFail("Outputs mismatch")
          }

          alert.dismiss
        }

      case None =>
        process(response.tx)
        alert.dismiss
    }

  def broadcastTx(desc: TxDescription, finalTx: Transaction, received: Satoshi, sent: Satoshi, fee: Satoshi, incoming: Int): Future[OkOrError] = {
    val txInfo = TxInfo(finalTx.toString, finalTx.txid.toHex, invalidPubKey.toString, depth = 0, received, sent, fee, seenAt = System.currentTimeMillis,
      updatedAt = System.currentTimeMillis, desc, 0L.msat, WalletApp.fiatRates.info.rates.toJson.compactPrint, incoming, doubleSpent = 0)

    WalletApp.seenTxInfos(finalTx.txid) = txInfo
    WalletApp.pendingTxInfos(finalTx.txid) = txInfo

    DbStreams.next(DbStreams.txDbStream)
    ElectrumWallet.broadcast(finalTx)
  }

  def cleanFailedBroadcast(failedTxid: ByteVector32, message: String): Unit = {
    WalletApp.pendingTxInfos.remove(failedTxid)
    WalletApp.seenTxInfos.remove(failedTxid)
    DbStreams.next(DbStreams.txDbStream)
    onFail(message)
  }

  // LNAuth

  def showAuthForm(lnUrl: LNUrl): Unit = lnUrl.k1.foreach { k1 =>
    val (successResource, actionResource) = lnUrl.authAction match {
      case "register" => (lnurl_auth_register_ok, lnurl_auth_register)
      case "auth" => (lnurl_auth_auth_ok, lnurl_auth_auth)
      case "link" => (lnurl_auth_link_ok, lnurl_auth_link)
      case _ => (lnurl_auth_login_ok, lnurl_auth_login)
    }

    val spec = LNUrlAuthSpec(lnUrl.uri.getHost, ByteVector32 fromValidHex k1)
    mkCheckFormNeutral(alert => runAnd(alert.dismiss)(doAuth), none, displayInfo,
      titleBodyAsViewBuilder(s"<big>${lnUrl.warnUri}</big>".asDefView, null),
      actionResource, dialog_cancel, dialog_info)

    def displayInfo(alert: AlertDialog): Unit = {
      val explanation = getString(lnurl_auth_info).format(lnUrl.warnUri, spec.linkingPubKey.humanFour).html
      mkCheckFormNeutral(_.dismiss, none, _ => share(spec.linkingPubKey), new AlertDialog.Builder(me).setMessage(explanation), dialog_ok, -1, dialog_share)
    }

    def doAuth: Unit = snack(contentWindow, lnUrl.warnUri.html, dialog_cancel).foreach { snack =>
      val uri = lnUrl.uri.buildUpon.appendQueryParameter("sig", spec.derSignatureHex).appendQueryParameter("key", spec.linkingPubKey)
      val level2Obs = LNUrl.level2DataResponse(uri).doOnUnsubscribe(snack.dismiss).doOnTerminate(snack.dismiss)
      val level2Sub = level2Obs.subscribe(_ => UITask(WalletApp.app quickToast successResource).run, onFail)
      val listener = onButtonTap(level2Sub.unsubscribe)
      snack.setAction(dialog_cancel, listener).show
    }
  }

  def getAddressSpec = {
    val addresses = for {
      spec <- ElectrumWallet.specs.values.toList
      description = AddressDescription(spec.info.label.asSome)
      (_, extPubKey) <- spec.data.keys.accountKeyMap ++ spec.data.keys.changeKeyMap
    } yield AddressInfo(spec.data.keys.ewt, spec.info, extPubKey, description)

    val amounts = Map.empty[ExtendedPublicKey, Satoshi]
    AddressSpec(ElectrumWallet.specs.values.flatMap(_.data.utxos).foldLeft(amounts) { case (acc, utxo) =>
      acc.updated(utxo.key, acc.getOrElse(utxo.key, 0L.sat) + utxo.item.value.sat)
    }, addresses)
  }

  def findTxInputAddress(tx: Transaction): Stream[AddressInfo] = {
    // Using stream to make computation lazy and don't overcompute
    // can be used to take the first found value

    for {
      txIn <- tx.txIn.toStream
      spec <- ElectrumWallet.specs.valuesIterator
      extPubKey <- spec.data.extPubKeyFromInput(txIn)
      description = AddressDescription(spec.info.label.asSome)
    } yield AddressInfo(spec.data.keys.ewt, spec.info, extPubKey, description)
  }

  private def drongoNetwork = ElectrumWallet.chainHash match {
    case Block.LivenetGenesisBlock.hash => drongo.Network.MAINNET
    case Block.TestnetGenesisBlock.hash => drongo.Network.TESTNET
    case _ => drongo.Network.REGTEST
  }
}
