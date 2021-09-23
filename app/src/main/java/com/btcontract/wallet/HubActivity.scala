package com.btcontract.wallet

import immortan._
import immortan.fsm._
import immortan.utils._
import android.widget._
import fr.acinq.eclair._
import immortan.crypto.Tools._
import fr.acinq.eclair.channel._
import scala.concurrent.duration._
import com.softwaremill.quicklens._
import com.btcontract.wallet.Colors._
import com.btcontract.wallet.R.string._
import scala.collection.JavaConverters._
import com.btcontract.wallet.HubActivity._
import immortan.utils.ImplicitJsonFormats._

import scala.util.{Success, Try}
import android.view.{View, ViewGroup}
import immortan.sqlite.{SQLiteData, Table}
import android.graphics.{Bitmap, BitmapFactory}
import rx.lang.scala.{Observable, Subject, Subscription}
import com.androidstudy.networkmanager.{Monitor, Tovuti}
import fr.acinq.eclair.wire.{FullPaymentTag, PaymentTagTlv}
import fr.acinq.bitcoin.{ByteVector32, Crypto, Transaction}
import fr.acinq.eclair.blockchain.{CurrentBlockCount, TxAndFee}
import immortan.ChannelMaster.{OutgoingAdds, RevealedLocalFulfills}
import fr.acinq.eclair.transactions.{LocalFulfill, RemoteFulfill, Scripts}
import com.chauthai.swipereveallayout.{SwipeRevealLayout, ViewBinderHelper}
import com.google.android.material.button.{MaterialButton, MaterialButtonToggleGroup}
import com.google.android.material.button.MaterialButtonToggleGroup.OnButtonCheckedListener
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import fr.acinq.eclair.blockchain.electrum.ElectrumEclairWallet
import org.ndeftools.util.activity.NfcReaderActivity
import concurrent.ExecutionContext.Implicits.global
import com.btcontract.wallet.BaseActivity.StringOps
import com.github.mmin18.widget.RealtimeBlurView
import androidx.recyclerview.widget.RecyclerView
import com.btcontract.wallet.utils.LocalBackup
import androidx.transition.TransitionManager
import immortan.ChannelListener.Malfunction
import com.google.common.cache.LoadingCache
import com.indicator.ChannelIndicatorLine
import androidx.appcompat.app.AlertDialog
import org.apmem.tools.layouts.FlowLayout
import android.graphics.drawable.Drawable
import android.content.pm.PackageManager
import com.ornach.nobobutton.NoboButton
import immortan.LNParams.WalletExt
import scala.concurrent.Await
import org.ndeftools.Message
import java.util.TimerTask
import android.os.Bundle


object HubActivity {
  var txInfos = new ItemsWithMemory[TxInfo]
  var paymentInfos = new ItemsWithMemory[PaymentInfo]
  var payMarketInfos = new ItemsWithMemory[LNUrlPayLink]
  var relayedPreimageInfos = new ItemsWithMemory[RelayedPreimageInfo]
  val allItems = List(txInfos, paymentInfos, relayedPreimageInfos, payMarketInfos)

  final val chainWalletStream: Subject[WalletExt] = Subject[WalletExt]
  // Run clear up method once on app start, do not re-run it every time this activity gets restarted
  lazy val markAsFailedOnce: Unit = LNParams.cm.markAsFailed(paymentInfos.lastItems, LNParams.cm.allInChannelOutgoing)

  var lastHashToReveals: Map[ByteVector32, RevealedLocalFulfills] = Map.empty
  var lastInChannelOutgoing: Map[FullPaymentTag, OutgoingAdds] = Map.empty
  var allInfos: Seq[TransactionDetails] = Nil

  def requestHostedChannel: Unit = {
    val localParams = LNParams.makeChannelParams(LNParams.chainWallets, isFunder = false, LNParams.minDustLimit)
    new HCOpenHandler(LNParams.syncParams.motherbase, randomBytes32, localParams.defaultFinalScriptPubKey, LNParams.cm) {
      // Stop automatic HC opening attempts on getting any kind of local/remote error, this won't be triggered on disconnect
      def onFailure(reason: Throwable): Unit = WalletApp.app.prefs.edit.putBoolean(WalletApp.OPEN_HC, false).commit
      def onEstablished(cs: Commitments, channel: ChannelHosted): Unit = implant(cs, channel)
    }

    def implant(cs: Commitments, channel: ChannelHosted): Unit = {
      WalletApp.app.prefs.edit.putBoolean(WalletApp.OPEN_HC, false).commit
      RemotePeerActivity.implantNewChannel(cs, channel)
    }
  }
}

class HubActivity extends NfcReaderActivity with ChanErrorHandlerActivity with ExternalDataChecker with ChoiceReceiver with ChannelListener { me =>
  private def incoming(amount: MilliSatoshi): String = WalletApp.denom.directedWithSign(incoming = amount, outgoing = 0L.msat, cardOut, cardIn, cardZero, isIncoming = true)
  private def dangerousHCRevealed(fullTag: FullPaymentTag): List[LocalFulfill] = ChannelMaster.dangerousHCRevealed(lastHashToReveals, LNParams.blockCount.get, fullTag.paymentHash).toList
  private def itemsToTags = Map(R.id.bitcoinPayments -> "bitcoinPayments", R.id.lightningPayments -> "lightningPayments", R.id.relayedPayments -> "relayedPayments", R.id.payMarketLinks -> "payMarketLinks")
  private def hasItems: Boolean = allItems.exists(_.lastItems.nonEmpty)

  private def updateLnCaches: Unit = {
    lastHashToReveals = LNParams.cm.allIncomingRevealed(LNParams.cm.allHostedCommits)
    lastInChannelOutgoing = LNParams.cm.allInChannelOutgoing
  }

  // Resource references must be lazy

  private[this] lazy val expiresInBlocks = getResources.getStringArray(R.array.expires_in_blocks)
  private[this] lazy val partsInFlight = getResources.getStringArray(R.array.parts_in_flight)
  private[this] lazy val pctCollected = getResources.getStringArray(R.array.pct_collected)
  private[this] lazy val inBlocks = getResources.getStringArray(R.array.in_blocks)
  private[this] lazy val lnSplitNotice = getString(tx_ln_notice_split)
  private[this] lazy val lnDefTitle = getString(tx_ln)

  private[this] lazy val paymentTypeIconIds = List(R.id.btcIncoming, R.id.btcOutgoing, R.id.lnIncoming, R.id.lnOutgoing, R.id.lnRouted, R.id.btcLn, R.id.lnBtc, R.id.lnOutgoing)

  private[this] lazy val bottomBlurringArea = findViewById(R.id.bottomBlurringArea).asInstanceOf[RealtimeBlurView]
  private[this] lazy val bottomActionBar = findViewById(R.id.bottomActionBar).asInstanceOf[LinearLayout]
  private[this] lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]
  private[this] lazy val itemsList = findViewById(R.id.itemsList).asInstanceOf[ListView]

  private[this] lazy val walletCards = new WalletCardsViewHolder
  private[this] val viewBinderHelper = new ViewBinderHelper
  private[this] val CHOICE_RECEIVE_TAG = "choiceReceiveTag"
  var disaplyThreshold: Long = System.currentTimeMillis
  var openListItems = Set.empty[String]

  // PAYMENT LIST

  def reloadTxInfos: Unit = txInfos.setItems(WalletApp.txDataBag.listRecentTxs(Table.DEFAULT_LIMIT.get) map WalletApp.txDataBag.toTxInfo)
  def reloadPaymentInfos: Unit = paymentInfos.setItems(LNParams.cm.payBag.listRecentPayments(Table.DEFAULT_LIMIT.get) map LNParams.cm.payBag.toPaymentInfo)
  def reloadRelayedPreimageInfos: Unit = relayedPreimageInfos.setItems(LNParams.cm.payBag.listRecentRelays(Table.DEFAULT_LIMIT.get) map LNParams.cm.payBag.toRelayedPreimageInfo)
  def reloadPayMarketInfos: Unit = payMarketInfos.setItems(WalletApp.lnUrlPayBag.listRecentLinks(Table.DEFAULT_LIMIT.get) map WalletApp.lnUrlPayBag.toLinkInfo)

  def isImportantItem: PartialFunction[TransactionDetails, Boolean] = {
    case anyFreshInfo if anyFreshInfo.updatedAt > disaplyThreshold => true
    case info: PaymentInfo => info.status == PaymentStatus.PENDING
    case info: TxInfo => !info.isConfirmed
    case _ => false
  }

  def updAllInfos: Unit = {
    val dr = LNParams.cm.delayedRefunds.asSome.filter(_.totalAmount > 0L.msat)
    val alwaysVisibleInfos = allItems.flatMap(_.lastItems filter isImportantItem)
    val itemsToDisplayMap = Map(R.id.bitcoinPayments -> txInfos, R.id.lightningPayments -> paymentInfos, R.id.relayedPayments -> relayedPreimageInfos, R.id.payMarketLinks -> payMarketInfos)
    val allVisibleInfos = if (isSearchOn) List(txInfos, paymentInfos, payMarketInfos) else walletCards.toggleGroup.getCheckedButtonIds.asScala.map(_.toInt).map(itemsToDisplayMap)
    allInfos = SemanticOrder.makeSemanticOrder(allVisibleInfos.flatMap(_.lastItems) ++ alwaysVisibleInfos ++ dr)
  }

  def loadRecent: Unit = {
    WalletApp.txDataBag.db.txWrap {
      reloadRelayedPreimageInfos
      reloadPayMarketInfos
      reloadPaymentInfos
      reloadTxInfos
      updAllInfos
    }
  }

  def loadSearch(query: String): Unit = WalletApp.txDataBag.db.txWrap {
    txInfos.lastItems = WalletApp.txDataBag.searchTransactions(query).map(WalletApp.txDataBag.toTxInfo)
    paymentInfos.lastItems = LNParams.cm.payBag.searchPayments(query).map(LNParams.cm.payBag.toPaymentInfo)
    payMarketInfos.lastItems = WalletApp.lnUrlPayBag.searchLinks(query).map(WalletApp.lnUrlPayBag.toLinkInfo)
    updAllInfos
  }

  val searchWorker: ThrottledWork[String, Unit] = new ThrottledWork[String, Unit] {
    def work(query: String): Observable[Unit] = Rx.ioQueue.map(_ => if (query.nonEmpty) loadSearch(query) else loadRecent)
    def process(userTypedQuery: String, searchLoadResultEffect: Unit): Unit = paymentAdapterDataChanged.run
  }

  val payLinkImageMemo: LoadingCache[Bytes, Bitmap] = memoize {
    bytes => BitmapFactory.decodeByteArray(bytes, 0, bytes.length)
  }

  val paymentsAdapter: BaseAdapter = new BaseAdapter {
    override def getItem(pos: Int): TransactionDetails = allInfos(pos)
    override def getItemId(position: Int): Long = position
    override def getCount: Int = allInfos.size

    override def getView(position: Int, savedView: View, parent: ViewGroup): View = getItem(position) match { case item =>
      val view = if (null == savedView) getLayoutInflater.inflate(R.layout.frag_payment_line, null) else savedView.asInstanceOf[View]
      val holder = if (null == view.getTag) new PaymentLineViewHolder(view) else view.getTag.asInstanceOf[PaymentLineViewHolder]
      if (openListItems contains item.identity) holder.expand(item) else holder.collapse(item)
      setVisMany(item.isExpandedItem -> holder.spacer, !item.isExpandedItem -> holder.spacer1)
      viewBinderHelper.bind(holder.swipeWrap, item.identity)
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
    spacer1.setZ(Float.MaxValue)

    val paymentCardContainer: View = swipeWrap.findViewById(R.id.paymentCardContainer)
    val setItemLabel: NoboButton = swipeWrap.findViewById(R.id.setItemLabel).asInstanceOf[NoboButton]
    val removeItem: NoboButton = swipeWrap.findViewById(R.id.removeItem).asInstanceOf[NoboButton]
    val shareItem: NoboButton = swipeWrap.findViewById(R.id.shareItem).asInstanceOf[NoboButton]

    val nonLinkContainer: LinearLayout = swipeWrap.findViewById(R.id.nonLinkContainer).asInstanceOf[LinearLayout]
    val detailsAndStatus: RelativeLayout = swipeWrap.findViewById(R.id.detailsAndStatus).asInstanceOf[RelativeLayout]
    val description: TextView = swipeWrap.findViewById(R.id.description).asInstanceOf[TextView]
    val statusIcon: ImageView = swipeWrap.findViewById(R.id.statusIcon).asInstanceOf[ImageView]
    val labelIcon: ImageView = swipeWrap.findViewById(R.id.labelIcon).asInstanceOf[ImageView]
    val amount: TextView = swipeWrap.findViewById(R.id.amount).asInstanceOf[TextView]
    val meta: TextView = swipeWrap.findViewById(R.id.meta).asInstanceOf[TextView]

    val linkContainer: RelativeLayout = swipeWrap.findViewById(R.id.linkContainer).asInstanceOf[RelativeLayout]
    val marketItems: FlowLayout = swipeWrap.findViewById(R.id.marketItems).asInstanceOf[FlowLayout]
    val marketLabel: TextView = swipeWrap.findViewById(R.id.marketLabel).asInstanceOf[TextView]
    val linkImage: ImageView = swipeWrap.findViewById(R.id.linkImage).asInstanceOf[ImageView]
    val linkImageWrap: View = swipeWrap.findViewById(R.id.linkImageWrap)
    itemView.setTag(this)

    val paymentTypeIconViews: List[View] = paymentTypeIconIds.map(swipeWrap.findViewById)
    val iconMap: Map[Int, View] = paymentTypeIconIds.zip(paymentTypeIconViews).toMap
    var currentDetails: TransactionDetails = _
    var lastVisibleIconId: Int = -1

    paymentCardContainer setOnClickListener onButtonTap(ractOnTap)
    setItemLabel setOnClickListener onButtonTap(doSetItemLabel)
    removeItem setOnClickListener onButtonTap(doRemoveItem)
    shareItem setOnClickListener onButtonTap(doShareItem)

    def doSetItemLabel: Unit = {
      singleInputPopup(dialog_set_record_label, title = null) { input =>
        val trimmedLabelInput = Option(input).map(trimmed).filter(_.nonEmpty)

        currentDetails match {
          case info: LNUrlPayLink => WalletApp.lnUrlPayBag.updDescription(info.description.modify(_.label).setTo(trimmedLabelInput), info.domain, info.payString)
          case info: PaymentInfo => LNParams.cm.payBag.updDescription(info.description.modify(_.label).setTo(trimmedLabelInput), info.paymentHash)
          case info: TxInfo => WalletApp.txDataBag.updDescription(info.description.modify(_.label).setTo(trimmedLabelInput), info.txid)
          case _ =>
        }
      }
    }

    def doRemoveItem: Unit = {
      def proceed: Unit = currentDetails match {
        case info: LNUrlPayLink => WalletApp.lnUrlPayBag.remove(info.payString)
        case info: PaymentInfo => LNParams.cm.payBag.removePaymentInfo(info.paymentHash)
        case _ =>
      }

      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none,
        new AlertDialog.Builder(me).setMessage(confirm_remove_item),
        dialog_ok, dialog_cancel)
    }

    def doShareItem: Unit = currentDetails match {
      case info: TxInfo => me share getString(share_chain_tx).format(info.txString)
      case info: LNUrlPayLink => me share info.payString
      case info: PaymentInfo =>
        val externalInfo = info.description.externalInfo.getOrElse("n/a")
        val report = LNParams.cm.dataBag.tryGetReport(info.paymentHash).getOrElse("n/a")
        val preimage = Some(info.preimage.toHex).filter(realPreimage => ChannelMaster.NO_PREIMAGE.toHex != realPreimage).getOrElse("n/a")
        me share getString(share_ln_payment).format(info.prExt.raw, info.paymentHash.toHex, externalInfo, info.prExt.pr.nodeId.toString, preimage, report)
      case _ =>
    }

    def ractOnTap: Unit = currentDetails match {
      case info: DelayedRefunds => showPending(info)
      case info: LNUrlPayLink => doCallPayLink(info)
      case info: PaymentInfo if info.isIncoming && info.status == PaymentStatus.PENDING && !lastHashToReveals.contains(info.paymentHash) =>
        // Intercept normal flow and show invoice if: this is an incoming, not yet fulfilled payment with no parts received
        runAnd(InputParser.value = info.prExt)(me goTo ClassNames.qrInvoiceActivityClass)
      case info: TransactionDetails =>
        TransitionManager.beginDelayedTransition(contentWindow)
        if (extraInfo.getVisibility == View.VISIBLE) collapse(info)
        else expand(info)
    }

    def getStallingCommits(localFulfills: List[LocalFulfill], info: PaymentInfo): String = {
      val hcStates = LNParams.cm.allHostedCommits.map(commits => commits.channelId -> commits).toMap
      val details = localFulfills.map(_.theirAdd.channelId).toSet.flatMap(hcStates.get).map(ChanActivity.getDetails)
      details.mkString("\n\n====\n\n")
    }

    def warnDangerousHc(info: PaymentInfo): Unit = {
      val myFulfills = dangerousHCRevealed(info.fullTag)
      val fromWallet = LNParams.chainWallets.mostFundedWallet
      val title = getString(error_hc_dangerous_state).asColoredView(R.color.buttonRed)
      val chainAddress = Await.result(fromWallet.getReceiveAddresses, atMost = 40.seconds).address2PubKey.keys.head
      val paymentAmount = WalletApp.denom.parsedWithSign(myFulfills.map(_.theirAdd.amountMsat).sum, cardOut, cardZero)
      val closestExpiry = WalletApp.app.plurOrZero(inBlocks)(myFulfills.map(_.theirAdd.cltvExpiry).min.toLong - LNParams.blockCount.get)
      val rate = LNParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(LNParams.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget)
      runFutureProcessOnUI(fromWallet.sendPreimageBroadcast(myFulfills.map(_.ourPreimage).toSet, chainAddress, rate), onCanNot)(onCan)

      def stampProof(stampTx: Transaction)(alert: AlertDialog): Unit = {
        val txOrder = SemanticOrder(id = info.identity, isParent = true, order = Long.MinValue).asSome
        val txDesc = OpReturnTxDescription(myFulfills.map(_.ourPreimage), label = None, semanticOrder = txOrder)
        val infoOrder = SemanticOrder(id = info.identity, isParent = false, order = -System.currentTimeMillis).asSome
        val infoDesc1 = info.description.modify(_.proofTxid).setTo(stampTx.txid.toHex.asSome).modify(_.semanticOrder).setTo(infoOrder)
        WalletApp.txDescriptions += Tuple2(stampTx.txid, txDesc)
        alert.dismiss

        runFutureProcessOnUI(fromWallet.commit(stampTx), onFail) {
          case true => LNParams.cm.payBag.updDescription(infoDesc1, info.paymentHash)
          case false => onFail(error = me getString error_btc_broadcast_fail)
        }
      }

      def shareDetails(alert: AlertDialog): Unit = {
        me share getStallingCommits(myFulfills, info)
        alert.dismiss
      }

      def onCan(txAndFee: TxAndFee): Unit = {
        val formattedFee = WalletApp.denom.directedWithSign(0L.msat, txAndFee.fee.toMilliSatoshi, cardOut, cardIn, cardZero, isIncoming = false)
        val msg = getString(error_hc_revealed_preimage).format(getString(error_hc_option_can_stamp).format(paymentAmount, formattedFee), paymentAmount, closestExpiry).html
        mkCheckFormNeutral(stampProof(txAndFee.tx), none, shareDetails, new AlertDialog.Builder(me).setCustomTitle(title).setMessage(msg), dialog_stamp, dialog_cancel, dialog_share)
      }

      def onCanNot(error: Throwable): Unit = {
        val msg = getString(error_hc_revealed_preimage).format(getString(error_hc_option_can_not_stamp), paymentAmount, closestExpiry).html
        mkCheckFormNeutral(_.dismiss, none, shareDetails, new AlertDialog.Builder(me).setCustomTitle(title).setMessage(msg), dialog_ok, noRes = -1, dialog_share)
      }
    }

    def retry(info: PaymentInfo): Unit = new HasTypicalChainFee {
      // When retrying we never cap max LN fee to chain because original failure may have happened because more expensive routes were omitted, but it's hard to figure that out with certainty
      private val cmd = LNParams.cm.makeSendCmd(info.prExt, info.sent, LNParams.cm.all.values.toList, info.chainFee, capLNFeeToChain = false).modify(_.split.totalSum).setTo(info.sent)
      replaceOutgoingPayment(ext = info.prExt, description = info.description, action = info.action, sentAmount = cmd.split.myPart, seenAt = info.seenAt)
      LNParams.cm.localSend(cmd)
    }

    def collapse[T <: TransactionDetails](item: T): Unit = {
      setVis(isVisible = false, extraInfo)
      extraInfo.removeAllViewsInLayout
      openListItems -= item.identity
    }

    def expand[T <: TransactionDetails](item: T): Unit = {
      setVis(isVisible = true, extraInfo)
      extraInfo.removeAllViewsInLayout
      openListItems += item.identity

      item match {
        case info: PaymentInfo =>
          val myFulfills = dangerousHCRevealed(info.fullTag)
          val amount = if (info.isIncoming) info.received else info.sent
          val outgoingFSMSpec = LNParams.cm.opm.data.payments.get(info.fullTag).map(_.data)

          val liveFeePaid = outgoingFSMSpec.map(_.usedFee).getOrElse(info.fee)
          val offChainFeePaid = WalletApp.denom.directedWithSign(0L.msat, liveFeePaid, cardOut, cardIn, cardZero, isIncoming = false)
          val onChainFeeSaved = WalletApp.denom.directedWithSign(info.chainFee - liveFeePaid, 0L.msat, cardOut, cardIn, cardZero, info.chainFee > liveFeePaid)
          val shouldDisplayFee = liveFeePaid > 0L.msat && (info.status == PaymentStatus.SUCCEEDED || info.status != PaymentStatus.ABORTED && outgoingFSMSpec.isDefined)
          val shouldRetry = info.status == PaymentStatus.ABORTED && info.description.split.isEmpty && !info.prExt.pr.isExpired

          addFlowChip(extraInfo, getString(popup_hash) format info.paymentHash.toHex.short, R.drawable.border_green, info.paymentHash.toHex.asSome)
          if (info.isIncoming && myFulfills.nonEmpty) addFlowChip(extraInfo, getString(error_hc_dangerous_state), R.drawable.border_red, _ => self warnDangerousHc info)
          for (txid <- info.description.proofTxid) addFlowChip(extraInfo, getString(popup_proof_stamp), R.drawable.border_yellow, _ => me share getStallingCommits(myFulfills, info) + "\n\n====\n\n" + txid)
          for (invoiceDescription <- info.description.externalInfo if info.description.label.nonEmpty) addFlowChip(extraInfo, invoiceDescription, R.drawable.border_blue, invoiceDescription.asSome)
          if (!info.isIncoming) addFlowChip(extraInfo, getString(popup_ln_payee) format info.prExt.pr.nodeId.toString.short, R.drawable.border_blue, info.prExt.pr.nodeId.toString.asSome)

          addFlowChip(extraInfo, getString(popup_prior_chain_balance) format WalletApp.denom.parsedWithSign(info.balanceSnapshot, cardIn, cardZero), R.drawable.border_gray)
          addFlowChip(extraInfo, getString(popup_then) format WalletApp.msatInFiatHuman(info.fiatRateSnapshot, WalletApp.fiatCode, amount, Denomination.formatFiatPrecise), R.drawable.border_gray)
          addFlowChip(extraInfo, getString(popup_now) format WalletApp.msatInFiatHuman(LNParams.fiatRates.info.rates, WalletApp.fiatCode, amount, Denomination.formatFiatPrecise), R.drawable.border_gray)
          if (!info.isIncoming && shouldDisplayFee) addFlowChip(extraInfo, getString(popup_ln_fee).format(offChainFeePaid, onChainFeeSaved), R.drawable.border_gray)
          if (shouldRetry) addFlowChip(extraInfo, getString(popup_retry), R.drawable.border_yellow, _ => self retry info)

          for (action <- info.action if info.status == PaymentStatus.SUCCEEDED) {
            def run: Unit = resolveAction(theirPreimage = info.preimage, paymentAction = action)
            addFlowChip(extraInfo, getString(popup_run_action), R.drawable.border_green, _ => run)
          }

          lastInChannelOutgoing.get(info.fullTag).map(_.maxBy(_.cltvExpiry).cltvExpiry.toLong - LNParams.blockCount.get) match {
            case Some(blocksLeft) if blocksLeft > 0 => addFlowChip(extraInfo, WalletApp.app.plurOrZero(expiresInBlocks)(blocksLeft), R.drawable.border_gray)
            case Some(blocksLeft) if blocksLeft <= 0 => addFlowChip(extraInfo, expiresInBlocks.head, R.drawable.border_red)
            case None => // Either incoming or not in channels
          }

        case info: TxInfo =>
          val amount = if (info.isIncoming) info.receivedSat.toMilliSatoshi else info.sentSat.toMilliSatoshi
          val fee = WalletApp.denom.directedWithSign(0L.msat, info.feeSat.toMilliSatoshi, cardOut, cardIn, cardZero, isIncoming = false)

          addFlowChip(extraInfo, getString(popup_explorer), R.drawable.border_green, _ => me browseTxid info.txid)
          addFlowChip(extraInfo, getString(popup_txid) format info.txidString.short, R.drawable.border_green, info.txidString.asSome)
          for (address <- info.description.toAddress) addFlowChip(extraInfo, getString(popup_to_address) format address.short, R.drawable.border_yellow, address.asSome)
          for (nodeId <- info.description.withNodeId) addFlowChip(extraInfo, getString(popup_ln_node) format nodeId.toString.short, R.drawable.border_blue, nodeId.toString.asSome)

          addFlowChip(extraInfo, getString(popup_prior_chain_balance) format WalletApp.denom.parsedWithSign(info.balanceSnapshot, cardIn, cardZero), R.drawable.border_gray)
          addFlowChip(extraInfo, getString(popup_then) format WalletApp.msatInFiatHuman(info.fiatRateSnapshot, WalletApp.fiatCode, amount, Denomination.formatFiatPrecise), R.drawable.border_gray)
          addFlowChip(extraInfo, getString(popup_now) format WalletApp.msatInFiatHuman(LNParams.fiatRates.info.rates, WalletApp.fiatCode, amount, Denomination.formatFiatPrecise), R.drawable.border_gray)
          if (!info.isIncoming) addFlowChip(extraInfo, getString(popup_chain_fee) format fee, R.drawable.border_gray)

        case info: RelayedPreimageInfo =>
          val relayedHuman = WalletApp.denom.parsedWithSign(info.relayed, cardIn, cardZero)
          addFlowChip(extraInfo, getString(popup_hash) format info.paymentHashString.short, R.drawable.border_green)
          addFlowChip(extraInfo, getString(stats_item_relayed) format relayedHuman, R.drawable.border_gray)

        case _ =>
          // Do nothing
      }
    }

    def showPending(info: DelayedRefunds): Unit = {
      val adapter = new ArrayAdapter(me, R.layout.simple_list_item_2, R.id.text1, info.txToParent.toArray) {
        override def getView(position: Int, convertView: View, parentViewGroup: ViewGroup): View = {
          val view: View = super.getView(position, convertView, parentViewGroup)
          val text1 = view.findViewById(R.id.text1).asInstanceOf[TextView]
          val text2 = view.findViewById(R.id.text2).asInstanceOf[TextView]

          getItem(position) match {
            case tx ~ _ if LNParams.blockCount.get == 0L =>
              text1.setText(incoming(tx.txOut.head.amount.toMilliSatoshi).html)
              text2.setText(inBlocks.head)

            case tx ~ Some(at) =>
              val blocksDone = LNParams.blockCount.get - at.blockHeight
              val csv = math.max(Scripts.csvTimeouts(tx).values.headOption.getOrElse(0L) - blocksDone, 0L)
              val cltv = math.max(Scripts.cltvTimeout(tx) - LNParams.blockCount.get, 0L)
              text2.setText(WalletApp.app.plurOrZero(inBlocks)(cltv + csv).html)
              text1.setText(incoming(tx.txOut.head.amount.toMilliSatoshi).html)

            case tx ~ None =>
              val csvEstimate = math.max(Scripts.csvTimeouts(tx).values.headOption.getOrElse(0L), 0L)
              val cltv = math.max(Scripts.cltvTimeout(tx) - LNParams.blockCount.get, 0L)
              text1.setText(incoming(tx.txOut.head.amount.toMilliSatoshi).html)
              text2.setText(inBlocks.last.format(cltv + csvEstimate).html)
          }

          view
        }
      }

      val list = selectorList(adapter)
      val title = getString(delayed_refunding).asDefView
      titleBodyAsViewBuilder(title, list).show
      list.setDividerHeight(0)
      list.setDivider(null)
    }

    def doCallPayLink(info: LNUrlPayLink): Unit = {
      InputParser.value = info.payLink.get
      checkExternalData(noneRunnable)
    }

    def updateDetails: Unit = currentDetails match {
      // Reusing the same view to speed the list up

      case info: RelayedPreimageInfo =>
        setVisMany(false -> labelIcon, false -> detailsAndStatus, true -> nonLinkContainer, false -> linkContainer)
        meta.setText(WalletApp.app.when(info.date, WalletApp.app.dateFormat).html)
        nonLinkContainer setBackgroundResource paymentBackground(info.fullTag)
        amount.setText(incoming(info.earned).html)
        setVisibleIcon(id = R.id.lnRouted)
        swipeWrap.setLockDrag(true)

      case info: TxInfo =>
        statusIcon setImageResource txStatusIcon(info)
        nonLinkContainer setBackgroundResource R.drawable.border_gray
        setVisMany(info.description.label.isDefined -> labelIcon, true -> detailsAndStatus, true -> nonLinkContainer, false -> linkContainer, false -> removeItem)
        amount.setText(WalletApp.denom.directedWithSign(info.receivedSat.toMilliSatoshi, info.sentSat.toMilliSatoshi, cardOut, cardIn, cardZero, info.isIncoming).html)
        description.setText(info.description.label getOrElse txDescription(info).html)
        swipeWrap.setLockDrag(false)
        setTxTypeIcon(info)
        setTxMeta(info)

      case info: PaymentInfo =>
        statusIcon setImageResource paymentStatusIcon(info)
        nonLinkContainer setBackgroundResource paymentBackground(info.fullTag)
        if (info.isIncoming) setIncomingPaymentMeta(info) else setOutgoingPaymentMeta(info)
        setVisMany(info.description.label.isDefined -> labelIcon, true -> detailsAndStatus, true -> nonLinkContainer, false -> linkContainer, true -> removeItem)
        amount.setText(WalletApp.denom.directedWithSign(info.received, info.sent, cardOut, cardIn, cardZero, info.isIncoming).html)
        description.setText(paymentDescription(info).html)
        swipeWrap.setLockDrag(false)
        setPaymentTypeIcon(info)

      case info: DelayedRefunds =>
        setVisMany(false -> labelIcon, true -> detailsAndStatus, true -> nonLinkContainer, false -> linkContainer)
        nonLinkContainer setBackgroundResource R.drawable.border_gray
        statusIcon setImageResource R.drawable.baseline_feedback_24
        amount.setText(incoming(info.totalAmount).html)
        meta.setText(getString(delayed_pending).html)
        description.setText(delayed_refunding)
        setVisibleIcon(id = R.id.lnBtc)
        swipeWrap.setLockDrag(true)

      case info: LNUrlPayLink =>
        marketItems.removeAllViewsInLayout
        setVisMany(info.imageBytes.isDefined -> linkImageWrap, info.description.label.isDefined -> marketLabel, false -> labelIcon, true -> linkContainer, false -> nonLinkContainer, true -> removeItem)
        addFlowChip(marketItems, marketLinkCaption(info).take(28), R.drawable.border_gray).setCompoundDrawablesWithIntrinsicBounds(marketLinkIcon(info), null, null, null)
        for (lastComment <- info.lastComment) addFlowChip(marketItems, lastComment, R.drawable.border_blue)
        linkContainer setBackgroundResource paymentBackground(info.description.fullTag)
        info.imageBytes.map(payLinkImageMemo.get).foreach(linkImage.setImageBitmap)
        info.description.label.foreach(marketLabel.setText)
        swipeWrap.setLockDrag(false)
      }

    def setVisibleIcon(id: Int): Unit = if (lastVisibleIconId != id) {
      iconMap.get(lastVisibleIconId).foreach(_ setVisibility View.GONE)
      iconMap.get(id).foreach(_ setVisibility View.VISIBLE)
      lastVisibleIconId = id
    }

    def marketLinkCaption(info: LNUrlPayLink): String = info.payMetaData match {
      case Success(payMeta) if payMeta.identities.nonEmpty => payMeta.identities.head
      case Success(payMeta) if payMeta.emails.nonEmpty => payMeta.emails.head
      case _ => info.payLink.get.uri.getHost
    }

    def marketLinkIcon(info: LNUrlPayLink): Drawable = info.payMetaData match {
      case Success(payMeta) if payMeta.identities.nonEmpty => getDrawable(R.drawable.baseline_perm_identity_18)
      case Success(payMeta) if payMeta.emails.nonEmpty => getDrawable(R.drawable.baseline_alternate_email_18)
      case _ => getDrawable(R.drawable.baseline_language_18)
    }

    // TX helpers

    def txDescription(transactionInfo: TxInfo): String = transactionInfo.description match {
      case plain: PlainTxDescription => plain.toAddress.map(_.short) getOrElse getString(tx_btc)
      case _: ChanRefundingTxDescription => getString(tx_description_refunding)
      case _: HtlcClaimTxDescription => getString(tx_description_htlc_claiming)
      case _: ChanFundingTxDescription => getString(tx_description_funding)
      case _: OpReturnTxDescription => getString(tx_description_op_return)
      case _: PenaltyTxDescription => getString(tx_description_penalty)
    }

    def setTxTypeIcon(info: TxInfo): Unit = info.description match {
      case _: PlainTxDescription if info.isIncoming => setVisibleIcon(id = R.id.btcIncoming)
      case _: OpReturnTxDescription => setVisibleIcon(id = R.id.btcOutgoing)
      case _: ChanRefundingTxDescription => setVisibleIcon(id = R.id.lnBtc)
      case _: ChanFundingTxDescription => setVisibleIcon(id = R.id.btcLn)
      case _: HtlcClaimTxDescription => setVisibleIcon(id = R.id.lnBtc)
      case _: PenaltyTxDescription => setVisibleIcon(id = R.id.lnBtc)
      case _ => setVisibleIcon(id = R.id.btcOutgoing)
    }

    def setTxMeta(info: TxInfo): Unit = {
      if (info.isDoubleSpent) meta setText getString(tx_state_double_spent).html
      else if (info.isConfirmed) meta setText WalletApp.app.when(info.date, WalletApp.app.dateFormat).html
      else if (info.depth > 0) meta setText getString(tx_state_confs).format(info.depth, LNParams.minDepthBlocks).html
      else meta setText pctCollected.head
    }

    def txStatusIcon(info: TxInfo): Int = {
      if (info.isConfirmed) R.drawable.baseline_done_24
      else if (info.isDoubleSpent) R.drawable.baseline_block_24
      else R.drawable.baseline_hourglass_empty_24
    }

    // LN helpers

    def paymentDescription(info: PaymentInfo): String = {
      val finalText = info.description.label.orElse(info.description.externalInfo).getOrElse(lnDefTitle)
      info.description.split.map(split => lnSplitNotice.format(split.sentRatio) + finalText).getOrElse(finalText)
    }

    def setIncomingPaymentMeta(info: PaymentInfo): Unit = {
      def receivedRatio(fsm: IncomingPaymentProcessor): Long = ratio(info.received, fsm.lastAmountIn)
      val valueHuman = LNParams.cm.inProcessors.get(info.fullTag).map(receivedRatio).map(WalletApp.app plurOrZero pctCollected)
      if (PaymentStatus.SUCCEEDED == info.status && valueHuman.isEmpty) meta setText WalletApp.app.when(info.date, WalletApp.app.dateFormat).html // Payment has been cleared
      else if (PaymentStatus.SUCCEEDED == info.status && valueHuman.isDefined) meta setText pctCollected.last.html // Notify user that we are not exactly done yet
      else meta setText valueHuman.getOrElse(pctCollected.head).html // Show either value collected so far or that we are still waiting
    }

    def setOutgoingPaymentMeta(info: PaymentInfo): Unit = {
      val activeParts = lastInChannelOutgoing.getOrElse(info.fullTag, Nil).size
      val isPendingOrBeingSent = PaymentStatus.PENDING == info.status || activeParts > 0
      if (isPendingOrBeingSent) meta setText WalletApp.app.plurOrZero(partsInFlight)(activeParts).html
      else meta setText WalletApp.app.when(info.date, WalletApp.app.dateFormat).html
    }

    def setPaymentTypeIcon(info: PaymentInfo): Unit = {
      if (info.isIncoming) setVisibleIcon(R.id.lnIncoming)
      else setOutgoingPaymentIcons(info)
    }

    def setOutgoingPaymentIcons(info: PaymentInfo): Unit = {
      setVis(view = iconMap(R.id.lnOutgoing), isVisible = true)
      setVisibleIcon(R.id.lnOutgoing)
    }

    def paymentStatusIcon(info: PaymentInfo): Int = {
      if (PaymentStatus.SUCCEEDED == info.status) R.drawable.baseline_done_24
      else if (PaymentStatus.ABORTED == info.status) R.drawable.baseline_block_24
      else R.drawable.baseline_hourglass_empty_24
    }

    def paymentBackground(fullTag: FullPaymentTag): Int = {
      if (dangerousHCRevealed(fullTag).nonEmpty) R.drawable.border_red
      else if (LNParams.cm.opm.data.payments contains fullTag) R.drawable.border_blue // An active outgoing FSM is present for this tag
      else if (LNParams.cm.inProcessors contains fullTag) R.drawable.border_blue // An active incoming FSM is present for this tag
      else if (lastInChannelOutgoing contains fullTag) R.drawable.border_blue // Payments in channel are present for this tag
      else R.drawable.border_gray
    }
  }

  // LIST CAPTION CLASS

  class WalletCardsViewHolder {
    val view: LinearLayout = getLayoutInflater.inflate(R.layout.frag_wallet_cards, null).asInstanceOf[LinearLayout]
    val recoveryPhrase: TextView = view.findViewById(R.id.recoveryPhraseWarning).asInstanceOf[TextView]
    val defaultHeader: LinearLayout = view.findViewById(R.id.defaultHeader).asInstanceOf[LinearLayout]
    val rateTeaser: TextView = view.findViewById(R.id.rateTeaser).asInstanceOf[TextView]

    val totalBalance: TextView = view.findViewById(R.id.totalBalance).asInstanceOf[TextView]
    val totalFiatBalance: TextView = view.findViewById(R.id.totalFiatBalance).asInstanceOf[TextView]
    val fiatUnitPriceAndChange: TextView = view.findViewById(R.id.fiatUnitPriceAndChange).asInstanceOf[TextView]
    val chainSyncIndicator: TextView = view.findViewById(R.id.chainSyncIndicator).asInstanceOf[TextView]
    val offlineIndicator: TextView = view.findViewById(R.id.offlineIndicator).asInstanceOf[TextView]

    val totalLightningBalance: TextView = view.findViewById(R.id.totalLightningBalance).asInstanceOf[TextView]
    val channelStateIndicators: RelativeLayout = view.findViewById(R.id.channelStateIndicators).asInstanceOf[RelativeLayout]
    val channelIndicator: ChannelIndicatorLine = view.findViewById(R.id.channelIndicator).asInstanceOf[ChannelIndicatorLine]
    val lnBalanceFiat: TextView = view.findViewById(R.id.lnBalanceFiat).asInstanceOf[TextView]

    val inFlightIncoming: TextView = view.findViewById(R.id.inFlightIncoming).asInstanceOf[TextView]
    val inFlightOutgoing: TextView = view.findViewById(R.id.inFlightOutgoing).asInstanceOf[TextView]
    val inFlightRelayed: TextView = view.findViewById(R.id.inFlightRelayed).asInstanceOf[TextView]
    val addChannelTip: ImageView = view.findViewById(R.id.addChannelTip).asInstanceOf[ImageView]

    val listCaption: RelativeLayout = view.findViewById(R.id.listCaption).asInstanceOf[RelativeLayout]
    val toggleGroup: MaterialButtonToggleGroup = view.findViewById(R.id.toggleGroup).asInstanceOf[MaterialButtonToggleGroup]
    val lightningPayments: MaterialButton = view.findViewById(R.id.lightningPayments).asInstanceOf[MaterialButton]
    val bitcoinPayments: MaterialButton = view.findViewById(R.id.bitcoinPayments).asInstanceOf[MaterialButton]
    val relayedPayments: MaterialButton = view.findViewById(R.id.relayedPayments).asInstanceOf[MaterialButton]
    val payMarketLinks: MaterialButton = view.findViewById(R.id.payMarketLinks).asInstanceOf[MaterialButton]
    val searchField: EditText = view.findViewById(R.id.searchField).asInstanceOf[EditText]
    // This means search is off at start
    searchField.setTag(false)

    val chainCards: ChainWalletCards = new ChainWalletCards(me) {
      val holder: LinearLayout = view.findViewById(R.id.chainCardsContainer).asInstanceOf[LinearLayout]
      def onModernWalletTap(wallet: ElectrumEclairWallet): Unit = onChoiceMade(CHOICE_RECEIVE_TAG, 0)
      def onLegacyWalletTap(wallet: ElectrumEclairWallet): Unit = bringLegacyChainOptions(wallet)
    }

    def resetChainCards(ext1: WalletExt): Unit = UITask {
      // Remove all existing cards and place new ones
      chainCards.holder.removeAllViewsInLayout
      chainCards.init(ext1)
    }.run

    def updateView: Unit = {
      val allChannels = LNParams.cm.all.values.take(8)
      val localInCount = LNParams.cm.inProcessors.count { case (fullTag, _) => fullTag.tag == PaymentTagTlv.FINAL_INCOMING }
      val trampolineCount = LNParams.cm.inProcessors.count { case (fullTag, _) => fullTag.tag == PaymentTagTlv.TRAMPLOINE_ROUTED }
      val localOutCount = LNParams.cm.opm.data.payments.count { case (fullTag, _) => fullTag.tag == PaymentTagTlv.LOCALLY_SENT }
      val change = LNParams.fiatRates.info.pctDifference(WalletApp.fiatCode).map(_ + "<br>").getOrElse(new String)
      val unitPriceAndChange = s"<small>$change</small>${WalletApp currentMsatInFiatHuman 100000000000L.msat}"
      fiatUnitPriceAndChange.setText(unitPriceAndChange.html)

      totalFiatBalance.setText(WalletApp.currentMsatInFiatHuman(BaseActivity.totalBalance).html)
      totalBalance.setText(WalletApp.denom.parsedWithSign(BaseActivity.totalBalance, cardIn, totalZero).html)

      val lnBalance = Channel.totalBalance(LNParams.cm.all.values)
      lnBalanceFiat.setText(WalletApp currentMsatInFiatHuman lnBalance)
      totalLightningBalance.setText(WalletApp.denom.parsedWithSign(lnBalance, cardIn, lnCardZero).html)
      channelIndicator.createIndicators(allChannels.toArray)

      setVisMany(allChannels.nonEmpty -> channelStateIndicators, allChannels.nonEmpty -> totalLightningBalance, allChannels.isEmpty -> addChannelTip)
      // We have updated chain wallet balances at this point because listener in WalletApp gets called first
      chainCards.update(LNParams.chainWallets)

      val hideAll = localInCount + trampolineCount + localOutCount == 0
      inFlightIncoming setAlpha { if (hideAll) 0F else if (localInCount > 0) 1F else 0.3F }
      inFlightOutgoing setAlpha { if (hideAll) 0F else if (localOutCount > 0) 1F else 0.3F }
      inFlightRelayed setAlpha { if (hideAll) 0F else if (trampolineCount > 0) 1F else 0.3F }
      setVis(isVisible = hideAll, lnBalanceFiat)

      inFlightIncoming.setText(localInCount.toString)
      inFlightOutgoing.setText(localOutCount.toString)
      inFlightRelayed.setText(trampolineCount.toString)
    }
  }

  // LISTENERS

  private var stateSubscription = Option.empty[Subscription]
  private var statusSubscription = Option.empty[Subscription]
  private var inFinalizedSubscription = Option.empty[Subscription]
  private var chainWalletSubscription = Option.empty[Subscription]

  private val netListener = new Monitor.ConnectivityListener {
    override def onConnectivityChanged(ct: Int, isConnected: Boolean, isFast: Boolean): Unit = UITask {
      // This will make channels SLEEPING right away instead of a bit later when we receive no Pong
      if (!isConnected) CommsTower.workers.values.foreach(_.disconnect)
      setVis(!isConnected, walletCards.offlineIndicator)
    }.run
  }

  private val chainListener = new WalletEventsListener {
    override def onChainTipKnown(event: CurrentBlockCount): Unit =
      if (WalletApp.openHc) HubActivity.requestHostedChannel

    override def onChainSyncStarted(localTip: Long, remoteTip: Long): Unit = UITask {
      setVis(isVisible = remoteTip - localTip > 2016 * 4, walletCards.chainSyncIndicator)
    }.run

    override def onChainSyncEnded(localTip: Long): Unit = UITask {
      setVis(isVisible = false, walletCards.chainSyncIndicator)
    }.run

    override def onWalletReady(event: WalletReady): Unit = {
      // First, update payments to highlight nearly expired revealed incoming now that chain tip it known
      // Second, check if any of unconfirmed chain transactions became confirmed or double-spent
      ChannelMaster.next(ChannelMaster.statusUpdateStream)

      for {
        txInfo <- txInfos.lastItems if !txInfo.isDoubleSpent && !txInfo.isConfirmed
        relatedChainWallet <- LNParams.chainWallets.findByPubKey(pub = txInfo.pubKey)
        (depth, doubleSpent) <- relatedChainWallet.doubleSpent(tx = txInfo.tx)
        if depth != txInfo.depth || doubleSpent != txInfo.isDoubleSpent
      } WalletApp.txDataBag.updStatus(txInfo.txid, depth, doubleSpent)
    }
  }

  private val fiatRatesListener = new FiatRatesListener {
    def onFiatRates(rates: FiatRatesInfo): Unit = UITask(walletCards.updateView).run
  }

  private val extraOutgoingListener = new OutgoingPaymentListener {
    override def wholePaymentFailed(data: OutgoingPaymentSenderData): Unit = UITask {
      val warnFeeCap = data.failures.exists { case local: LocalFailure => PaymentFailure.NO_ROUTES_FOUND == local.status case _ => false }
      if (WalletApp.capLNFeeToChain && warnFeeCap) snack(contentWindow, getString(settings_ln_fee_expensive_omitted), dialog_ok, _.dismiss)
    }.run

    override def gotFirstPreimage(data: OutgoingPaymentSenderData, fulfill: RemoteFulfill): Unit = UITask {
      val actionOpt = paymentInfos.lastItems.find(_.paymentHash == fulfill.ourAdd.paymentHash).flatMap(_.action)
      for (paymentAction <- actionOpt) resolveAction(fulfill.theirPreimage, paymentAction)
      Vibrator.vibrate
    }.run
  }

  // NFC

  def readEmptyNdefMessage: Unit = WalletApp.app quickToast error_nothing_useful
  def readNonNdefMessage: Unit = WalletApp.app quickToast error_nothing_useful
  def onNfcStateChange(ok: Boolean): Unit = none
  def onNfcFeatureNotFound: Unit = none
  def onNfcStateDisabled: Unit = none
  def onNfcStateEnabled: Unit = none

  def readNdefMessage(nfcMessage: Message): Unit = runInFutureProcessOnUI(InputParser recordValue ndefMessageString(nfcMessage), _ => readEmptyNdefMessage)(_ => me checkExternalData noneRunnable)

  // Chan exceptions

  override def onException: PartialFunction[Malfunction, Unit] = {
    case (CMDException(_, _: CMD_CLOSE), _, _: HasNormalCommitments) => // Swallow this specific error here, it will be displayed on StatActivity
    case (CMDException(_, _: CMD_HOSTED_STATE_OVERRIDE), _, _: HostedCommits) => // Swallow this specific error here, it will be displayed on StatActivity
    case (error: ChannelTransitionFail, _, data: HasNormalCommitments) => chanError(data.channelId, getString(error_channel_closed).format(error.stackTraceAsString), data.commitments.remoteInfo)
    case (error: ChannelTransitionFail, _, hc: HostedCommits) if hc.error.isEmpty => chanError(hc.channelId, getString(error_channel_suspended).format(error.stackTraceAsString), hc.remoteInfo)
    case (RemoteErrorException(details), _, data: HasNormalCommitments) => chanError(data.channelId, getString(error_channel_remote).format(details), data.commitments.remoteInfo)
    case (RemoteErrorException(details), _, hc: HostedCommits) if hc.error.isEmpty => chanError(hc.channelId, getString(error_channel_remote).format(details), hc.remoteInfo)
    case (error, _, data: HasNormalCommitments) => chanError(data.channelId, error.stackTraceAsString, data.commitments.remoteInfo)
    case (error, _, hc: HostedCommits) => chanError(hc.channelId, error.stackTraceAsString, hc.remoteInfo)
  }

  // Lifecycle methods

  override def onResume: Unit = {
    checkExternalData(noneRunnable)
    super.onResume
  }

  override def onDestroy: Unit = {
    stateSubscription.foreach(_.unsubscribe)
    statusSubscription.foreach(_.unsubscribe)
    inFinalizedSubscription.foreach(_.unsubscribe)
    chainWalletSubscription.foreach(_.unsubscribe)

    try LNParams.chainWallets.catcher ! WalletEventsCatcher.Remove(chainListener) catch none
    try for (channel <- LNParams.cm.all.values) channel.listeners -= me catch none
    try LNParams.cm.localPaymentListeners remove extraOutgoingListener catch none
    try LNParams.fiatRates.listeners -= fiatRatesListener catch none
    Tovuti.from(me).stop
    super.onDestroy
  }

  override def onBackPressed: Unit = {
    if (viewBinderHelper.getOpenCount > 0) viewBinderHelper.closeOthers(new String, null)
    else if (currentSnackbar.isDefined) removeCurrentSnack.run
    else if (isSearchOn) rmSearch(null)
    else super.onBackPressed
  }

  type GrantResults = Array[Int]
  override def onRequestPermissionsResult(reqCode: Int, permissions: Array[String], results: GrantResults): Unit =
    if (reqCode == scannerRequestCode && results.nonEmpty && results.head == PackageManager.PERMISSION_GRANTED)
      bringScanner(null)

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case bitcoinUri: BitcoinUri if bitcoinUri.isValid => bringSendBitcoinPopup(bitcoinUri, LNParams.chainWallets.mostFundedWallet)

    case info: RemoteNodeInfo =>
      // In case if IP address has changed
      LNParams.cm.all.values.foreach(_ process info)
      me goTo ClassNames.remotePeerActivityClass

    case prExt: PaymentRequestExt =>
      lnSendGuard(prExt, contentWindow) {
        case Some(origAmount) if prExt.splits.nonEmpty =>
          new OffChainSender(maxSendable = LNParams.cm.maxSendable(LNParams.cm.all.values).min(prExt.splitLeftover * 2), minSendable = LNParams.minPayment) {
            override def isNeutralEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat < prExt.splitLeftover - minSendable
            override def isPayEnabled: Boolean = manager.resultMsat >= prExt.splitLeftover && manager.resultMsat <= maxSendable
            override def neutral(alert: AlertDialog): Unit = proceedSplit(prExt, origAmount, alert)

            override def send(alert: AlertDialog): Unit = {
              val cmd = LNParams.cm.makeSendCmd(prExt, manager.resultMsat, LNParams.cm.all.values.toList, typicalChainTxFee, WalletApp.capLNFeeToChain).modify(_.split.totalSum).setTo(origAmount)
              val pd = PlainDescription(split = cmd.split.asSome, label = manager.resultExtraInput, semanticOrder = None, proofTxid = None, invoiceText = prExt.descriptionOrEmpty)
              replaceOutgoingPayment(prExt, pd, action = None, sentAmount = cmd.split.myPart)
              LNParams.cm.localSend(cmd)
              alert.dismiss
            }

            override val alert: AlertDialog = {
              val title = new TitleView(getString(dialog_split_ln) format prExt.brDescription)
              val builder = titleBodyAsViewBuilder(title.asColoredView(R.color.cardLightning), manager.content)
              addFlowChip(title.flow, getString(dialog_ln_requested) format WalletApp.denom.parsedWithSign(origAmount, cardIn, cardZero), R.drawable.border_blue)
              addFlowChip(title.flow, getString(dialog_ln_left) format WalletApp.denom.parsedWithSign(prExt.splitLeftover, cardIn, cardZero), R.drawable.border_blue)
              mkCheckFormNeutral(send, none, neutral, builder, dialog_ok, dialog_cancel, dialog_split)
            }

            // Prefill with what's left to pay
            manager.updateText(prExt.splitLeftover)
          }

        case Some(origAmount) =>
          new OffChainSender(maxSendable = LNParams.cm.maxSendable(LNParams.cm.all.values).min(origAmount * 2), minSendable = LNParams.minPayment) {
            override def isNeutralEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat < origAmount - minSendable
            override def isPayEnabled: Boolean = manager.resultMsat >= origAmount && manager.resultMsat <= maxSendable
            override def neutral(alert: AlertDialog): Unit = proceedSplit(prExt, origAmount, alert)
            override def send(alert: AlertDialog): Unit = baseSendNow(prExt, alert)

            override val alert: AlertDialog = {
              val totalHuman = WalletApp.denom.parsedWithSign(origAmount, cardIn, cardZero)
              val title = new TitleView(getString(dialog_send_ln) format prExt.brDescription)
              val builder = titleBodyAsViewBuilder(title.asColoredView(R.color.cardLightning), manager.content)
              addFlowChip(title.flow, getString(dialog_ln_requested).format(totalHuman), R.drawable.border_blue)
              mkCheckFormNeutral(send, none, neutral, builder, dialog_ok, dialog_cancel, dialog_split)
            }

            // Prefill with asked amount
            manager.updateText(origAmount)
          }

        case None =>
          new OffChainSender(maxSendable = LNParams.cm.maxSendable(LNParams.cm.all.values), minSendable = LNParams.minPayment) {
            override def isPayEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat <= maxSendable
            override def neutral(alert: AlertDialog): Unit = manager.updateText(maxSendable)
            override def send(alert: AlertDialog): Unit = baseSendNow(prExt, alert)
            override def isNeutralEnabled: Boolean = true

            override val alert: AlertDialog = {
              val title = getString(dialog_send_ln).format(prExt.brDescription).asColoredView(R.color.cardLightning)
              mkCheckFormNeutral(send, none, neutral, titleBodyAsViewBuilder(title, manager.content), dialog_ok, dialog_cancel, dialog_max)
            }

            // Do not prefill since amount is unknown, disable pay button
            manager.updateButton(getPositiveButton(alert), isEnabled = false)
          }
      }

    case lnUrl: LNUrl =>
      lnUrl.fastWithdrawAttempt.toOption match {
        case Some(withdraw) => bringWithdrawPopup(withdraw)
        case None if lnUrl.isAuth => showAuthForm(lnUrl)
        case None => resolveLnurl(lnUrl)
      }

    case _ =>
      whenNone.run
  }

  def resolveLnurl(lnUrl: LNUrl): Unit = {
    val resolve: PartialFunction[LNUrlData, Unit] = {
      case pay: PayRequest => bringPayPopup(pay, lnUrl).run
      case withdraw: WithdrawRequest => UITask(me bringWithdrawPopup withdraw).run
      case nc: NormalChannelRequest => runAnd(InputParser.value = nc)(me goTo ClassNames.remotePeerActivityClass)
      case hc: HostedChannelRequest => runAnd(InputParser.value = hc)(me goTo ClassNames.remotePeerActivityClass)
      case _ => nothingUsefulTask.run
    }

    val msg = getString(dialog_lnurl_processing).format(lnUrl.uri.getHost).html
    val obs = lnUrl.level1DataResponse.doOnTerminate(removeCurrentSnack.run)
    cancellingSnack(contentWindow, obs.subscribe(resolve, onFail), msg)
  }

  def showAuthForm(lnUrl: LNUrl): Unit = lnUrl.k1.foreach { k1 =>
    val linkingPrivKey = LNParams.secret.keys.makeLinkingKey(lnUrl.uri.getHost)
    val linkingPubKey = linkingPrivKey.publicKey.toString
    val dataToSign = ByteVector32.fromValidHex(k1)

    val (successRes, actionRes) = lnUrl.authAction match {
      case "register" => (lnurl_auth_register_ok, lnurl_auth_register)
      case "auth" => (lnurl_auth_auth_ok, lnurl_auth_auth)
      case "link" => (lnurl_auth_link_ok, lnurl_auth_link)
      case _ => (lnurl_auth_login_ok, lnurl_auth_login)
    }

    val title = titleBodyAsViewBuilder(s"<big>${lnUrl.uri.getHost}</big>".asColoredView(R.color.cardLightning), null)
    mkCheckFormNeutral(doAuth, none, displayInfo, title, actionRes, dialog_cancel, dialog_info)

    def displayInfo(alert: AlertDialog): Unit = {
      val explanation = getString(lnurl_auth_info).format(lnUrl.uri.getHost, linkingPubKey.humanFour).html
      mkCheckFormNeutral(_.dismiss, none, _ => share(linkingPubKey), new AlertDialog.Builder(me).setMessage(explanation), dialog_ok, -1, dialog_share)
    }

    def doAuth(alert: AlertDialog): Unit = {
      val signature = Crypto.sign(dataToSign, linkingPrivKey)
      val msg = getString(dialog_lnurl_processing).format(lnUrl.uri.getHost).html
      val uri = lnUrl.uri.buildUpon.appendQueryParameter("sig", Crypto.compact2der(signature).toHex).appendQueryParameter("key", linkingPubKey)
      val sub = LNUrl.level2DataResponse(uri).doOnTerminate(removeCurrentSnack.run).subscribe(_ => UITask(WalletApp.app quickToast successRes).run, onFail)
      cancellingSnack(contentWindow, sub, msg)
      alert.dismiss
    }
  }

  def isSearchOn: Boolean = walletCards.searchField.getTag.asInstanceOf[Boolean]

  override def onChoiceMade(tag: AnyRef, pos: Int): Unit = (tag, pos) match {
    case (legacy: ElectrumEclairWallet, 0) => transferFromLegacyToModern(legacy)
    case (legacy: ElectrumEclairWallet, 1) => bringLegacyAddressScanner(legacy)
    case (CHOICE_RECEIVE_TAG, 0) => me goTo ClassNames.qrChainActivityClass
    case (CHOICE_RECEIVE_TAG, 1) => bringReceivePopup
    case _ =>
  }

  def INIT(state: Bundle): Unit = {
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(com.btcontract.wallet.R.layout.activity_hub)
      for (channel <- LNParams.cm.all.values) channel.listeners += me
      LNParams.cm.localPaymentListeners add extraOutgoingListener
      LNParams.fiatRates.listeners += fiatRatesListener
      LNParams.chainWallets.catcher ! chainListener
      Tovuti.from(me).monitor(netListener)

      bottomActionBar post UITask {
        bottomBlurringArea.setHeightTo(bottomActionBar)
        itemsList.setPadding(0, 0, 0, bottomActionBar.getHeight)
      }

      val defaultButtons = Set("bitcoinPayments", "lightningPayments")
      val checkedButtonTags = WalletApp.getCheckedButtons(defaultButtons)

      for {
        (itemId, buttonTag) <- itemsToTags
        if checkedButtonTags.contains(buttonTag)
      } walletCards.toggleGroup.check(itemId)

      walletCards.recoveryPhrase setOnClickListener onButtonTap(viewRecoveryCode)
      walletCards.toggleGroup addOnButtonCheckedListener new OnButtonCheckedListener {
        def onButtonChecked(group: MaterialButtonToggleGroup, checkId: Int, isChecked: Boolean): Unit = {
          WalletApp.putCheckedButtons(itemsToTags.filterKeys(group.getCheckedButtonIds.contains).values.toSet)
          runAnd(disaplyThreshold = System.currentTimeMillis)(updAllInfos)
          paymentAdapterDataChanged.run
        }
      }

      itemsList.addHeaderView(walletCards.view)
      itemsList.setAdapter(paymentsAdapter)
      itemsList.setDividerHeight(0)
      itemsList.setDivider(null)

      // Fill wallet list with wallet card views here
      walletCards.chainCards.init(LNParams.chainWallets)
      walletCards.updateView

      runInFutureProcessOnUI(loadRecent, none) { _ =>
        // We suggest user to rate us if: no rate attempt has been made before, LN payments were successful, user has been using an app for certain period
        setVis(WalletApp.showRateUs && paymentInfos.lastItems.forall(_.status == PaymentStatus.SUCCEEDED) && allInfos.size > 4 && allInfos.size < 8, walletCards.rateTeaser)
        walletCards.searchField addTextChangedListener onTextChange(searchWorker.addWork)
        runAnd(updateLnCaches)(paymentAdapterDataChanged.run)
        markAsFailedOnce
      }

      val window = 600.millis
      val txEvents = ChannelMaster.txDbStream.onBackpressureLatest.doOnNext(_ => reloadTxInfos)
      val relayEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.relayDbStream, window).doOnNext(_ => reloadRelayedPreimageInfos)
      val marketEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.payMarketDbStream, window).doOnNext(_ => reloadPayMarketInfos)
      val paymentEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.paymentDbStream, window).doOnNext(_ => reloadPaymentInfos)
      val stateEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.stateUpdateStream, window).doOnNext(_ => updateLnCaches)

      stateSubscription = txEvents.merge(paymentEvents).merge(relayEvents).merge(marketEvents).merge(stateEvents).doOnNext(_ => updAllInfos).subscribe(_ => paymentAdapterDataChanged.run).asSome
      statusSubscription = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.statusUpdateStream, window).merge(stateEvents).subscribe(_ => UITask(walletCards.updateView).run).asSome
      chainWalletSubscription = chainWalletStream.subscribe(ext1 => walletCards resetChainCards ext1).asSome

      inFinalizedSubscription = ChannelMaster.inFinalized
        .collect { case _: IncomingRevealed => true }
        .subscribe(_ => Vibrator.vibrate).asSome

      timer.scheduleAtFixedRate(paymentAdapterDataChanged, 30000, 30000)
      val backupAllowed = LocalBackup.isAllowed(context = WalletApp.app)
      if (!backupAllowed) LocalBackup.askPermission(activity = me)
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }
  }

  // VIEW HANDLERS

  def bringSearch(view: View): Unit = {
    walletCards.searchField.setTag(true)
    TransitionManager.beginDelayedTransition(contentWindow)
    walletCards.searchField.setVisibility(View.VISIBLE)
    walletCards.defaultHeader.setVisibility(View.GONE)
    showKeys(walletCards.searchField)
  }

  def rmSearch(view: View): Unit = {
    walletCards.searchField.setTag(false)
    walletCards.searchField.setText(new String)
    WalletApp.app.hideKeys(walletCards.searchField)
    TransitionManager.beginDelayedTransition(contentWindow)
    walletCards.defaultHeader.setVisibility(View.VISIBLE)
    walletCards.searchField.setVisibility(View.GONE)
  }

  def bringScanner(view: View): Unit = {
    def parseTypedInput(input: String): Unit = runInFutureProcessOnUI(InputParser recordValue input, onFail) { _ =>
      // Try to parse user input, show terminal failure details, fallback to manual input if nothing useful was found
      def proceed: Unit = runAnd(switchToTypeInputOnFailure.run)(nothingUsefulTask.run)
      me checkExternalData UITask(proceed)
    }

    def switchToTypeInputOnFailure: Runnable = UITask {
      val title = getString(error_nothing_in_clipboard).asDefView
      singleInputPopup(typing_hints, title)(parseTypedInput)
    }

    val onType = UITask { singleInputPopup(typing_hints, null)(parseTypedInput) /* Has no title */ }
    val onScan = UITask { checkExternalData(nothingUsefulTask) /* Check data and tell nothing useful is found as fallback */ }
    val onPaste = UITask { checkExternalData(switchToTypeInputOnFailure) /* Check data and propose to type it as fallback */ }
    val sheet = new sheets.ScannerBottomSheet(me, None, Some(onType), onScan, onPaste)
    callScanner(sheet)
  }

  def bringLegacyAddressScanner(legacy: ElectrumEclairWallet): Unit = {
    def resolveLegacyWalletBtcAddressQr: Unit = InputParser.checkAndMaybeErase {
      case bitcoinUri: BitcoinUri if bitcoinUri.isValid => bringSendBitcoinPopup(bitcoinUri, legacy)
      case _ => nothingUsefulTask.run
    }

    val instruction = getString(scan_btc_address).asSome
    def onData: Runnable = UITask(resolveLegacyWalletBtcAddressQr)
    val sheet = new sheets.ScannerBottomSheet(me, instruction, None, onData, onData)
    callScanner(sheet)
  }

  def bringReceiveOptions(view: View): Unit = {
    val options = Array(dialog_receive_btc, dialog_receive_ln).map(getString).map(_.html)
    val list = me selectorList new ArrayAdapter(me, android.R.layout.simple_expandable_list_item_1, options)
    new sheets.ChoiceBottomSheet(list, CHOICE_RECEIVE_TAG, me).show(getSupportFragmentManager, "unused-tag")
  }

  def bringLegacyChainOptions(wallet: ElectrumEclairWallet): Unit = {
    val options = Array(dialog_legacy_transfer_btc, dialog_legacy_send_btc).map(getString)
    val list = me selectorList new ArrayAdapter(me, android.R.layout.simple_expandable_list_item_1, options)
    new sheets.ChoiceBottomSheet(list, wallet, me).show(getSupportFragmentManager, "unused-tag")
  }

  def goToStatPage(view: View): Unit = me goTo ClassNames.chanActivityClass
  def goToSettingsPage(view: View): Unit = me goTo ClassNames.settingsActivityClass

  def transferFromLegacyToModern(legacy: ElectrumEclairWallet): Unit = {
    runFutureProcessOnUI(LNParams.chainWallets.lnWallet.getReceiveAddresses, onFail) { addresses =>
      val labelAndMessage = s"?label=${me getString btc_transfer_label}&message=${me getString btc_transfer_message}"
      val uri = BitcoinUri.fromRaw(s"bitcoin:${addresses.address2PubKey.keySet.head}$labelAndMessage")
      bringSendBitcoinPopup(uri, legacy)
    }
  }

  def bringSendBitcoinPopup(uri: BitcoinUri, fromWallet: ElectrumEclairWallet): Unit = {
    val body = getLayoutInflater.inflate(R.layout.frag_input_on_chain, null).asInstanceOf[ScrollView]
    val manager = new RateManager(body, getString(dialog_add_btc_label).asSome, dialog_visibility_private, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
    val canSend = WalletApp.denom.parsedWithSign(fromWallet.info.lastBalance.toMilliSatoshi, cardIn, cardZero)
    val canSendFiat = WalletApp.currentMsatInFiatHuman(fromWallet.info.lastBalance.toMilliSatoshi)

    def switchToLn(alert: AlertDialog): Unit = {
      uri.prExt.foreach(ext => InputParser.value = ext)
      checkExternalData(noneRunnable)
      alert.dismiss
    }

    def attempt(alert: AlertDialog): Unit = {
      // On success tx will be recorded in a listener
      // on failure user will be notified right away
      alert.dismiss

      for {
        txAndFee <- fromWallet.sendPayment(manager.resultMsat.truncateToSatoshi, uri.address, feeView.rate)
        knownDescription = PlainTxDescription(uri.address :: Nil, manager.resultExtraInput orElse uri.label orElse uri.message)
        // Record this description before sending, we won't be able to know a memo and label otherwise
        _ = WalletApp.txDescriptions += Tuple2(txAndFee.tx.txid, knownDescription)
        committed <- fromWallet.commit(txAndFee.tx) if !committed
      } onFail(me getString error_btc_broadcast_fail)
    }

    lazy val alert = {
      val neutralRes = if (uri.amount.isDefined) -1 else dialog_max
      val label = uri.label.map(label => s"<br><br><b>$label</b>").getOrElse(new String)
      val message = uri.message.map(message => s"<br><i>$message<i>").getOrElse(new String)
      val caption = getString(dialog_send_btc).format(uri.address.short, label + message)

      val title = new TitleView(caption)
      val backgroundRes = if (fromWallet.info.core.isRemovable) R.color.cardBitcoinLegacy else R.color.cardBitcoinModern
      addFlowChip(title.flow, getString(dialog_send_btc_from).format(fromWallet.info.label), R.drawable.border_yellow)

      val builder = titleBodyAsViewBuilder(title.asColoredView(backgroundRes), manager.content)
      def useMax(alert: AlertDialog): Unit = manager.updateText(fromWallet.info.lastBalance.toMilliSatoshi)
      if (uri.prExt.isEmpty) mkCheckFormNeutral(attempt, none, useMax, builder, dialog_ok, dialog_cancel, neutralRes)
      else mkCheckFormNeutral(attempt, none, switchToLn, builder, dialog_ok, dialog_cancel, lightning_wallet)
    }

    lazy val feeView: FeeView[TxAndFee] = new FeeView[TxAndFee](body) {
      override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
        manager.updateButton(getPositiveButton(alert), feeOpt.isDefined)
        super.update(feeOpt, showIssue)
      }.run

      rate = {
        val target = LNParams.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget
        LNParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(target)
      }

      worker = new ThrottledWork[String, TxAndFee] {
        def work(reason: String): Observable[TxAndFee] = Rx fromFutureOnIo fromWallet.sendPayment(manager.resultMsat.truncateToSatoshi, uri.address, rate)
        def process(reason: String, txAndFee: TxAndFee): Unit = update(feeOpt = txAndFee.fee.toMilliSatoshi.asSome, showIssue = false)
        override def error(exc: Throwable): Unit = update(feeOpt = None, showIssue = manager.resultMsat >= LNParams.minDustLimit)
      }
    }

    manager.hintDenom.setText(getString(dialog_up_to).format(canSend).html)
    manager.hintFiatDenom.setText(getString(dialog_up_to).format(canSendFiat).html)
    manager.inputAmount addTextChangedListener onTextChange(feeView.worker.addWork)
    feeView.update(feeOpt = None, showIssue = false)

    uri.amount.foreach { asked =>
      manager.updateText(value = asked)
      manager.inputAmount.setEnabled(false)
      manager.fiatInputAmount.setEnabled(false)
    }
  }

  def bringReceivePopup: Unit = lnReceiveGuard(contentWindow) {
    new OffChainReceiver(initMaxReceivable = Long.MaxValue.msat, initMinReceivable = 0L.msat) {
      override def getManager: RateManager = new RateManager(body, getString(dialog_add_description).asSome, dialog_visibility_public, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
      override def getDescription: PaymentDescription = PlainDescription(split = None, label = None, semanticOrder = None, proofTxid = None, invoiceText = manager.resultExtraInput getOrElse new String)
      override def processInvoice(prExt: PaymentRequestExt): Unit = runAnd(InputParser.value = prExt)(me goTo ClassNames.qrInvoiceActivityClass)
      override def getTitleText: String = getString(dialog_receive_ln)
    }
  }

  def bringWithdrawPopup(data: WithdrawRequest): Unit = lnReceiveGuard(contentWindow) {
    new OffChainReceiver(initMaxReceivable = data.maxWithdrawable.msat, initMinReceivable = data.minCanReceive) {
      override def getManager: RateManager = new RateManager(body, getString(dialog_add_ln_label).asSome, dialog_visibility_private, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
      override def getDescription: PaymentDescription = PlainMetaDescription(split = None, label = manager.resultExtraInput, semanticOrder = None, proofTxid = None, invoiceText = new String, meta = data.descriptionOrEmpty)
      override def getTitleText: String = getString(dialog_lnurl_withdraw).format(data.callbackUri.getHost, data.brDescription)
      override def processInvoice(prExt: PaymentRequestExt): Unit = data.requestWithdraw(prExt).foreach(none, onFail)
    }
  }

  def bringPayPopup(data: PayRequest, lnUrl: LNUrl): TimerTask = UITask {
    new OffChainSender(maxSendable = LNParams.cm.maxSendable(LNParams.cm.all.values).min(data.maxSendable.msat), minSendable = Denomination.satCeil(data.minSendable.msat) max LNParams.minPayment) {
      override lazy val manager: RateManager = new RateManager(body, data.commentAllowed.map(_ => me getString dialog_add_comment), dialog_visibility_public, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
      override def isNeutralEnabled: Boolean = manager.resultMsat >= LNParams.minPayment && manager.resultMsat <= minSendable - LNParams.minPayment
      override def isPayEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat <= maxSendable

      private def getComment = for {
        maxLength <- data.commentAllowed
        comment <- manager.resultExtraInput
      } yield comment.take(maxLength)

      override def neutral(alert: AlertDialog): Unit = {
        def proceed(pf: PayRequestFinal): TimerTask = UITask {
          lnSendGuard(pf.prExt, container = contentWindow) { _ =>
            // This LN payment is related to LNURL-PAY link, more recent payments are displayed higher under link
            val semanticOrder = SemanticOrder(id = lnUrl.request, isParent = false, order = -System.currentTimeMillis)
            val cmd = LNParams.cm.makeSendCmd(pf.prExt, manager.resultMsat, LNParams.cm.all.values.toList, typicalChainTxFee, WalletApp.capLNFeeToChain).modify(_.split.totalSum).setTo(minSendable)
            val pd = PlainMetaDescription(cmd.split.asSome, label = None, semanticOrder = semanticOrder.asSome, proofTxid = None, invoiceText = new String, meta = data.meta.textPlain)
            InputParser.value = SplitParams(pf.prExt, pf.successAction, pd, cmd, typicalChainTxFee)
            runAnd(alert.dismiss)(me goTo ClassNames.qrSplitActivityClass)
          }
        }

//        val obs = getFinal(minSendable).doOnTerminate(removeCurrentSnack.run)
//        val msg = getString(dialog_lnurl_splitting).format(data.callbackUri.getHost).html
//        cancellingSnack(contentWindow, obs.subscribe(prf => proceed(prf).run, onFail), msg)
      }

      override def send(alert: AlertDialog): Unit = {
        def proceed(pf: PayRequestFinal): TimerTask = UITask {
          lnSendGuard(pf.prExt, container = contentWindow) { _ =>
            // This LN payment is related to LNURL-PAY link, more recent payments are displayed higher under link
            val semanticOrder = SemanticOrder(id = lnUrl.request, isParent = false, order = -System.currentTimeMillis)
            val cmd = LNParams.cm.makeSendCmd(pf.prExt, manager.resultMsat, LNParams.cm.all.values.toList, typicalChainTxFee, WalletApp.capLNFeeToChain).modify(_.split.totalSum).setTo(manager.resultMsat)
            val pd = PlainMetaDescription(split = None, label = None, semanticOrder = semanticOrder.asSome, proofTxid = None, invoiceText = new String, meta = data.meta.textPlain)
            replaceOutgoingPayment(pf.prExt, pd, pf.successAction, sentAmount = cmd.split.myPart)
            LNParams.cm.localSend(cmd)

            if (!pf.isThrowAway) {
              val desc = LNUrlDescription(label = None, semanticOrder.copy(isParent = true, order = Long.MinValue).asSome, pf.prExt.pr.paymentHash, pf.prExt.pr.paymentSecret.get, manager.resultMsat)
              val info = LNUrlPayLink(domain = lnUrl.uri.getHost, payString = lnUrl.request, data.metadata, System.currentTimeMillis, desc, pf.prExt.pr.nodeId.toString, getComment getOrElse new String)
              WalletApp.lnUrlPayBag.saveLink(info)
            }
          }
        }

//        val obs = getFinal(manager.resultMsat).doOnTerminate(removeCurrentSnack.run)
//        val amountHuman = WalletApp.denom.parsedWithSign(manager.resultMsat, cardIn, cardZero).html
//        val msg = getString(dialog_lnurl_sending).format(amountHuman, data.callbackUri.getHost).html
//        cancellingSnack(contentWindow, obs.subscribe(prf => proceed(prf).run, onFail), msg)
//        alert.dismiss
      }

      override val alert: AlertDialog = {
        val text = getString(dialog_lnurl_pay).format(data.callbackUri.getHost, s"<br><br>${data.meta.textPlain}")
        val title = titleBodyAsViewBuilder(text.asColoredView(R.color.cardLightning), manager.content)
        mkCheckFormNeutral(send, none, neutral, title, dialog_ok, dialog_cancel, dialog_split)
      }

      private def getFinal(amount: MilliSatoshi) = LNUrl.level2DataResponse {
        data.callbackUri.buildUpon
      }
//        data.requestFinal(getComment, amount).map { rawResponse =>
//          val payRequestFinal: PayRequestFinal = to[PayRequestFinal](rawResponse)
//          val descriptionHashOpt: Option[ByteVector32] = payRequestFinal.prExt.pr.description.right.toOption
//          require(descriptionHashOpt.contains(data.metaDataHash), s"Metadata hash mismatch, original=${data.metaDataHash}, provided=$descriptionHashOpt")
//          require(payRequestFinal.prExt.pr.amount.contains(amount), s"Payment amount mismatch, requested=$amount, provided=${payRequestFinal.prExt.pr.amount}")
//          payRequestFinal.modify(_.successAction.each.domain).setTo(data.callbackUri.getHost.asSome)
//        }

      // Prefill with min possible
      manager.updateText(minSendable)
    }
  }

  def paymentAdapterDataChanged: TimerTask = UITask {
    if (txInfos.lastDelta > 0) walletCards.bitcoinPayments.setText(s"+${txInfos.lastDelta}")
    if (paymentInfos.lastDelta > 0) walletCards.lightningPayments.setText(s"+${paymentInfos.lastDelta}")
    if (payMarketInfos.lastDelta > 0) walletCards.payMarketLinks.setText(s"+${payMarketInfos.lastDelta}")
    if (relayedPreimageInfos.lastDelta > 0) walletCards.relayedPayments.setText(s"+${relayedPreimageInfos.lastDelta}")
    setVisMany(relayedPreimageInfos.lastItems.nonEmpty -> walletCards.relayedPayments, payMarketInfos.lastItems.nonEmpty -> walletCards.payMarketLinks)
    setVisMany(!hasItems -> walletCards.recoveryPhrase, hasItems -> walletCards.listCaption)
    paymentsAdapter.notifyDataSetChanged
  }

  // Payment actions

  def resolveAction(theirPreimage: ByteVector32, paymentAction: PaymentAction): Unit = paymentAction match {
    case data: MessageAction => mkCheckFormNeutral(_.dismiss, none, _ => share(data.message), actionPopup(data.finalMessage.html, data), dialog_ok, noRes = -1, dialog_share)
    case data: UrlAction => mkCheckFormNeutral(_ => browse(data.url), none, _ => share(data.url), actionPopup(data.finalMessage.html, data), dialog_open, dialog_cancel, dialog_share)
    case data: AESAction => showAesAction(theirPreimage, data) getOrElse mkCheckForm(_.dismiss, none, actionPopup(getString(dialog_lnurl_decrypt_fail), data), dialog_ok, noRes = -1)
  }

  private def showAesAction(preimage: ByteVector32, aes: AESAction) = Try {
    val secret = SQLiteData byteVecToString AES.decode(data = aes.ciphertextBytes, key = preimage.toArray, initVector = aes.ivBytes)
    val msg = if (secret.length > 36) s"${aes.finalMessage}<br><br><tt>$secret</tt><br>" else s"${aes.finalMessage}<br><br><tt><big>$secret</big></tt><br>"
    mkCheckFormNeutral(_.dismiss, none, _ => share(secret), actionPopup(msg.html, aes), dialog_ok, dialog_cancel, dialog_share)
  }

  private def actionPopup(msg: CharSequence, action: PaymentAction) = {
    val fromVendor = action.domain.map(site => s"<br><br><b>$site</b>").getOrElse(new String)
    val title = getString(dialog_lnurl_from_vendor).format(fromVendor).asDefView
    new AlertDialog.Builder(me).setCustomTitle(title).setMessage(msg)
  }
}
