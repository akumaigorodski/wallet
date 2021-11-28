package com.btcontract.wallet

import java.net.InetSocketAddress
import java.util.TimerTask

import android.content.pm.PackageManager
import android.graphics.drawable.Drawable
import android.graphics.{Bitmap, BitmapFactory}
import android.os.Bundle
import android.view.{View, ViewGroup}
import android.widget._
import androidx.appcompat.app.AlertDialog
import androidx.recyclerview.widget.RecyclerView
import androidx.transition.TransitionManager
import com.btcontract.wallet.BaseActivity.StringOps
import com.btcontract.wallet.Colors._
import com.btcontract.wallet.HubActivity._
import com.btcontract.wallet.R.string._
import com.btcontract.wallet.utils.LocalBackup
import com.chauthai.swipereveallayout.{SwipeRevealLayout, ViewBinderHelper}
import com.github.mmin18.widget.RealtimeBlurView
import com.google.android.material.button.MaterialButtonToggleGroup.OnButtonCheckedListener
import com.google.android.material.button.{MaterialButton, MaterialButtonToggleGroup}
import com.google.common.cache.LoadingCache
import com.indicator.ChannelIndicatorLine
import com.ornach.nobobutton.NoboButton
import com.softwaremill.quicklens._
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.CurrentBlockCount
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{GenerateTxResponse, RBFResponse, WalletReady}
import fr.acinq.eclair.blockchain.electrum.{ElectrumEclairWallet, ElectrumWallet}
import fr.acinq.eclair.blockchain.fee.FeeratePerByte
import fr.acinq.eclair.channel._
import fr.acinq.eclair.router.Router
import fr.acinq.eclair.transactions.{LocalFulfill, RemoteFulfill, Scripts}
import fr.acinq.eclair.wire.{FullPaymentTag, NodeAnnouncement, PaymentTagTlv, UnknownNextPeer}
import immortan.ChannelListener.Malfunction
import immortan.ChannelMaster.{OutgoingAdds, RevealedLocalFulfills}
import immortan.PathFinder.{ExpectedRouteFees, GetExpectedRouteFees}
import immortan._
import immortan.crypto.CanBeRepliedTo
import immortan.crypto.Tools._
import immortan.fsm._
import immortan.sqlite.SQLiteData
import immortan.utils.ImplicitJsonFormats._
import immortan.utils._
import org.apmem.tools.layouts.FlowLayout
import org.ndeftools.Message
import org.ndeftools.util.activity.NfcReaderActivity
import rx.lang.scala.{Observable, Subscription}
import spray.json._

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Success, Try}


object HubActivity {
  var txInfos: Iterable[TxInfo] = Nil
  var paymentInfos: Iterable[PaymentInfo] = Nil
  var lnUrlPayLinks: Iterable[LNUrlPayLink] = Nil
  var relayedPreimageInfos: Iterable[RelayedPreimageInfo] = Nil
  // Run clear up method once on app start, do not re-run it every time this activity gets restarted
  lazy val markAsFailedOnce: Unit = LNParams.cm.markAsFailed(paymentInfos, LNParams.cm.allInChannelOutgoing)
  val disaplyThreshold: Long = System.currentTimeMillis

  var lastHostedReveals: Map[ByteVector32, RevealedLocalFulfills] = Map.empty
  var lastInChannelOutgoing: Map[FullPaymentTag, OutgoingAdds] = Map.empty
  var allInfos: Seq[TransactionDetails] = Nil
  var instance: HubActivity = _

  def requestHostedChannel: Unit = {
    val localParams = LNParams.makeChannelParams(isFunder = false, LNParams.minChanDustLimit)
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

  def dangerousHCRevealed(fullTag: FullPaymentTag): List[LocalFulfill] = ChannelMaster.dangerousHCRevealed(lastHostedReveals, LNParams.blockCount.get, fullTag.paymentHash).toList
  def itemsToTags: Map[Int, String] = Map(R.id.bitcoinPayments -> "bitcoinPayments", R.id.lightningPayments -> "lightningPayments", R.id.relayedPayments -> "relayedPayments", R.id.payMarketLinks -> "payMarketLinks")
  def incoming(amount: MilliSatoshi): String = WalletApp.denom.directedWithSign(incoming = amount, outgoing = 0L.msat, cardOut, cardIn, cardZero, isIncoming = true)
  def hasItems: Boolean = txInfos.nonEmpty || paymentInfos.nonEmpty || lnUrlPayLinks.nonEmpty || relayedPreimageInfos.nonEmpty
}

class HubActivity extends NfcReaderActivity with ChanErrorHandlerActivity with ExternalDataChecker with ChoiceReceiver with ChannelListener with CanBeRepliedTo { me =>
  private[this] lazy val expiresInBlocks = getResources.getStringArray(R.array.expires_in_blocks)
  private[this] lazy val partsInFlight = getResources.getStringArray(R.array.parts_in_flight)
  private[this] lazy val pctCollected = getResources.getStringArray(R.array.pct_collected)
  private[this] lazy val inBlocks = getResources.getStringArray(R.array.in_blocks)
  private[this] lazy val lnSplitNotice = getString(tx_ln_notice_split)
  private[this] lazy val lnDefTitle = getString(tx_ln)

  private[this] lazy val paymentTypeIconIds =
    List(R.id.btcIncoming, R.id.btcInBoosted, R.id.btcOutBoosted, R.id.btcOutCancelled, R.id.btcOutgoing,
      R.id.lnIncoming, R.id.lnOutgoing, R.id.lnRouted, R.id.btcLn, R.id.lnBtc, R.id.lnOutgoing)

  private[this] lazy val bottomBlurringArea = findViewById(R.id.bottomBlurringArea).asInstanceOf[RealtimeBlurView]
  private[this] lazy val bottomActionBar = findViewById(R.id.bottomActionBar).asInstanceOf[LinearLayout]
  private[this] lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]
  private[this] lazy val itemsList = findViewById(R.id.itemsList).asInstanceOf[ListView]

  lazy val walletCards = new WalletCardsViewHolder
  private[this] val viewBinderHelper = new ViewBinderHelper
  private[this] val CHOICE_RECEIVE_TAG: String = "choiceReceiveTag"
  var openListItems = Set.empty[String]

  private def updateLnCaches: Unit = {
    // Calling these functions on each payment card would be too much computation, hence they are cached
    lastHostedReveals = LNParams.cm.allHostedCommits.flatMap(_.revealedFulfills).groupBy(_.theirAdd.paymentHash)
    lastInChannelOutgoing = LNParams.cm.allInChannelOutgoing
  }

  // PAYMENT LIST

  def reloadTxInfos: Unit = txInfos = WalletApp.txDataBag.listRecentTxs(20).map(WalletApp.txDataBag.toTxInfo)
  def reloadPaymentInfos: Unit = paymentInfos = LNParams.cm.payBag.listRecentPayments(20).map(LNParams.cm.payBag.toPaymentInfo)
  def reloadRelayedPreimageInfos: Unit = relayedPreimageInfos = LNParams.cm.payBag.listRecentRelays(20).map(LNParams.cm.payBag.toRelayedPreimageInfo)
  def reloadPayMarketInfos: Unit = lnUrlPayLinks = WalletApp.lnUrlPayBag.listRecentLinks(20).map(WalletApp.lnUrlPayBag.toLinkInfo)

  def isImportantItem: PartialFunction[TransactionDetails, Boolean] = {
    case anyFreshInfo if anyFreshInfo.updatedAt > disaplyThreshold => true
    case info: PaymentInfo => info.status == PaymentStatus.PENDING
    case info: TxInfo => !info.isConfirmed && !info.isDoubleSpent
    case _ => false
  }

  def updAllInfos: Unit = UITask {
    val exceptRouted = List(txInfos, paymentInfos, lnUrlPayLinks)
    val dr = LNParams.cm.delayedRefunds.asSome.filter(_.totalAmount > 0L.msat)
    val itemsToDisplayMap = Map(R.id.bitcoinPayments -> txInfos, R.id.lightningPayments -> paymentInfos, R.id.relayedPayments -> relayedPreimageInfos, R.id.payMarketLinks -> lnUrlPayLinks)
    val allVisibleInfos = if (isSearchOn) exceptRouted else walletCards.toggleGroup.getCheckedButtonIds.asScala.map(_.toInt).map(itemsToDisplayMap)
    allInfos = SemanticOrder.makeSemanticOrder(allVisibleInfos.flatten ++ exceptRouted.flatMap(_ filter isImportantItem) ++ dr)
  }.run

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
    txInfos = WalletApp.txDataBag.searchTransactions(query).map(WalletApp.txDataBag.toTxInfo)
    paymentInfos = LNParams.cm.payBag.searchPayments(query).map(LNParams.cm.payBag.toPaymentInfo)
    lnUrlPayLinks = WalletApp.lnUrlPayBag.searchLinks(query).map(WalletApp.lnUrlPayBag.toLinkInfo)
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
    val marketLabel: TextView = swipeWrap.findViewById(R.id.marketLabel).asInstanceOf[TextView]
    val marketInfo: TextView = swipeWrap.findViewById(R.id.marketInfo).asInstanceOf[TextView]
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

    // MENU BUTTONS

    def doViewInvoice(info: PaymentInfo): Unit = goToWithValue(ClassNames.qrInvoiceActivityClass, info.prExt)
    def doCallPayLink(info: LNUrlPayLink): Unit = runAnd(InputParser.value = info.payLink.get)(me checkExternalData noneRunnable)

    def doSetItemLabel: Unit = {
      val (container, extraInputLayout, extraInput) = singleInputPopup
      val builder = titleBodyAsViewBuilder(title = null, body = container)
      mkCheckForm(proceed, none, builder, dialog_ok, dialog_cancel)
      extraInputLayout.setHint(dialog_set_label)
      showKeys(extraInput)

      def proceed(alert: AlertDialog): Unit = runAnd(alert.dismiss) {
        val optionalInput = Option(extraInput.getText.toString).map(trimmed).filter(_.nonEmpty)

        currentDetails match {
          case info: LNUrlPayLink => WalletApp.lnUrlPayBag.updDescription(info.description.copy(label = optionalInput), info.domain, info.payString)
          case info: PaymentInfo => LNParams.cm.payBag.updDescription(info.description.copy(label = optionalInput), info.paymentHash)
          case info: TxInfo => WalletApp.txDataBag.updDescription(info.description.withNewLabel(optionalInput), info.txid)
          case _ =>
        }
      }
    }

    def doRemoveItem: Unit = {
      def proceed: Unit = currentDetails match {
        case info: LNUrlPayLink => WalletApp.lnUrlPayBag.remove(info.payString)
        case info: PaymentInfo => LNParams.cm.payBag.removePaymentInfo(info.paymentHash)
        case _ => // Other items are not removable currently
      }

      val builder = new AlertDialog.Builder(me).setMessage(confirm_remove_item)
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)
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
      case info: TransactionDetails =>
        val isVisible = extraInfo.getVisibility == View.VISIBLE
        if (isVisible) collapse(info) else expand(info)
    }

    // DANGEROUS HC STUFF

    def getStallingCommits(localFulfills: List[LocalFulfill], info: PaymentInfo): String = {
      val hcStates = LNParams.cm.allHostedCommits.map(commits => commits.channelId -> commits).toMap
      val affectedHcStates = localFulfills.map(_.theirAdd.channelId).toSet.flatMap(hcStates.get)
      val details = for (hc <- affectedHcStates) yield ChanActivity.getDetails(hc, "n/a")
      details.mkString("\n\n====\n\n")
    }

    def warnDangerousHc(info: PaymentInfo): Unit = {
      val myFulfills = dangerousHCRevealed(info.fullTag)
      val title = getString(error_hc_dangerous_state).asColoredView(R.color.buttonRed)
      val paymentAmount = WalletApp.denom.parsedWithSign(myFulfills.map(_.theirAdd.amountMsat).sum, cardOut, cardZero)
      val closestExpiry = WalletApp.app.plurOrZero(myFulfills.map(_.theirAdd.cltvExpiry.underlying).min - LNParams.blockCount.get, inBlocks)

      def stampProof(tx: Transaction)(alert: AlertDialog): Unit = {
        val txOrder = SemanticOrder(id = info.identity, order = Long.MinValue).asSome
        val infoOrder = SemanticOrder(id = info.identity, order = -System.currentTimeMillis).asSome
        val infoDesc1 = info.description.modify(_.proofTxid).setTo(tx.txid.toHex.asSome).modify(_.semanticOrder).setTo(infoOrder)
        WalletApp.txDescriptions(tx.txid) = OpReturnTxDescription(myFulfills.map(_.ourPreimage), label = None, semanticOrder = txOrder)
        alert.dismiss

        runFutureProcessOnUI(notifyAndBroadcast(LNParams.chainWallets.lnWallet, tx), onFail) {
          case true => LNParams.cm.payBag.updDescription(infoDesc1, info.paymentHash)
          case false => onFail(error = me getString error_btc_broadcast_fail)
        }
      }

      def shareDetails(alert: AlertDialog): Unit = {
        me share getStallingCommits(myFulfills, info)
        alert.dismiss
      }

      def onCan(response: GenerateTxResponse): Unit = {
        val formattedFee = WalletApp.denom.directedWithSign(0L.msat, response.fee.toMilliSatoshi, cardOut, cardIn, cardZero, isIncoming = false)
        val msg = getString(error_hc_revealed_preimage).format(getString(error_hc_option_can_stamp).format(paymentAmount, formattedFee), paymentAmount, closestExpiry).html
        mkCheckFormNeutral(stampProof(response.tx), none, shareDetails, new AlertDialog.Builder(me).setCustomTitle(title).setMessage(msg), dialog_stamp, dialog_cancel, dialog_share)
      }

      def onCanNot(error: Throwable): Unit = {
        val msg = getString(error_hc_revealed_preimage).format(getString(error_hc_option_can_not_stamp), paymentAmount, closestExpiry).html
        mkCheckFormNeutral(_.dismiss, none, shareDetails, new AlertDialog.Builder(me).setCustomTitle(title).setMessage(msg), dialog_ok, noRes = -1, dialog_share)
      }

      val chainAddress = Await.result(LNParams.chainWallets.lnWallet.getReceiveAddresses, atMost = 40.seconds).firstAccountAddress
      val rate = LNParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(LNParams.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget)
      runFutureProcessOnUI(LNParams.chainWallets.lnWallet.sendPreimageBroadcast(myFulfills.map(_.ourPreimage).toSet, LNParams.addressToPubKeyScript(chainAddress), rate), onCanNot)(onCan)
    }

    // PENDING CHANNEL REFUNDS

    def showPending(info: DelayedRefunds): Unit = {
      val adapter = new ArrayAdapter(me, R.layout.frag_delayed_refunds, R.id.text1, info.txToParent.toArray) {
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
              text2.setText(WalletApp.app.plurOrZero(cltv + csv, inBlocks).html)
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
      val title = getString(delayed_refunding_ext).asDefView
      titleBodyAsViewBuilder(title, list).show
      list.setDividerHeight(0)
      list.setDivider(null)
    }

    // CPFP / RBF

    def boostCPFP(info: TxInfo): Unit = LNParams.chainWallets.findByPubKey(info.pubKey) match {
      case None => snack(contentWindow, getString(error_btc_no_wallet).html, dialog_ok, _.dismiss)
      case Some(fromWallet) => doBoostCPFP(fromWallet, info)
    }

    def doBoostCPFP(fromWallet: ElectrumEclairWallet, info: TxInfo): Unit = {
      val fromOutPoints = for (txOutputIndex <- info.tx.txOut.indices) yield OutPoint(info.tx.hash, txOutputIndex)
      val chainAddress = Await.result(LNParams.chainWallets.lnWallet.getReceiveAddresses, atMost = 40.seconds).firstAccountAddress
      val chainPubKeyScript = LNParams.addressToPubKeyScript(chainAddress)
      val receivedMsat = info.receivedSat.toMilliSatoshi

      val body = getLayoutInflater.inflate(R.layout.frag_input_cpfp, null).asInstanceOf[ScrollView]
      val cpfpCurrent = new TwoSidedItem(body.findViewById(R.id.cpfpCurrent), getString(tx_cpfp_current), new String)
      val cpfpAfter = new TwoSidedItem(body.findViewById(R.id.cpfpAfter), getString(tx_cpfp_after), new String)

      val blockTarget = LNParams.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val target = LNParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(target), body) {
        rate = target

        worker = new ThrottledWork[String, GenerateTxResponse] {
          def work(reason: String): Observable[GenerateTxResponse] = Rx fromFutureOnIo fromWallet.makeCPFP(fromOutPoints.toSet, chainPubKeyScript, rate)
          def process(reason: String, response: GenerateTxResponse): Unit = update(feeOpt = response.fee.toMilliSatoshi.asSome, showIssue = false)
          override def error(exc: Throwable): Unit = update(feeOpt = None, showIssue = true)
        }

        override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
          val currentAmount = WalletApp.denom.directedWithSign(incoming = receivedMsat, outgoing = 0L.msat, cardOut, cardIn, cardZero, isIncoming = true)
          val afterAmount = WalletApp.denom.directedWithSign(feeOpt.map(receivedMsat.-).getOrElse(receivedMsat), 0L.msat, cardOut, cardIn, cardZero, isIncoming = true)
          updatePopupButton(getPositiveButton(alert), feeOpt.isDefined)
          cpfpCurrent.secondItem.setText(currentAmount.html)
          cpfpAfter.secondItem.setText(afterAmount.html)
          super.update(feeOpt, showIssue)
        }.run
      }

      def attempt(alert: AlertDialog): Unit = {
        val cpfpBumpOrder = SemanticOrder(info.txid.toHex, System.currentTimeMillis)
        // Only update parent semantic order if it does not already have one, record it BEFORE sending CPFP
        val parentDescWithOrder = info.description.withNewOrderCond(cpfpBumpOrder.copy(order = Long.MinValue).asSome)
        WalletApp.txDataBag.updDescription(parentDescWithOrder, info.txid)
        alert.dismiss

        for {
          check <- fromWallet.doubleSpent(info.tx) if check.depth < 1 && !check.isDoubleSpent
          cpfpResponse <- fromWallet.makeCPFP(fromOutPoints.toSet, chainPubKeyScript, feeView.rate)
          bumpDescription = PlainTxDescription(chainAddress :: Nil, None, cpfpBumpOrder.asSome, None, cpfpOf = info.txid.asSome)
          // Record this description before sending, otherwise we won't be able to know a memo, label and semantic order
          _ = WalletApp.txDescriptions(cpfpResponse.tx.txid) = bumpDescription
          isSent <- notifyAndBroadcast(fromWallet, cpfpResponse.tx)
        } if (isSent) {
          // Parent semantic order is already updated, now we also update CPFP parent info
          WalletApp.txDataBag.updDescription(parentDescWithOrder.withNewCPFPBy(cpfpResponse.tx.txid), info.txid)
        } else {
          // We revert the whole description back since CPFP has failed
          WalletApp.txDataBag.updDescription(info.description, info.txid)
          onFail(me getString error_btc_broadcast_fail)
        }
      }

      lazy val alert = {
        val builder = titleBodyAsViewBuilder(getString(tx_cpfp_explain).asDefView, body)
        mkCheckForm(attempt, none, builder, dialog_ok, dialog_cancel)
      }

      feeView.update(feeOpt = None, showIssue = false)
      feeView.customFeerateOption.performClick
    }

    def boostRBF(info: TxInfo): Unit = LNParams.chainWallets.findByPubKey(info.pubKey) match {
      case None => snack(contentWindow, getString(error_btc_no_wallet).html, dialog_ok, _.dismiss)
      case Some(fromWallet) => doBoostRBF(fromWallet, info)
    }

    def doBoostRBF(fromWallet: ElectrumEclairWallet, info: TxInfo): Unit = {
      val currentFee = WalletApp.denom.parsedWithSign(info.feeSat.toMilliSatoshi, cardOut, cardIn)
      val body = getLayoutInflater.inflate(R.layout.frag_input_rbf, null).asInstanceOf[ScrollView]
      val rbfCurrent = new TwoSidedItem(body.findViewById(R.id.rbfCurrent), getString(tx_rbf_current), new String)
      val rbfIssue = body.findViewById(R.id.rbfIssue).asInstanceOf[TextView]

      val blockTarget = LNParams.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val target = LNParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView: FeeView[RBFResponse] = new FeeView[RBFResponse](FeeratePerByte(target), body) {
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
          super.update(feeOpt = None, showIssue = false)
          setVis(isVisible = true, rbfIssue)
          rbfIssue.setText(descRes)
        }.run

        override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
          updatePopupButton(getPositiveButton(alert), isEnabled = feeOpt.isDefined)
          setVis(isVisible = false, rbfIssue)
          super.update(feeOpt, showIssue)
        }.run
      }

      def attempt(alert: AlertDialog): Unit = {
        val rbfParams = RBFParams(info.txid, TxDescription.RBF_BOOST)
        val rbfBumpOrder = SemanticOrder(info.txid.toHex, -System.currentTimeMillis)
        alert.dismiss

        for {
          check <- fromWallet.doubleSpent(info.tx) if check.depth < 1 && !check.isDoubleSpent
          rbfBumpResponse <- fromWallet.makeRBFBump(info.tx, feeView.rate).map(_.result.right.get)
          bumpDescription = PlainTxDescription(addresses = Nil, None, rbfBumpOrder.asSome, None, None, rbfParams.asSome)
          // Record this description before sending, otherwise we won't be able to know a memo, label and semantic order
          _ = WalletApp.txDescriptions(rbfBumpResponse.tx.txid) = bumpDescription
          isSent <- notifyAndBroadcast(fromWallet, rbfBumpResponse.tx)
        } if (isSent) {
          val parentLowestOrder = rbfBumpOrder.copy(order = Long.MaxValue)
          val parentDesc = info.description.withNewOrderCond(parentLowestOrder.asSome)
          WalletApp.txDataBag.updDescription(parentDesc, info.txid)
        } else onFail(me getString error_btc_broadcast_fail)
      }

      lazy val alert = {
        val builder = titleBodyAsViewBuilder(getString(tx_rbf_boost_explain).asDefView, body)
        mkCheckForm(attempt, none, builder, dialog_ok, dialog_cancel)
      }

      rbfCurrent.secondItem.setText(currentFee.html)
      feeView.update(feeOpt = None, showIssue = false)
      feeView.customFeerateOption.performClick
      feeView.worker addWork "RBF-INIT-CALL"
    }

    def cancelRBF(info: TxInfo): Unit = LNParams.chainWallets.findByPubKey(info.pubKey) match {
      case None => snack(contentWindow, getString(error_btc_no_wallet).html, dialog_ok, _.dismiss)
      case Some(fromWallet) => doCancelRBF(fromWallet, info)
    }

    def doCancelRBF(fromWallet: ElectrumEclairWallet, info: TxInfo): Unit = {
      val currentFee = WalletApp.denom.parsedWithSign(info.feeSat.toMilliSatoshi, cardOut, cardIn)
      val body = getLayoutInflater.inflate(R.layout.frag_input_rbf, null).asInstanceOf[ScrollView]
      val rbfCurrent = new TwoSidedItem(body.findViewById(R.id.rbfCurrent), getString(tx_rbf_current), new String)
      val rbfIssue = body.findViewById(R.id.rbfIssue).asInstanceOf[TextView]

      val changeKey = Await.result(LNParams.chainWallets.lnWallet.getReceiveAddresses, atMost = 40.seconds)
      val changePubKeyScript = LNParams.addressToPubKeyScript(changeKey.changeAddress)

      val blockTarget = LNParams.feeRates.info.onChainFeeConf.feeTargets.fundingBlockTarget
      val target = LNParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(blockTarget)
      lazy val feeView: FeeView[RBFResponse] = new FeeView[RBFResponse](FeeratePerByte(target), body) {
        rate = target

        worker = new ThrottledWork[String, RBFResponse] {
          def process(reason: String, response: RBFResponse): Unit = response.result match {
            case Left(ElectrumWallet.PARENTS_MISSING) => showRbfErrorDesc(tx_rbf_err_parents_missing)
            case Left(ElectrumWallet.FOREIGN_INPUTS) => showRbfErrorDesc(tx_rbf_err_foreign_inputs)
            case Left(ElectrumWallet.RBF_DISABLED) => showRbfErrorDesc(tx_rbf_err_rbf_disabled)
            case Right(res) => update(res.fee.toMilliSatoshi.asSome, showIssue = false)
            case _ => error(new RuntimeException)
          }

          def work(reason: String): Observable[RBFResponse] = Rx fromFutureOnIo fromWallet.makeRBFReroute(info.tx, rate, changePubKeyScript)
          override def error(exception: Throwable): Unit = update(feeOpt = None, showIssue = true)
        }

        private def showRbfErrorDesc(descRes: Int): Unit = UITask {
          updatePopupButton(getPositiveButton(alert), isEnabled = false)
          super.update(feeOpt = None, showIssue = false)
          setVis(isVisible = true, rbfIssue)
          rbfIssue.setText(descRes)
        }.run

        override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
          updatePopupButton(getPositiveButton(alert), isEnabled = feeOpt.isDefined)
          setVis(isVisible = false, rbfIssue)
          super.update(feeOpt, showIssue)
        }.run
      }

      def attempt(alert: AlertDialog): Unit = {
        val rbfParams = RBFParams(info.txid, TxDescription.RBF_CANCEL)
        val rbfBumpOrder = SemanticOrder(info.txid.toHex, -System.currentTimeMillis)
        alert.dismiss

        for {
          check <- fromWallet.doubleSpent(info.tx) if check.depth < 1 && !check.isDoubleSpent
          rbfReroute <- fromWallet.makeRBFReroute(info.tx, feeView.rate, changePubKeyScript).map(_.result.right.get)
          bumpDescription = PlainTxDescription(addresses = Nil, None, rbfBumpOrder.asSome, None, None, rbfParams.asSome)
          // Record this description before sending, otherwise we won't be able to know a memo, label and semantic order
          _ = WalletApp.txDescriptions(rbfReroute.tx.txid) = bumpDescription
          isSent <- notifyAndBroadcast(fromWallet, rbfReroute.tx)
        } if (isSent) {
          val parentLowestOrder = rbfBumpOrder.copy(order = Long.MaxValue)
          val parentDesc = info.description.withNewOrderCond(parentLowestOrder.asSome)
          WalletApp.txDataBag.updDescription(parentDesc, info.txid)
        } else onFail(me getString error_btc_broadcast_fail)
      }

      lazy val alert = {
        val builder = titleBodyAsViewBuilder(getString(tx_rbf_cancel_explain).asDefView, body)
        mkCheckForm(attempt, none, builder, dialog_ok, dialog_cancel)
      }

      rbfCurrent.secondItem.setText(currentFee.html)
      feeView.update(feeOpt = None, showIssue = false)
      feeView.customFeerateOption.performClick
      feeView.worker addWork "RBF-INIT-CALL"
    }

    def retry(info: PaymentInfo): Unit = new HasTypicalChainFee {
      private val feeReserve = LNParams.cm.feeReserve(info.sent, info.chainFee, capLNFeeToChain = false, LNParams.maxOffChainFeeAboveRatio)
      private val cmd = LNParams.cm.makeSendCmd(info.prExt, LNParams.cm.all.values.toList, feeReserve, info.sent).modify(_.split.totalSum).setTo(info.sent)
      replaceOutgoingPayment(ext = info.prExt, description = info.description, action = info.action, sentAmount = cmd.split.myPart, seenAt = info.seenAt)
      LNParams.cm.localSend(cmd)
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
        case info: PaymentInfo =>
          val myFulfills = dangerousHCRevealed(info.fullTag)
          val amount = if (info.isIncoming) info.received else info.sent
          val incomingFSMOpt = LNParams.cm.inProcessors.get(info.fullTag)
          val outgoingFSMOpt = LNParams.cm.opm.data.payments.get(info.fullTag)

          val fiatThen = WalletApp.msatInFiatHuman(info.fiatRateSnapshot, WalletApp.fiatCode, amount, Denomination.formatFiatPrecise)
          val fiatNow = WalletApp.msatInFiatHuman(LNParams.fiatRates.info.rates, WalletApp.fiatCode, amount, Denomination.formatFiatPrecise)

          val liveFeePaid = outgoingFSMOpt.map(_.data.usedFee).getOrElse(info.fee)
          val offChainFeePaid = WalletApp.denom.directedWithSign(0L.msat, liveFeePaid, cardOut, cardIn, cardZero, isIncoming = false)
          val shouldDisplayFee = liveFeePaid > 0L.msat && (info.status == PaymentStatus.SUCCEEDED || info.status != PaymentStatus.ABORTED && outgoingFSMOpt.isDefined)
          val shouldRetry = info.status == PaymentStatus.ABORTED && !info.prExt.pr.isExpired && info.description.split.isEmpty && info.description.toSelfPreimage.isEmpty
          val shouldShowPayee = !info.isIncoming && info.description.toSelfPreimage.isEmpty

          addFlowChip(extraInfo, getString(popup_hash) format info.paymentHash.toHex.short, R.drawable.border_green, info.paymentHash.toHex.asSome)
          if (info.isIncoming && myFulfills.nonEmpty) addFlowChip(extraInfo, getString(error_hc_dangerous_state), R.drawable.border_red, _ => self warnDangerousHc info)
          for (txid <- info.description.proofTxid) addFlowChip(extraInfo, getString(popup_proof_stamp), R.drawable.border_yellow, _ => me share getStallingCommits(myFulfills, info) + "\n\n====\n\n" + txid)
          for (invoiceDescription <- info.description.externalInfo if info.description.label.nonEmpty) addFlowChip(extraInfo, invoiceDescription, R.drawable.border_blue, invoiceDescription.asSome)
          if (shouldShowPayee) addFlowChip(extraInfo, getString(popup_ln_payee) format info.prExt.pr.nodeId.toString.short, R.drawable.border_blue, info.prExt.pr.nodeId.toString.asSome)

          addFlowChip(extraInfo, getString(popup_fiat).format(s"<font color=$cardIn>$fiatNow</font>", fiatThen), R.drawable.border_gray)
          addFlowChip(extraInfo, getString(popup_prior_chain_balance) format WalletApp.denom.parsedWithSign(info.balanceSnapshot, cardIn, cardZero), R.drawable.border_gray)
          if (info.isIncoming && info.status == PaymentStatus.PENDING) addFlowChip(extraInfo, getString(popup_view_invoice), R.drawable.border_blue, _ => self doViewInvoice info)
          if (!info.isIncoming && shouldDisplayFee) addFlowChip(extraInfo, getString(popup_ln_fee).format(offChainFeePaid, ratio(amount, liveFeePaid) + PERCENT), R.drawable.border_gray)
          if (shouldRetry) addFlowChip(extraInfo, getString(popup_retry), R.drawable.border_yellow, _ => self retry info)

          incomingFSMOpt.filter(info.isActivelyHolding).foreach { fsm =>
            addFlowChip(extraInfo, getString(dialog_accept), R.drawable.border_green, _ => fsm doProcess IncomingPaymentProcessor.CMDReleaseHold)
            addFlowChip(extraInfo, getString(dialog_cancel), R.drawable.border_yellow, _ => fsm doProcess IncomingPaymentProcessor.CMDTimeout)
          }

          for (action <- info.action if info.status == PaymentStatus.SUCCEEDED) {
            def run: Unit = resolveAction(theirPreimage = info.preimage, paymentAction = action)
            addFlowChip(extraInfo, getString(popup_run_action), R.drawable.border_green, _ => run)
          }

          lastInChannelOutgoing.get(info.fullTag).map(_.maxBy(_.cltvExpiry.underlying).cltvExpiry.underlying - LNParams.blockCount.get) match {
            case Some(left) if left > 0 => addFlowChip(extraInfo, WalletApp.app.plurOrZero(left, expiresInBlocks), R.drawable.border_gray)
            case Some(left) if left <= 0 => addFlowChip(extraInfo, expiresInBlocks.head, R.drawable.border_red)
            case None => // Either incoming or not in channels
          }

        case info: TxInfo =>
          val amount = if (info.isIncoming) info.receivedSat.toMilliSatoshi else info.sentSat.toMilliSatoshi
          val canRBF = !info.isIncoming && !info.isDoubleSpent && info.depth < 1 && info.description.rbf.isEmpty && info.description.cpfpOf.isEmpty
          val canCPFP = info.isIncoming && !info.isDoubleSpent && info.depth < 1 && info.description.rbf.isEmpty && info.description.canBeCPFPd
          val isFromSigningWallet = LNParams.chainWallets.findByPubKey(info.pubKey).exists(_.isSigning)
          val isRbfCancel = info.description.rbf.exists(_.mode == TxDescription.RBF_CANCEL)

          val fee = WalletApp.denom.directedWithSign(0L.msat, info.feeSat.toMilliSatoshi, cardOut, cardIn, cardZero, isIncoming = false)
          val fiatThen = WalletApp.msatInFiatHuman(LNParams.fiatRates.info.rates, WalletApp.fiatCode, amount, Denomination.formatFiatPrecise)
          val fiatNow = WalletApp.msatInFiatHuman(info.fiatRateSnapshot, WalletApp.fiatCode, amount, Denomination.formatFiatPrecise)
          val balanceSnapshot = WalletApp.denom.parsedWithSign(info.balanceSnapshot, cardIn, cardZero)

          addFlowChip(extraInfo, getString(popup_txid) format info.txidString.short, R.drawable.border_green, info.txidString.asSome)
          for (address <- info.description.toAddress) addFlowChip(extraInfo, getString(popup_to_address) format address.short, R.drawable.border_yellow, address.asSome)
          for (nodeId <- info.description.withNodeId) addFlowChip(extraInfo, getString(popup_ln_node) format nodeId.toString.short, R.drawable.border_blue, nodeId.toString.asSome)

          addFlowChip(extraInfo, getString(popup_fiat).format(s"<font color=$cardIn>$fiatNow</font>", fiatThen), R.drawable.border_gray)
          if (info.description.cpfpOf.isEmpty && info.description.rbf.isEmpty) addFlowChip(extraInfo, getString(popup_prior_chain_balance) format balanceSnapshot, R.drawable.border_gray)
          if (!info.isIncoming || isRbfCancel || info.description.cpfpOf.isDefined) addFlowChip(extraInfo, getString(popup_chain_fee) format fee, R.drawable.border_gray)

          if (isFromSigningWallet && canCPFP) addFlowChip(extraInfo, getString(dialog_boost), R.drawable.border_yellow, _ => self boostCPFP info)
          if (isFromSigningWallet && canRBF) addFlowChip(extraInfo, getString(dialog_cancel), R.drawable.border_yellow, _ => self cancelRBF info)
          if (isFromSigningWallet && canRBF) addFlowChip(extraInfo, getString(dialog_boost), R.drawable.border_yellow, _ => self boostRBF info)

        case info: RelayedPreimageInfo =>
          val relayedHuman = WalletApp.denom.parsedWithSign(info.relayed, cardIn, cardZero)
          addFlowChip(extraInfo, getString(popup_hash) format info.paymentHashString.short, R.drawable.border_green)
          addFlowChip(extraInfo, getString(stats_item_relayed) format relayedHuman, R.drawable.border_gray)

        case _ =>
          // Do nothing
      }
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
        if (info.isIncoming) setVisibleIcon(R.id.lnIncoming) else setVisibleIcon(R.id.lnOutgoing)
        setVisMany(info.description.label.isDefined -> labelIcon, true -> detailsAndStatus, true -> nonLinkContainer, false -> linkContainer, true -> removeItem)
        amount.setText(WalletApp.denom.directedWithSign(info.received, info.sent, cardOut, cardIn, cardZero, info.isIncoming).html)
        description.setText(paymentDescription(info).html)
        swipeWrap.setLockDrag(false)

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
        marketInfo.setText(marketLinkCaption(info).html)
        marketInfo.setCompoundDrawablesWithIntrinsicBounds(marketLinkIcon(info), null, null, null)
        setVisMany(info.imageBytes.isDefined -> linkImageWrap, info.description.label.isDefined -> marketLabel)
        setVisMany(false -> labelIcon, true -> linkContainer, false -> nonLinkContainer, true -> removeItem)
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
      case _ => info.payLink.get.warnUri
    }

    def marketLinkIcon(info: LNUrlPayLink): Drawable = info.payMetaData match {
      case Success(payMeta) if payMeta.identities.nonEmpty => getDrawable(R.drawable.baseline_perm_identity_18)
      case Success(payMeta) if payMeta.emails.nonEmpty => getDrawable(R.drawable.baseline_alternate_email_18)
      case _ => getDrawable(R.drawable.baseline_language_18)
    }

    // TX helpers

    def txDescription(transactionInfo: TxInfo): String = transactionInfo.description match {
      case _ if transactionInfo.description.cpfpOf.isDefined => getString(tx_description_cpfp)
      case _ if transactionInfo.description.rbf.exists(_.mode == TxDescription.RBF_BOOST) => getString(tx_description_rbf_boost)
      case _ if transactionInfo.description.rbf.exists(_.mode == TxDescription.RBF_CANCEL) => getString(tx_description_rbf_cancel)
      case plain: PlainTxDescription => plain.toAddress.map(_.short) getOrElse getString(tx_btc)
      case _: ChanRefundingTxDescription => getString(tx_description_refunding)
      case _: HtlcClaimTxDescription => getString(tx_description_htlc_claiming)
      case _: ChanFundingTxDescription => getString(tx_description_funding)
      case _: OpReturnTxDescription => getString(tx_description_op_return)
      case _: PenaltyTxDescription => getString(tx_description_penalty)
    }

    def setTxTypeIcon(info: TxInfo): Unit = info.description match {
      case _ if info.description.cpfpOf.isDefined => setVisibleIcon(id = R.id.btcInBoosted)
      case _ if info.description.rbf.exists(_.mode == TxDescription.RBF_BOOST) => setVisibleIcon(id = R.id.btcOutBoosted)
      case _ if info.description.rbf.exists(_.mode == TxDescription.RBF_CANCEL) => setVisibleIcon(id = R.id.btcOutCancelled)
      case _: PlainTxDescription if info.isIncoming => setVisibleIcon(id = R.id.btcIncoming)
      case _: OpReturnTxDescription => setVisibleIcon(id = R.id.btcOutgoing)
      case _: ChanRefundingTxDescription => setVisibleIcon(id = R.id.lnBtc)
      case _: ChanFundingTxDescription => setVisibleIcon(id = R.id.btcLn)
      case _: HtlcClaimTxDescription => setVisibleIcon(id = R.id.lnBtc)
      case _: PenaltyTxDescription => setVisibleIcon(id = R.id.lnBtc)
      case _ => setVisibleIcon(id = R.id.btcOutgoing)
    }

    def txStatusIcon(info: TxInfo): Int = {
      if (info.isConfirmed) R.drawable.baseline_done_24
      else if (info.isDoubleSpent) R.drawable.baseline_block_24
      else R.drawable.baseline_hourglass_empty_24
    }

    def setTxMeta(info: TxInfo): Unit = {
      if (info.isDoubleSpent) meta setText getString(tx_state_double_spent).html
      else if (info.isConfirmed) meta setText WalletApp.app.when(info.date, WalletApp.app.dateFormat).html
      else if (info.depth > 0) meta setText getString(tx_state_confs).format(info.depth, LNParams.minDepthBlocks).html
      else meta setText pctCollected.head
    }

    // LN helpers

    def paymentDescription(info: PaymentInfo): String = {
      val finalText = info.description.label.orElse(info.description.externalInfo).getOrElse(lnDefTitle)
      info.description.split.map(split => lnSplitNotice.format(split.sentRatio) + finalText).getOrElse(finalText)
    }

    def setIncomingPaymentMeta(info: PaymentInfo): Unit = {
      val fsmOpt = LNParams.cm.inProcessors.get(info.fullTag)
      val fsmReceiving = fsmOpt.exists(_.state == IncomingPaymentProcessor.RECEIVING)
      if (fsmOpt exists info.isActivelyHolding) meta setText s"<b>${fsmOpt.get.secondsLeft}</b> sec".html // Show how many seconds left until cancel
      else if (fsmOpt.isDefined && PaymentStatus.SUCCEEDED == info.status) meta setText pctCollected.last.html // Preimage is revealed but we are not done yet
      else if (fsmOpt.isEmpty && PaymentStatus.SUCCEEDED == info.status) meta setText WalletApp.app.when(info.date, WalletApp.app.dateFormat).html // Payment has been cleared
      else if (fsmReceiving && PaymentStatus.PENDING == info.status) meta setText WalletApp.app.plurOrZero(info.ratio(fsmOpt.get), pctCollected).html // Actively collecting parts
      else meta setText pctCollected.head
    }

    def setOutgoingPaymentMeta(info: PaymentInfo): Unit = {
      val activeParts = lastInChannelOutgoing.getOrElse(info.fullTag, Nil).size
      val isPendingOrBeingSent = PaymentStatus.PENDING == info.status || activeParts > 0
      if (isPendingOrBeingSent) meta setText WalletApp.app.plurOrZero(activeParts, partsInFlight).html
      else meta setText WalletApp.app.when(info.date, WalletApp.app.dateFormat).html
    }

    def paymentStatusIcon(info: PaymentInfo): Int = {
      if (PaymentStatus.SUCCEEDED == info.status) R.drawable.baseline_done_24
      else if (PaymentStatus.ABORTED == info.status) R.drawable.baseline_block_24
      else if (LNParams.cm.inProcessors.get(info.fullTag) exists info.isActivelyHolding) R.drawable.baseline_feedback_24
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
    val lnSyncIndicator: TextView = view.findViewById(R.id.lnSyncIndicator).asInstanceOf[TextView]

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
      override def onCoinControlTap(wallet: ElectrumEclairWallet): Unit = goToWithValue(ClassNames.coinControlActivityClass, wallet)

      override def onWalletTap(wallet: ElectrumEclairWallet): Unit =
        if (wallet.isBuiltIn) goToWithValue(ClassNames.qrChainActivityClass, wallet)
        else if (wallet.isSigning) bringLegacyWalletMenuSpendOptions(wallet)
        else goToWithValue(ClassNames.qrChainActivityClass, wallet)

      override def onLabelTap(wallet: ElectrumEclairWallet): Unit = {
        val (container, extraInputLayout, extraInput) = singleInputPopup
        val builder = titleBodyAsViewBuilder(title = null, body = container)
        mkCheckForm(proceed, none, builder, dialog_ok, dialog_cancel)
        extraInputLayout.setHint(dialog_set_label)
        showKeys(extraInput)

        def proceed(alert: AlertDialog): Unit = runAnd(alert.dismiss) {
          val withnewLabel: ElectrumEclairWallet => WalletExt = LNParams.chainWallets.withNewLabel(extraInput.getText.toString)
          LNParams.chainWallets.findByPubKey(wallet.ewt.xPub.publicKey).map(withnewLabel).foreach(resetChainCards)
        }
      }

      override def onRemoveTap(wallet: ElectrumEclairWallet): Unit = {
        def proceed: Unit = LNParams.chainWallets.findByPubKey(wallet.ewt.xPub.publicKey).map(LNParams.chainWallets.withoutWallet).foreach(resetChainCards)
        mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, new AlertDialog.Builder(me).setMessage(confirm_remove_item), dialog_ok, dialog_cancel)
      }
    }

    def resetChainCards(ext1: WalletExt): Unit = {
      // Remove all existing cards and place new ones
      LNParams.synchronized(LNParams.chainWallets = ext1)
      chainCards.holder.removeAllViewsInLayout
      chainCards.init(ext1.wallets)
      updateView
    }

    def updateView: Unit = {
      val allChannels = LNParams.cm.all.values.take(8)
      val lnBalance = Channel.totalBalance(LNParams.cm.all.values)
      val localInCount = LNParams.cm.inProcessors.count { case (fullTag, _) => fullTag.tag == PaymentTagTlv.FINAL_INCOMING }
      val trampolineCount = LNParams.cm.inProcessors.count { case (fullTag, _) => fullTag.tag == PaymentTagTlv.TRAMPLOINE_ROUTED }
      val localOutCount = LNParams.cm.opm.data.payments.count { case (fullTag, _) => fullTag.tag == PaymentTagTlv.LOCALLY_SENT }
      val change = LNParams.fiatRates.info.pctDifference(WalletApp.fiatCode).map(_ + "<br>").getOrElse(new String)
      val unitPriceAndChange = s"<small>$change</small>${WalletApp currentMsatInFiatHuman 100000000000L.msat}"

      TransitionManager.beginDelayedTransition(defaultHeader)
      fiatUnitPriceAndChange.setText(unitPriceAndChange.html)
      totalFiatBalance.setText(WalletApp.currentMsatInFiatHuman(BaseActivity.totalBalance).html)
      totalBalance.setText(WalletApp.denom.parsedWithSign(BaseActivity.totalBalance, cardIn, totalZero).html)
      totalLightningBalance.setText(WalletApp.denom.parsedWithSign(lnBalance, cardIn, lnCardZero).html)
      lnBalanceFiat.setText(WalletApp currentMsatInFiatHuman lnBalance)
      channelIndicator.createIndicators(allChannels.toArray)

      setVisMany(allChannels.nonEmpty -> channelStateIndicators, allChannels.nonEmpty -> totalLightningBalance, allChannels.isEmpty -> addChannelTip)
      // We have updated chain wallet balances at this point because listener in WalletApp gets called first
      chainCards.update(LNParams.chainWallets.wallets)

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

  private val chainListener = new WalletEventsListener {
    override def onChainTipKnown(currentBlockCount: CurrentBlockCount): Unit = {
      val openHc = WalletApp.openHc && LNParams.isMainnet && LNParams.cm.allHostedCommits.isEmpty
      if (openHc) HubActivity.requestHostedChannel
    }

    override def onChainMasterSelected(event: InetSocketAddress): Unit = UITask {
      TransitionManager.beginDelayedTransition(walletCards.defaultHeader)
      setVis(isVisible = false, walletCards.offlineIndicator)
    }.run

    override def onChainDisconnected: Unit = UITask {
      setVis(isVisible = true, walletCards.offlineIndicator)
    }.run

    override def onChainSyncStarted(localTip: Long, remoteTip: Long): Unit = UITask {
      setVis(isVisible = remoteTip - localTip > 2016 * 4, walletCards.chainSyncIndicator)
    }.run

    override def onChainSyncEnded(localTip: Long): Unit = UITask {
      setVis(isVisible = false, walletCards.chainSyncIndicator)
    }.run

    override def onWalletReady(event: WalletReady): Unit = {
      ChannelMaster.next(ChannelMaster.statusUpdateStream)
      ChannelMaster.next(ChannelMaster.txDbStream)
    }
  }

  private val fiatRatesListener = new FiatRatesListener {
    def onFiatRates(rates: FiatRatesInfo): Unit = UITask(walletCards.updateView).run
  }

  private val extraOutgoingListener = new OutgoingPaymentListener {
    override def wholePaymentFailed(data: OutgoingPaymentSenderData): Unit = UITask {
      val assistedShortIds = data.cmd.assistedEdges.map(_.updExt.update.shortChannelId)
      val warnPayeeOffline = data.failures.exists { case rf: RemoteFailure if rf.packet.failureMessage == UnknownNextPeer => assistedShortIds.contains(rf.originShortChanId) case _ => false }
      val warnNoRouteFound = data.failures.exists { case localFailure: LocalFailure => PaymentFailure.NO_ROUTES_FOUND == localFailure.status case _ => false }
      if (warnPayeeOffline && warnNoRouteFound) snack(parent = contentWindow, msg = getString(ln_payee_likely_offline), res = dialog_ok, _.dismiss)
      else if (WalletApp.capLNFeeToChain && warnNoRouteFound) snack(contentWindow, getString(ln_fee_expensive_omitted), dialog_ok, _.dismiss)
    }.run

    override def gotFirstPreimage(data: OutgoingPaymentSenderData, fulfill: RemoteFulfill): Unit = UITask {
      val actionOpt = paymentInfos.find(_.paymentHash == fulfill.ourAdd.paymentHash).flatMap(_.action)
      for (paymentAction <- actionOpt) resolveAction(fulfill.theirPreimage, paymentAction)
      Vibrator.vibrate
    }.run
  }

  // NFC

  def readEmptyNdefMessage: Unit = nothingUsefulTask.run
  def readNonNdefMessage: Unit = nothingUsefulTask.run
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

    try LNParams.chainWallets.catcher ! WalletEventsCatcher.Remove(chainListener) catch none
    try for (channel <- LNParams.cm.all.values) channel.listeners -= me catch none
    try LNParams.cm.localPaymentListeners remove extraOutgoingListener catch none
    try LNParams.fiatRates.listeners -= fiatRatesListener catch none
    try LNParams.cm.pf.listeners -= me catch none
    super.onDestroy
  }

  override def onBackPressed: Unit = if (isSearchOn) rmSearch(null) else super.onBackPressed

  // Getting graph sync status and our peer announcements

  override def process(reply: Any): Unit = reply match {
    case na: NodeAnnouncement => LNParams.cm.all.values.foreach(_ process na.toRemoteInfo)
    case _: SyncMasterShortIdData => UITask(walletCards.lnSyncIndicator setVisibility View.VISIBLE).run
    case _: PureRoutingData => UITask(walletCards.lnSyncIndicator setVisibility View.VISIBLE).run
    case _: SyncMaster => UITask(walletCards.lnSyncIndicator setVisibility View.GONE).run
    case _ => // Do nothing
  }

  type GrantResults = Array[Int]

  override def onRequestPermissionsResult(reqCode: Int, permissions: Array[String], results: GrantResults): Unit = {
    if (reqCode == scannerRequestCode && results.nonEmpty && results.head == PackageManager.PERMISSION_GRANTED) bringScanner(null)
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case bitcoinUri: BitcoinUri if Try(LNParams addressToPubKeyScript bitcoinUri.address).isSuccess =>

      if (LNParams.chainWallets.usableWallets.size == 1) {
        // We have a single built-in wallet, no need to choose
        bringSendBitcoinPopup(bitcoinUri, LNParams.chainWallets.lnWallet)
      } else bringChainWalletChooser(me titleViewFromUri bitcoinUri) { wallet =>
        // We have wallet candidates to spend from here
        bringSendBitcoinPopup(bitcoinUri, wallet)
      }

    case a2a: MultiAddressParser.AddressToAmount =>
      val dustAmount = a2a.values.secondItems.find(amount => LNParams.chainWallets.params.dustLimit > amount)
      val badAddress = a2a.values.firstItems.find(address => Try(LNParams addressToPubKeyScript address).isFailure)

      if (dustAmount.nonEmpty) {
        val minimum = LNParams.chainWallets.params.dustLimit.toLong
        onFail(s"Incorrect amount=${dustAmount.get.toLong}, minimum=$minimum")
      } else if (badAddress.nonEmpty) {
        onFail(s"Incorrect Bitcoin address=${badAddress.get}")
      } else if (LNParams.chainWallets.usableWallets.size == 1) {
        // We have a single built-in wallet, no need to choose one
        bringSendMultiBitcoinPopup(a2a, LNParams.chainWallets.lnWallet)
      } else bringChainWalletChooser(me getString dialog_send_btc_many) { wallet =>
        // We have wallet candidates to spend from here
        bringSendMultiBitcoinPopup(a2a, wallet)
      }

    case info: RemoteNodeInfo =>
      // In case if IP address has changed
      LNParams.cm.all.values.foreach(_ process info)
      goTo(ClassNames.remotePeerActivityClass)

    case prExt: PaymentRequestExt =>
      lnSendGuard(prExt, contentWindow) {
        case Some(origAmount) if prExt.splits.nonEmpty =>
          new OffChainSender(maxSendable = LNParams.cm.maxSendable(LNParams.cm.all.values).min(prExt.splitLeftover * 2), minSendable = LNParams.minPayment) {
            override def isNeutralEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat < prExt.splitLeftover - minSendable
            override def isPayEnabled: Boolean = manager.resultMsat >= prExt.splitLeftover && manager.resultMsat <= maxSendable
            override def neutral(alert: AlertDialog): Unit = proceedSplit(prExt, origAmount, alert)

            override def send(alert: AlertDialog): Unit = {
              val feeReserve = LNParams.cm.feeReserve(manager.resultMsat, typicalChainTxFee, WalletApp.capLNFeeToChain, LNParams.maxOffChainFeeAboveRatio)
              val cmd = LNParams.cm.makeSendCmd(prExt, LNParams.cm.all.values.toList, feeReserve, manager.resultMsat).modify(_.split.totalSum).setTo(origAmount)
              val pd = PaymentDescription(split = cmd.split.asSome, label = manager.resultExtraInput, semanticOrder = None, invoiceText = prExt.descriptionOpt getOrElse new String)
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
              val popup = mkCheckFormNeutral(send, none, neutral, builder, dialog_ok, dialog_cancel, dialog_split)

              def fillFlow(value: CharSequence) = UITask {
                runAnd(title.flow.removeAllViewsInLayout) {
                  addFlowChip(title.flow, getString(dialog_ln_requested).format(totalHuman), R.drawable.border_blue)
                  addFlowChip(title.flow, getString(dialog_ln_expected_fee).format(value), R.drawable.border_blue)
                }
              }

              val sender = new CanBeRepliedTo {
                override def process(reply: Any): Unit = reply match {
                  case PathFinder.NotifyNotReady => fillFlow(getString(dialog_up_to).format(ExpectedRouteFees(Router.defAvgHops) highCapRatio origAmount) + PERCENT).run
                  case exp: ExpectedRouteFees => fillFlow(getString(dialog_up_to).format(exp highCapRatio origAmount) + PERCENT).run
                  case _ => fillFlow("n/a").run
                }
              }

              val length = Router.DEFAULT_EXPECTED_ROUTE_LENGTH
              val cmd = GetExpectedRouteFees(sender, prExt.pr.nodeId, length)
              fillFlow(pctCollected.head).run
              LNParams.cm.pf process cmd
              popup
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
            updatePopupButton(getPositiveButton(alert), isEnabled = false)
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
      case nc: NormalChannelRequest => goToWithValue(ClassNames.remotePeerActivityClass, nc)
      case hc: HostedChannelRequest => goToWithValue(ClassNames.remotePeerActivityClass, hc)
      case _ => nothingUsefulTask.run
    }

    snack(contentWindow, getString(dialog_lnurl_processing).format(lnUrl.warnUri).html, dialog_cancel) foreach { snack =>
      val level1Sub = lnUrl.level1DataResponse.doOnUnsubscribe(snack.dismiss).doOnTerminate(snack.dismiss).subscribe(resolve, onFail)
      val listener = onButtonTap(level1Sub.unsubscribe)
      snack.setAction(dialog_cancel, listener).show
    }
  }

  def showAuthForm(lnUrl: LNUrl): Unit = lnUrl.k1.foreach { k1 =>
    val (successResource, actionResource) = lnUrl.authAction match {
      case "register" => (lnurl_auth_register_ok, lnurl_auth_register)
      case "auth" => (lnurl_auth_auth_ok, lnurl_auth_auth)
      case "link" => (lnurl_auth_link_ok, lnurl_auth_link)
      case _ => (lnurl_auth_login_ok, lnurl_auth_login)
    }

    val spec = LNUrlAuthSpec(lnUrl.uri.getHost, ByteVector32 fromValidHex k1)
    val title = titleBodyAsViewBuilder(s"<big>${lnUrl.warnUri}</big>".asColoredView(R.color.cardLightning), null)
    mkCheckFormNeutral(doAuth, none, displayInfo, title, actionResource, dialog_cancel, dialog_info)

    def displayInfo(alert: AlertDialog): Unit = {
      val explanation = getString(lnurl_auth_info).format(lnUrl.warnUri, spec.linkingPubKey.humanFour).html
      mkCheckFormNeutral(_.dismiss, none, _ => share(spec.linkingPubKey), new AlertDialog.Builder(me).setMessage(explanation), dialog_ok, -1, dialog_share)
    }

    def doAuth(alert: AlertDialog): Unit = runAnd(alert.dismiss) {
      snack(contentWindow, getString(dialog_lnurl_processing).format(lnUrl.warnUri).html, dialog_cancel) foreach { snack =>
        val uri = lnUrl.uri.buildUpon.appendQueryParameter("sig", spec.derSignatureHex).appendQueryParameter("key", spec.linkingPubKey)
        val level2Obs = LNUrl.level2DataResponse(uri).doOnUnsubscribe(snack.dismiss).doOnTerminate(snack.dismiss)
        val level2Sub = level2Obs.subscribe(_ => UITask(WalletApp.app quickToast successResource).run, onFail)
        val listener = onButtonTap(level2Sub.unsubscribe)
        snack.setAction(dialog_cancel, listener).show
      }
    }
  }

  def isSearchOn: Boolean = walletCards.searchField.getTag.asInstanceOf[Boolean]

  override def onChoiceMade(tag: AnyRef, pos: Int): Unit = (tag, pos) match {
    case (legacy: ElectrumEclairWallet, 0) if legacy.isSigning => transferFromLegacyToModern(legacy)
    case (legacy: ElectrumEclairWallet, 1) if legacy.isSigning => bringBitcoinSpecificScanner(legacy)
    case (CHOICE_RECEIVE_TAG, 0) => goToWithValue(ClassNames.qrChainActivityClass, LNParams.chainWallets.lnWallet)
    case (CHOICE_RECEIVE_TAG, 1) => bringReceivePopup
    case _ =>
  }

  def INIT(state: Bundle): Unit = {
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(com.btcontract.wallet.R.layout.activity_hub)
      instance = me

      for (channel <- LNParams.cm.all.values) channel.listeners += me
      LNParams.cm.localPaymentListeners add extraOutgoingListener
      LNParams.fiatRates.listeners += fiatRatesListener
      LNParams.chainWallets.catcher ! chainListener
      LNParams.cm.pf.listeners += me

      bottomActionBar post UITask {
        bottomBlurringArea.setHeightTo(bottomActionBar)
        itemsList.setPadding(0, 0, 0, bottomActionBar.getHeight)
      }

      // TOGGLE MENU

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
          runAnd(updAllInfos)(paymentAdapterDataChanged.run)
        }
      }

      // LIST

      itemsList.addHeaderView(walletCards.view)
      itemsList.setAdapter(paymentsAdapter)
      itemsList.setDividerHeight(0)
      itemsList.setDivider(null)

      // Fill wallet list with wallet card views here
      walletCards.chainCards.init(LNParams.chainWallets.wallets)
      walletCards.updateView

      runInFutureProcessOnUI(loadRecent, none) { _ =>
        // We suggest user to rate us if: no rate attempt has been made before, LN payments were successful, user has been using an app for certain period
        setVis(WalletApp.showRateUs && paymentInfos.forall(_.status == PaymentStatus.SUCCEEDED) && allInfos.size > 4 && allInfos.size < 8, walletCards.rateTeaser)
        // User may kill an activity but not an app and on getting back there won't be a chain listener event, so check connectivity once again here
        setVis(isVisible = WalletApp.currentChainNode.isEmpty, walletCards.offlineIndicator)
        walletCards.searchField addTextChangedListener onTextChange(searchWorker.addWork)
        runAnd(updateLnCaches)(paymentAdapterDataChanged.run)
        markAsFailedOnce
      }

      // STREAMS

      val window = 600.millis
      val txEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.txDbStream, window).doOnNext { _ =>
        // After each delayed update we check if pending txs got confirmed or double-spent
        // do this check specifically after updating txInfos with new items
        reloadTxInfos

        for {
          txInfo <- txInfos if !txInfo.isDoubleSpent && !txInfo.isConfirmed
          relatedChainWallet <- LNParams.chainWallets.findByPubKey(pub = txInfo.pubKey)
          res <- relatedChainWallet.doubleSpent(txInfo.tx) if res.depth != txInfo.depth || res.isDoubleSpent != txInfo.isDoubleSpent
        } WalletApp.txDataBag.updStatus(txInfo.txid, res.depth, updatedStamp = res.stamp, res.isDoubleSpent)
      }

      val relayEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.relayDbStream, window).doOnNext(_ => reloadRelayedPreimageInfos)
      val marketEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.payMarketDbStream, window).doOnNext(_ => reloadPayMarketInfos)
      val paymentEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.paymentDbStream, window).doOnNext(_ => reloadPaymentInfos)
      val stateEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.stateUpdateStream, window).doOnNext(_ => updateLnCaches)

      stateSubscription = txEvents.merge(paymentEvents).merge(relayEvents).merge(marketEvents).merge(stateEvents).doOnNext(_ => updAllInfos).subscribe(_ => paymentAdapterDataChanged.run).asSome
      statusSubscription = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.statusUpdateStream, window).merge(stateEvents).subscribe(_ => UITask(walletCards.updateView).run).asSome
      inFinalizedSubscription = ChannelMaster.inFinalized.collect { case _: IncomingRevealed => true }.subscribe(_ => Vibrator.vibrate).asSome

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
    setVisMany(true -> walletCards.searchField, false -> walletCards.defaultHeader, false -> walletCards.listCaption)
    showKeys(walletCards.searchField)
  }

  def rmSearch(view: View): Unit = {
    walletCards.searchField.setTag(false)
    walletCards.searchField.setText(new String)
    TransitionManager.beginDelayedTransition(contentWindow)
    setVisMany(false -> walletCards.searchField, true -> walletCards.defaultHeader, hasItems -> walletCards.listCaption)
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
      case uri: BitcoinUri if Try(LNParams addressToPubKeyScript uri.address).isSuccess =>
        bringSendBitcoinPopup(uri, fromWallet)
      case _ => nothingUsefulTask.run
    }

    val instruction = getString(scan_btc_address).asSome
    def onData: Runnable = UITask(resolveLegacyWalletBtcAddressQr)
    val sheet = new sheets.OnceBottomSheet(me, instruction, onData)
    callScanner(sheet)
  }

  def bringReceiveOptions(view: View): Unit = {
    val options = Array(dialog_receive_btc, dialog_receive_ln).map(getString).map(_.html)
    val list = me selectorList new ArrayAdapter(me, android.R.layout.simple_expandable_list_item_1, options)
    new sheets.ChoiceBottomSheet(list, CHOICE_RECEIVE_TAG, me).show(getSupportFragmentManager, "unused-tag")
  }

  def bringLegacyWalletMenuSpendOptions(wallet: ElectrumEclairWallet): Unit = {
    val options = Array(dialog_legacy_transfer_btc, dialog_legacy_send_btc).map(getString)
    val list = me selectorList new ArrayAdapter(me, android.R.layout.simple_expandable_list_item_1, options)
    new sheets.ChoiceBottomSheet(list, wallet, me).show(getSupportFragmentManager, "unused-legacy-tag")
  }

  def goToStatPage(view: View): Unit = goTo(ClassNames.chanActivityClass)

  def goToSettingsPage(view: View): Unit = goTo(ClassNames.settingsActivityClass)

  def transferFromLegacyToModern(legacy: ElectrumEclairWallet): Unit = {
    runFutureProcessOnUI(LNParams.chainWallets.lnWallet.getReceiveAddresses, onFail) { addresses =>
      val labelAndMessage = s"?label=${me getString btc_transfer_label}&message=${me getString btc_transfer_message}"
      val uri = BitcoinUri.fromRaw(InputParser.bitcoin + addresses.firstAccountAddress + labelAndMessage)
      bringSendBitcoinPopup(uri, legacy)
    }
  }

  def bringSendBitcoinPopup(uri: BitcoinUri, fromWallet: ElectrumEclairWallet): Unit = {
    val sendView = new ChainSendView(fromWallet, getString(dialog_set_label).asSome, dialog_visibility_private)
    val chainPubKeyScript = LNParams.addressToPubKeyScript(uri.address)

    def attempt(alert: AlertDialog): Unit = {
      runFutureProcessOnUI(fromWallet.makeTx(chainPubKeyScript, sendView.manager.resultMsat.truncateToSatoshi, Map.empty, feeView.rate), onFail) { response =>
        // It is fine to use the first map element here: we send to single address so response may will always have a single element (change not included)
        val finalSendButton = sendView.chainConfirmView.chainButtonsView.chainNextButton
        val totalSendAmount = response.pubKeyScriptToAmount.values.head.toMilliSatoshi

        if (fromWallet.isSigning) {
          finalSendButton setOnClickListener onButtonTap(process apply response.tx)
          sendView.switchToConfirm(alert, totalSendAmount, response.fee.toMilliSatoshi)
        } else if (fromWallet.hasFingerprint) {
          sendView.chainReaderView.onSignedTx = signedTx => UITask {
            if (signedTx.txOut.toSet != response.tx.txOut.toSet) alert.dismiss
            finalSendButton setOnClickListener onButtonTap(process apply signedTx)
            sendView.switchToConfirm(alert, totalSendAmount, response.fee.toMilliSatoshi)
          }.run

          val masterFingerprint = fromWallet.info.core.masterFingerprint.get
          val psbt = prepareBip84Psbt(response, masterFingerprint)
          sendView.switchToHardwareOutgoing(alert, psbt)
        } else alert.dismiss
      }
    }

    lazy val alert = {
      def switchToLn(alert: AlertDialog): Unit = {
        uri.prExt.foreach(ext => InputParser.value = ext)
        checkExternalData(noneRunnable)
        alert.dismiss
      }

      def useMax(alert: AlertDialog): Unit = {
        val max = fromWallet.info.lastBalance.toMilliSatoshi
        sendView.manager.updateText(value = max)
      }

      val title = titleViewFromUri(uri)
      val neutralRes = if (uri.amount.isDefined) -1 else dialog_max
      val builder = titleBodyAsViewBuilder(title.asColoredView(me chainWalletBackground fromWallet), sendView.body)
      addFlowChip(title.flow, getString(dialog_send_btc_from).format(fromWallet.info.label), R.drawable.border_yellow)
      if (uri.prExt.isEmpty) mkCheckFormNeutral(attempt, none, useMax, builder, dialog_ok, dialog_cancel, neutralRes)
      else mkCheckFormNeutral(attempt, none, switchToLn, builder, dialog_ok, dialog_cancel, lightning_wallet)
    }

    lazy val process: Transaction => Unit = signedTx => {
      val transactionLabel = sendView.manager.resultExtraInput orElse uri.label orElse uri.message
      WalletApp.txDescriptions(signedTx.txid) = PlainTxDescription(uri.address :: Nil, transactionLabel)
      val isSent = Await.result(notifyAndBroadcast(fromWallet, signedTx), atMost = 40.seconds)
      if (!isSent) onFail(me getString error_btc_broadcast_fail)
      alert.dismiss
    }

    lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.chainEditView.host) {
      rate = LNParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(LNParams.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget)

      worker = new ThrottledWork[String, GenerateTxResponse] {
        // This is a generic sending facility which may send to non-segwit, so always use a safer high dust threshold
        override def error(exception: Throwable): Unit = update(feeOpt = None, showIssue = sendView.manager.resultMsat >= LNParams.chainWallets.params.dustLimit)
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
    alert.setOnDismissListener(sendView.onDismissListener)
    feeView.update(feeOpt = None, showIssue = false)

    uri.amount foreach { asked =>
      sendView.manager.updateText(value = asked)
      sendView.manager.inputAmount.setEnabled(false)
      sendView.manager.fiatInputAmount.setEnabled(false)
    }
  }

  def bringSendMultiBitcoinPopup(addressToAmount: MultiAddressParser.AddressToAmount, fromWallet: ElectrumEclairWallet): Unit = {
    val scriptToAmount = addressToAmount.values.firstItems.map(LNParams.addressToPubKeyScript).zip(addressToAmount.values.secondItems).toMap
    val sendView: ChainSendView = new ChainSendView(fromWallet, badge = None, visibilityRes = -1)

    def attempt(alert: AlertDialog): Unit = {
      runFutureProcessOnUI(fromWallet.makeBatchTx(scriptToAmount, feeView.rate), onFail) { response =>
        // It is fine to sum the whole map element here: possible change output will never be present in it
        val finalSendButton = sendView.chainConfirmView.chainButtonsView.chainNextButton
        val totalSendAmount = response.pubKeyScriptToAmount.values.sum.toMilliSatoshi

        if (fromWallet.isSigning) {
          finalSendButton setOnClickListener onButtonTap(process apply response.tx)
          sendView.switchToConfirm(alert, totalSendAmount, response.fee.toMilliSatoshi)
        } else if (fromWallet.hasFingerprint) {
          sendView.chainReaderView.onSignedTx = signedTx => UITask {
            if (signedTx.txOut.toSet != response.tx.txOut.toSet) alert.dismiss
            finalSendButton setOnClickListener onButtonTap(process apply signedTx)
            sendView.switchToConfirm(alert, totalSendAmount, response.fee.toMilliSatoshi)
          }.run

          val masterFingerprint = fromWallet.info.core.masterFingerprint.get
          val psbt = prepareBip84Psbt(response, masterFingerprint)
          sendView.switchToHardwareOutgoing(alert, psbt)
        } else alert.dismiss
      }
    }

    lazy val alert = {
      val title = new TitleView(me getString dialog_send_btc_many)
      val builder = titleBodyAsViewBuilder(title.asColoredView(me chainWalletBackground fromWallet), sendView.body)
      addFlowChip(title.flow, getString(dialog_send_btc_from).format(fromWallet.info.label), R.drawable.border_yellow)
      mkCheckForm(attempt, none, builder, dialog_ok, dialog_cancel)
    }

    lazy val process: Transaction => Unit = signedTx => {
      WalletApp.txDescriptions(signedTx.txid) = PlainTxDescription(addressToAmount.values.firstItems.toList)
      val isSent = Await.result(notifyAndBroadcast(fromWallet, signedTx), atMost = 40.seconds)
      if (!isSent) onFail(me getString error_btc_broadcast_fail)
      alert.dismiss
    }

    lazy val feeView = new FeeView[GenerateTxResponse](FeeratePerByte(1L.sat), sendView.chainEditView.host) {
      rate = LNParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(LNParams.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget)

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
    alert.setOnDismissListener(sendView.onDismissListener)
    feeView.update(feeOpt = None, showIssue = false)
    feeView.worker addWork "MULTI-SEND-INIT-CALL"
  }

  def bringReceivePopup: Unit = lnReceiveGuard(LNParams.cm.all.values, contentWindow) {
    val holdPeriodInMinutes: String = getString(popup_hold).format(LNParams.maxHoldSecs / 60)
    new OffChainReceiver(LNParams.cm.all.values, initMaxReceivable = Long.MaxValue.msat, initMinReceivable = 0L.msat) {
      override def getManager: RateManager = new RateManager(body, getString(dialog_add_description).asSome, dialog_visibility_sender, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
      override def processInvoice(payRequestExt: PaymentRequestExt): Unit = goToWithValue(ClassNames.qrInvoiceActivityClass, payRequestExt)
      override def getTitleText: String = getString(dialog_receive_ln)

      override def getDescription: PaymentDescription = {
        val invoiceText = manager.resultExtraInput.getOrElse(new String)
        val hold = if (manager.holdPayment.isChecked) Some(LNParams.maxHoldSecs) else None
        PaymentDescription(split = None, label = None, semanticOrder = None, invoiceText, holdPeriodSec = hold)
      }

      manager.holdPayment.setText(holdPeriodInMinutes.html)
      setVis(isVisible = true, manager.holdPayment)
    }
  }

  def bringWithdrawPopup(data: WithdrawRequest): Unit = lnReceiveGuard(LNParams.cm.all.values, contentWindow) {
    new OffChainReceiver(LNParams.cm.all.values, initMaxReceivable = data.maxWithdrawable.msat, initMinReceivable = data.minCanReceive) {
      override def getManager: RateManager = new RateManager(body, getString(dialog_set_label).asSome, dialog_visibility_private, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
      override def getDescription: PaymentDescription = PaymentDescription(split = None, label = manager.resultExtraInput, semanticOrder = None, invoiceText = new String, meta = data.descriptionOpt)
      override def getTitleText: String = getString(dialog_lnurl_withdraw).format(data.callbackUri.getHost, data.descriptionOpt.map(desc => s"<br><br>$desc") getOrElse new String)
      override def processInvoice(prExt: PaymentRequestExt): Unit = data.requestWithdraw(prExt).foreach(none, onFail)
    }
  }

  def bringPayPopup(data: PayRequest, lnUrl: LNUrl): TimerTask = UITask {
    new OffChainSender(maxSendable = LNParams.cm.maxSendable(LNParams.cm.all.values) min data.maxSendable.msat, minSendable = data.minSendable.msat max LNParams.minPayment) {
      override lazy val manager: RateManager = new RateManager(body, data.commentAllowed.map(_ => me getString dialog_add_comment), dialog_visibility_receiver, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
      val expectedIds: ExpectedIds = ExpectedIds(wantsAuth = None, wantsRandomKey = false)
      val maxCommentLength: Int = data.commentAllowed.getOrElse(0)
      val randKey: Crypto.PrivateKey = randomKey

      override def isNeutralEnabled: Boolean = manager.resultMsat >= LNParams.minPayment && manager.resultMsat <= minSendable - LNParams.minPayment
      override def isPayEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat <= maxSendable
      private def getComment: String = manager.resultExtraInput.getOrElse(new String).take(maxCommentLength)

      override def neutral(alert: AlertDialog): Unit = runAnd(alert.dismiss) {
        snack(contentWindow, getString(dialog_lnurl_splitting).format(data.callbackUri.getHost).html, dialog_cancel) foreach { snack =>
          val level2Obs = getFinal(amount = minSendable).doOnUnsubscribe(snack.dismiss).doOnTerminate(snack.dismiss)
          val level2Sub = level2Obs.subscribe(payReqFinal => proceed(payReqFinal).run, onFail)
          val listener = onButtonTap(level2Sub.unsubscribe)
          snack.setAction(dialog_cancel, listener).show
        }

        def proceed(pf: PayRequestFinal): TimerTask = UITask {
          lnSendGuard(pf.prExt, container = contentWindow) { _ =>
            val paymentOrder = SemanticOrder(id = lnUrl.request, order = -System.currentTimeMillis)
            val feeReserve = LNParams.cm.feeReserve(manager.resultMsat, typicalChainTxFee, WalletApp.capLNFeeToChain, LNParams.maxOffChainFeeAboveRatio)
            val cmd = LNParams.cm.makeSendCmd(pf.prExt, LNParams.cm.all.values.toList, feeReserve, manager.resultMsat).modify(_.split.totalSum).setTo(minSendable)
            val pd = PaymentDescription(cmd.split.asSome, label = None, semanticOrder = paymentOrder.asSome, invoiceText = new String, meta = data.meta.textPlain.asSome)
            goToWithValue(value = SplitParams(pf.prExt, pf.successAction, pd, cmd, typicalChainTxFee), target = ClassNames.qrSplitActivityClass)
          }
        }
      }

      override def send(alert: AlertDialog): Unit = runAnd(alert.dismiss) {
        val amountHuman = WalletApp.denom.parsedWithSign(manager.resultMsat, cardIn, cardZero).html
        snack(contentWindow, getString(dialog_lnurl_sending).format(amountHuman, data.callbackUri.getHost).html, dialog_cancel) foreach { snack =>
          val level2Obs = getFinal(manager.resultMsat).doOnUnsubscribe(snack.dismiss).doOnTerminate(snack.dismiss)
          val level2Sub = level2Obs.subscribe(payReqFinal => proceed(payReqFinal).run, onFail)
          val listener = onButtonTap(level2Sub.unsubscribe)
          snack.setAction(dialog_cancel, listener).show
        }

        def proceed(pf: PayRequestFinal): TimerTask = UITask {
          lnSendGuard(pf.prExt, container = contentWindow) { _ =>
            val linkOrder = SemanticOrder(id = lnUrl.request, order = Long.MinValue)
            val paymentOrder = SemanticOrder(id = lnUrl.request, order = -System.currentTimeMillis)
            val feeReserve = LNParams.cm.feeReserve(manager.resultMsat, typicalChainTxFee, WalletApp.capLNFeeToChain, LNParams.maxOffChainFeeAboveRatio)
            val cmd = LNParams.cm.makeSendCmd(pf.prExt, LNParams.cm.all.values.toList, feeReserve, manager.resultMsat).modify(_.split.totalSum).setTo(manager.resultMsat)
            val pd = PaymentDescription(split = None, label = None, semanticOrder = paymentOrder.asSome, invoiceText = new String, meta = data.meta.textPlain.asSome)
            replaceOutgoingPayment(pf.prExt, pd, pf.successAction, sentAmount = cmd.split.myPart)
            LNParams.cm.localSend(cmd)

            if (!pf.isThrowAway) {
              val currentLabel = lnUrlPayLinks.find(_.payString == lnUrl.request).flatMap(_.description.label)
              val desc = LNUrlDescription(currentLabel, linkOrder.asSome, randKey.value.toHex, pf.prExt.pr.paymentHash, pf.prExt.pr.paymentSecret.get, manager.resultMsat)
              val info = LNUrlPayLink(domain = lnUrl.uri.getHost, payString = lnUrl.request, data.metadata, updatedAt = System.currentTimeMillis, desc, pf.prExt.pr.nodeId.toString, getComment)
              WalletApp.lnUrlPayBag.saveLink(info)
            }
          }
        }
      }

      override val alert: AlertDialog = {
        val text = getString(dialog_lnurl_pay).format(data.callbackUri.getHost, s"<br><br>${data.meta.textPlain}")
        val title = titleBodyAsViewBuilder(text.asColoredView(R.color.cardLightning), manager.content)
        mkCheckFormNeutral(send, none, neutral, title, dialog_ok, dialog_cancel, dialog_split)
      }

      private def getFinal(amount: MilliSatoshi) = LNUrl level2DataResponse {
        val ids1 = if (expectedIds.wantsRandomKey) List("application/pubkey", randKey.publicKey.toString) :: Nil else Nil
        val ids2 = expectedIds.wantsAuth.filter(_ => manager.attachIdentity.isChecked).map(_.getRecord(lnUrl.uri.getHost) :: ids1) getOrElse ids1

        val base1 = data.callbackUri.buildUpon.appendQueryParameter("amount", amount.toLong.toString)
        val base2 = if (ids2.nonEmpty) base1.appendQueryParameter("payerid", ids2.toJson.compactPrint) else base1
        if (manager.resultExtraInput.isDefined) base2.appendQueryParameter("comment", getComment) else base2
      } map { rawResponse =>
        val payRequestFinal = to[PayRequestFinal](rawResponse)
        val descriptionHashOpt = payRequestFinal.prExt.pr.description.right.toOption
        require(descriptionHashOpt.contains(data.metaDataHash), s"Metadata hash mismatch, original=${data.metaDataHash}, later provided=$descriptionHashOpt")
        require(payRequestFinal.prExt.pr.amount.contains(amount), s"Payment amount mismatch, requested by wallet=$amount, provided in invoice=${payRequestFinal.prExt.pr.amount}")
        payRequestFinal.modify(_.successAction.each.domain).setTo(data.callbackUri.getHost.asSome)
      }

      manager.updateText(minSendable)
      expectedIds.wantsAuth.foreach { expectedAuth =>
        manager.attachIdentity.setChecked(expectedAuth.isMandatory)
        manager.attachIdentity.setEnabled(!expectedAuth.isMandatory)
        setVis(isVisible = true, manager.attachIdentity)
      }

      data.commentAllowed.foreach { limit =>
        manager.extraInputLayout.setCounterEnabled(true)
        manager.extraInputLayout.setCounterMaxLength(limit)
      }
    }
  }

  def paymentAdapterDataChanged: TimerTask = UITask {
    setVis(isVisible = relayedPreimageInfos.nonEmpty, walletCards.relayedPayments)
    setVis(isVisible = lnUrlPayLinks.nonEmpty, walletCards.payMarketLinks)
    setVis(isVisible = hasItems && !isSearchOn, walletCards.listCaption)
    setVis(isVisible = !hasItems, walletCards.recoveryPhrase)
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

  def notifyAndBroadcast(fromWallet: ElectrumEclairWallet, tx: Transaction): Future[Boolean] = {
    UITask(WalletApp.app quickToast pctCollected.head).run
    fromWallet.broadcast(tx)
  }
}
