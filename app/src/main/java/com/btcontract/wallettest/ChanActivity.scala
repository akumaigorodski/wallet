package com.btcontract.wallettest

import immortan._
import android.widget._
import fr.acinq.eclair._
import fr.acinq.bitcoin._
import immortan.crypto.Tools._
import fr.acinq.eclair.channel._
import scala.concurrent.duration._
import com.btcontract.wallettest.Colors._
import com.btcontract.wallettest.R.string._

import java.util.{Date, TimerTask}
import android.view.{View, ViewGroup}
import android.graphics.{Bitmap, BitmapFactory}
import immortan.utils.{BitcoinUri, InputParser, PaymentRequestExt, Rx}
import com.chauthai.swipereveallayout.{SwipeRevealLayout, ViewBinderHelper}
import com.btcontract.wallettest.BaseActivity.StringOps
import fr.acinq.eclair.wire.HostedChannelBranding
import androidx.recyclerview.widget.RecyclerView
import immortan.ChannelListener.Malfunction
import com.google.common.cache.LoadingCache
import com.indicator.ChannelIndicatorLine
import androidx.appcompat.app.AlertDialog
import androidx.cardview.widget.CardView
import com.ornach.nobobutton.NoboButton
import rx.lang.scala.Subscription
import immortan.wire.HostedState
import android.text.Spanned
import android.os.Bundle


object ChanActivity {
  def getHcState(hc: HostedCommits): String = {
    val preimages = hc.revealedFulfills.map(_.ourPreimage.toHex).mkString("\n")
    val hostedState = HostedState(hc.remoteInfo.nodeId, hc.remoteInfo.nodeSpecificPubKey, hc.lastCrossSignedState)
    val serializedHostedState = immortan.wire.ExtCodecs.hostedStateCodec.encode(value = hostedState).require.toHex
    WalletApp.app.getString(ln_hosted_chan_state).format(getDetails(hc), serializedHostedState, preimages)
  }

  def getDetails(cs: Commitments): String = {
    val remoteId = cs.remoteInfo.nodeId.toString
    val localId = cs.remoteInfo.nodeSpecificPubKey.toString
    val shortId = cs.updateOpt.map(_.shortChannelId.toString).getOrElse("unknown")
    val stamp = WalletApp.app.when(new Date(cs.startedAt), WalletApp.app.dateFormat)
    WalletApp.app.getString(ln_chan_details).format(remoteId, localId, shortId, stamp)
  }
}

class ChanActivity extends ChanErrorHandlerActivity with ChoiceReceiver with HasTypicalChainFee with ChannelListener { me =>
  private[this] lazy val chanContainer = findViewById(R.id.chanContainer).asInstanceOf[LinearLayout]
  private[this] lazy val chanList = findViewById(R.id.chanList).asInstanceOf[ListView]

  private[this] lazy val brandingInfos = WalletApp.txDataBag.db.txWrap(getBrandingInfos.toMap)
  private[this] lazy val normalChanActions = getResources.getStringArray(R.array.ln_normal_chan_actions).map(_.html)
  private[this] lazy val hostedChanActions = getResources.getStringArray(R.array.ln_hosted_chan_actions).map(_.html)
  private[this] var updateSubscription = Option.empty[Subscription]
  private[this] var csToDisplay = Seq.empty[ChanAndCommits]

  val hcImageMemo: LoadingCache[Bytes, Bitmap] = memoize {
    bytes => BitmapFactory.decodeByteArray(bytes, 0, bytes.length)
  }

  val chanAdapter: BaseAdapter = new BaseAdapter {
    private[this] val viewBinderHelper = new ViewBinderHelper
    override def getItem(pos: Int): ChanAndCommits = csToDisplay(pos)
    override def getItemId(position: Int): Long = position
    override def getCount: Int = csToDisplay.size

    def getView(position: Int, savedView: View, parent: ViewGroup): View = {
      val card = if (null == savedView) getLayoutInflater.inflate(R.layout.frag_chan_card, null) else savedView

      val cardView = (getItem(position), card.getTag) match {
        case (ChanAndCommits(chan: ChannelHosted, hc: HostedCommits), view: HostedViewHolder) => view.fill(chan, hc)
        case (ChanAndCommits(chan: ChannelHosted, hc: HostedCommits), _) => new HostedViewHolder(card).fill(chan, hc)
        case (ChanAndCommits(chan: ChannelNormal, commits: NormalCommits), view: NormalViewHolder) => view.fill(chan, commits)
        case (ChanAndCommits(chan: ChannelNormal, commits: NormalCommits), _) => new NormalViewHolder(card).fill(chan, commits)
        case _ => throw new RuntimeException
      }

      viewBinderHelper.bind(cardView.swipeWrap, position.toString)
      card.setTag(cardView)
      card
    }
  }

  abstract class ChanCardViewHolder(view: View) extends RecyclerView.ViewHolder(view) {
    val swipeWrap: SwipeRevealLayout = itemView.asInstanceOf[SwipeRevealLayout]

    val removeItem: NoboButton = swipeWrap.findViewById(R.id.removeItem).asInstanceOf[NoboButton]
    val channelCard: CardView = swipeWrap.findViewById(R.id.channelCard).asInstanceOf[CardView]

    val hcBranding: RelativeLayout = swipeWrap.findViewById(R.id.hcBranding).asInstanceOf[RelativeLayout]
    val hcSupportInfo: TextView = swipeWrap.findViewById(R.id.hcSupportInfo).asInstanceOf[TextView]
    val hcImage: ImageView = swipeWrap.findViewById(R.id.hcImage).asInstanceOf[ImageView]

    val baseBar: ProgressBar = swipeWrap.findViewById(R.id.baseBar).asInstanceOf[ProgressBar]
    val overBar: ProgressBar = swipeWrap.findViewById(R.id.overBar).asInstanceOf[ProgressBar]
    val peerAddress: TextView = swipeWrap.findViewById(R.id.peerAddress).asInstanceOf[TextView]
    val chanState: View = swipeWrap.findViewById(R.id.chanState).asInstanceOf[View]

    val canSendText: TextView = swipeWrap.findViewById(R.id.canSendText).asInstanceOf[TextView]
    val canReceiveText: TextView = swipeWrap.findViewById(R.id.canReceiveText).asInstanceOf[TextView]
    val refundableAmountText: TextView = swipeWrap.findViewById(R.id.refundableAmountText).asInstanceOf[TextView]
    val paymentsInFlightText: TextView = swipeWrap.findViewById(R.id.paymentsInFlightText).asInstanceOf[TextView]
    val totalCapacityText: TextView = swipeWrap.findViewById(R.id.totalCapacityText).asInstanceOf[TextView]
    val overrideProposal: TextView = swipeWrap.findViewById(R.id.overrideProposal).asInstanceOf[TextView]
    val extraInfoText: TextView = swipeWrap.findViewById(R.id.extraInfoText).asInstanceOf[TextView]

    val wrappers: Seq[View] =
      swipeWrap.findViewById(R.id.progressBars).asInstanceOf[View] ::
        swipeWrap.findViewById(R.id.totalCapacity).asInstanceOf[View] ::
        swipeWrap.findViewById(R.id.refundableAmount).asInstanceOf[View] ::
        swipeWrap.findViewById(R.id.paymentsInFlight).asInstanceOf[View] ::
        swipeWrap.findViewById(R.id.canReceive).asInstanceOf[View] ::
        swipeWrap.findViewById(R.id.canSend).asInstanceOf[View] ::
        Nil

    def visibleExcept(goneRes: Int*): Unit = for (wrap <- wrappers) {
      val hideView = goneRes.contains(wrap.getId)
      setVis(!hideView, wrap)
    }

    baseBar.setMax(1000)
    overBar.setMax(1000)
  }

  class NormalViewHolder(view: View) extends ChanCardViewHolder(view) {
    def fill(chan: ChannelNormal, cs: NormalCommits): NormalViewHolder = {

      val capacity: Satoshi = cs.commitInput.txOut.amount
      val barCanReceive = (cs.availableForReceive.toLong / capacity.toLong).toInt
      val barCanSend = (cs.latestReducedRemoteSpec.toRemote.toLong / capacity.toLong).toInt
      val barLocalReserve = (cs.latestReducedRemoteSpec.toRemote - cs.availableForSend).toLong / capacity.toLong
      val tempFeeMismatch = chan.data match { case norm: DATA_NORMAL => norm.feeUpdateRequired case _ => false }
      val inFlight: MilliSatoshi = cs.latestReducedRemoteSpec.htlcs.foldLeft(0L.msat)(_ + _.add.amountMsat)
      val refundable: MilliSatoshi = cs.latestReducedRemoteSpec.toRemote + inFlight

      if (Channel isWaiting chan) {
        setVis(isVisible = true, extraInfoText)
        extraInfoText.setText(getString(ln_info_opening).html)
        channelCard setOnClickListener bringChanOptions(normalChanActions.take(2), cs)
        visibleExcept(R.id.progressBars, R.id.paymentsInFlight, R.id.canReceive, R.id.canSend)
      } else if (Channel isOperational chan) {
        channelCard setOnClickListener bringChanOptions(normalChanActions, cs)
        setVis(isVisible = cs.updateOpt.isEmpty || tempFeeMismatch, extraInfoText)
        if (cs.updateOpt.isEmpty) extraInfoText.setText(ln_info_no_update)
        if (tempFeeMismatch) extraInfoText.setText(ln_info_fee_mismatch)
        visibleExcept(goneRes = -1)
      } else {
        val closeInfoRes = chan.data match { case c: DATA_CLOSING => closedBy(c) case _ => ln_info_shutdown }
        channelCard setOnClickListener bringChanOptions(normalChanActions.take(2), cs)
        visibleExcept(R.id.progressBars, R.id.canReceive, R.id.canSend)
        extraInfoText.setText(getString(closeInfoRes).html)
        setVis(isVisible = true, extraInfoText)
      }

      removeItem setOnClickListener onButtonTap {
        def proceed: Unit = chan process CMD_CLOSE(None, force = true)
        val builder = confirmationBuilder(cs, getString(confirm_ln_normal_chan_force_close).html)
        mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)
      }

      setVis(isVisible = false, overrideProposal)
      setVis(isVisible = false, hcBranding)

      ChannelIndicatorLine.setView(chanState, chan)
      peerAddress.setText(peerInfo(cs.remoteInfo).html)
      overBar.setProgress(barCanSend min barLocalReserve.toInt)
      baseBar.setSecondaryProgress(barCanSend + barCanReceive)
      baseBar.setProgress(barCanSend)

      totalCapacityText.setText(sumOrNothing(capacity.toMilliSatoshi, cardIn).html)
      canReceiveText.setText(sumOrNothing(cs.availableForReceive, cardOut).html)
      canSendText.setText(sumOrNothing(cs.availableForSend, cardIn).html)
      refundableAmountText.setText(sumOrNothing(refundable, cardIn).html)
      paymentsInFlightText.setText(sumOrNothing(inFlight, cardIn).html)
      this
    }
  }

  class HostedViewHolder(view: View) extends ChanCardViewHolder(view) {
    def fill(chan: ChannelHosted, hc: HostedCommits): HostedViewHolder = {
      val capacity = hc.lastCrossSignedState.initHostedChannel.channelCapacityMsat
      val inFlight = hc.nextLocalSpec.htlcs.foldLeft(0L.msat)(_ + _.add.amountMsat)
      val barCanReceive = (hc.availableForReceive.toLong / capacity.truncateToSatoshi.toLong).toInt
      val barCanSend = (hc.availableForSend.toLong / capacity.truncateToSatoshi.toLong).toInt

      val errorText = (hc.localError, hc.remoteError) match {
        case Some(error) ~ _ => s"LOCAL: ${ErrorExt extractDescription error}"
        case _ ~ Some(error) => s"REMOTE: ${ErrorExt extractDescription error}"
        case _ => new String
      }

      val brandOpt = brandingInfos.get(hc.remoteInfo.nodeId)
      setVis(isVisible = hc.overrideProposal.isDefined, overrideProposal)
      setVis(isVisible = brandOpt.isDefined, hcBranding)

      for (HostedChannelBranding(_, pngIcon, contactInfo) <- brandOpt) {
        pngIcon.map(_.toArray).map(hcImageMemo.get).foreach(hcImage.setImageBitmap)
        hcSupportInfo.setText(contactInfo)
      }

      removeItem setOnClickListener onButtonTap {
        if (hc.localSpec.htlcs.nonEmpty) snack(chanContainer, getString(ln_hosted_chan_remove_impossible).html, R.string.dialog_ok, _.dismiss)
        else mkCheckForm(alert => runAnd(alert.dismiss)(me removeHc hc), none, confirmationBuilder(hc, getString(confirm_ln_hosted_chan_remove).html), dialog_ok, dialog_cancel)
        swipeWrap.close(true)
      }

      overrideProposal setOnClickListener onButtonTap {
        val newBalance = hc.lastCrossSignedState.initHostedChannel.channelCapacityMsat - hc.overrideProposal.get.localBalanceMsat
        val current = WalletApp.denom.parsedWithSign(hc.availableForReceive, cardIn, cardZero)
        val overridden = WalletApp.denom.parsedWithSign(newBalance, cardIn, cardZero)

        def proceed: Unit = chan process CMD_HOSTED_STATE_OVERRIDE(hc.overrideProposal.get)
        val builder = confirmationBuilder(hc, getString(ln_hc_override_warn).format(current, overridden).html)
        mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)
      }

      channelCard setOnClickListener bringChanOptions(hostedChanActions, hc)

      visibleExcept(R.id.refundableAmount)
      ChannelIndicatorLine.setView(chanState, chan)
      peerAddress.setText(peerInfo(hc.remoteInfo).html)
      baseBar.setSecondaryProgress(barCanSend + barCanReceive)
      baseBar.setProgress(barCanSend)

      totalCapacityText.setText(sumOrNothing(capacity, cardIn).html)
      canReceiveText.setText(sumOrNothing(hc.availableForReceive, cardOut).html)
      canSendText.setText(sumOrNothing(hc.availableForSend, cardIn).html)
      paymentsInFlightText.setText(sumOrNothing(inFlight, cardIn).html)

      // Order messages by degree of importance since user can only see a single one
      setVis(isVisible = hc.error.isDefined || hc.updateOpt.isEmpty, extraInfoText)
      extraInfoText.setText(ln_info_no_update)
      extraInfoText.setText(errorText)
      this
    }
  }

  override def onDestroy: Unit = {
    updateSubscription.foreach(_.unsubscribe)
    super.onDestroy
  }

  override def onChoiceMade(tag: AnyRef, pos: Int): Unit = (tag, pos) match {
    case (commits: Commitments, 0) => share(ChanActivity getDetails commits)
    case (hc: HostedCommits, 1) => share(ChanActivity getHcState hc)

    case (cs: NormalCommits, 1) =>
      val builder = confirmationBuilder(cs, getString(confirm_ln_normal_chan_close_wallet).html)
      def proceed: Unit = for (chan <- me getChanByCommits cs) chan process CMD_CLOSE(None, force = false)
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)

    case (hc: HostedCommits, 2) =>
      val builder = confirmationBuilder(hc, getString(confirm_ln_hosted_chan_drain).html)
      mkCheckForm(alert => runAnd(alert.dismiss)(me drainHc hc), none, builder, dialog_ok, dialog_cancel)

    case (cs: NormalCommits, 2) => closeNcToAddress(cs)
    case _ =>
  }

  override def onException: PartialFunction[Malfunction, Unit] = {
    case (CMDException(reason, _: CMD_CLOSE), _, data: HasNormalCommitments) => chanError(data.channelId, reason, data.commitments.remoteInfo)
    case (CMDException(reason, _: CMD_HOSTED_STATE_OVERRIDE), _, hc: HostedCommits) => chanError(hc.channelId, reason, hc.remoteInfo)
  }

  def closeNcToAddress(cs: NormalCommits): Unit = {
    def confirmResolve(bitcoinUri: BitcoinUri): Unit = {
      def proceed: Unit = for (chan <- me getChanByCommits cs) chan process CMD_CLOSE(Script.write(bitcoinUri.pubKeyScript).asSome, force = false)
      val builder = confirmationBuilder(cs, getString(confirm_ln_normal_chan_close_address).format(bitcoinUri.address.humanFour).html)
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)
    }

    def resolveClosingAddress: Unit = InputParser.checkAndMaybeErase {
      case ext: PaymentRequestExt if ext.pr.fallbackAddress.isDefined => ext.pr.fallbackAddress.map(BitcoinUri.fromRaw).foreach(confirmResolve)
      case closingBitcoinUri: BitcoinUri if closingBitcoinUri.isValid => confirmResolve(closingBitcoinUri)
      case _ => nothingUsefulTask.run
    }

    def onData: Runnable = UITask(resolveClosingAddress)
    val instruction: Option[String] = getString(scan_btc_address).asSome
    val sheet = new sheets.ScannerBottomSheet(me, instruction, onData)
    callScanner(sheet)
  }

  def drainHc(hc: HostedCommits): Unit = {
    val relatedHc = getChanByCommits(hc).toList

    maxNormalReceivable match {
      case _ if LNParams.cm.maxSendable(relatedHc) < LNParams.minPayment => snack(chanContainer, getString(ln_hosted_chan_drain_impossible_few_funds).html, R.string.dialog_ok, _.dismiss)
      case ncOpt if ncOpt.forall(_.maxReceivable < LNParams.minPayment) => snack(chanContainer, getString(ln_hosted_chan_drain_impossible_no_chans).html, R.string.dialog_ok, _.dismiss)
      case Some(csAndMax) => LNParams.cm.localSendToSelf(relatedHc, csAndMax, randomBytes32, typicalChainTxFee, capLNFeeToChain = false)
    }
  }

  def removeHc(hc: HostedCommits): Unit = {
    LNParams.cm.chanBag.delete(hc.channelId)
    LNParams.cm.all -= hc.channelId

    // Update hub activity balance and chan list here
    ChannelMaster.next(ChannelMaster.stateUpdateStream)
    CommsTower.disconnectNative(hc.remoteInfo)
    updateChanData.run
  }

  def scanNodeQr: Unit = {
    def resolveNodeQr: Unit = InputParser.checkAndMaybeErase {
      case _: RemoteNodeInfo => me exitTo ClassNames.remotePeerActivityClass
      case _ => nothingUsefulTask.run
    }

    def onData: Runnable = UITask(resolveNodeQr)
    val instruction: Option[String] = getString(chan_open_scan).asSome
    val sheet = new sheets.ScannerBottomSheet(me, instruction, onData)
    callScanner(sheet)
  }

  def INIT(state: Bundle): Unit = {
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_chan)
      updateChanData.run

      val title = new TitleView(me getString title_chans)
      title.view.setOnClickListener(me onButtonTap finish)
      title.backArrow.setVisibility(View.VISIBLE)
      chanList.addHeaderView(title.view)

      val footer = new TitleView(me getString chan_open)
      val isMainnet = LNParams.chainHash == Block.LivenetGenesisBlock.hash

      addFlowChip(footer.flow, getString(chan_open_scan), R.drawable.border_blue, _ => scanNodeQr)
      if (isMainnet) addFlowChip(footer.flow, getString(chan_open_lnbig), R.drawable.border_blue, _ => me browse "https://lnbig.com/#/open-channel")
      if (isMainnet) addFlowChip(footer.flow, getString(chan_open_bitrefill), R.drawable.border_blue, _ => me browse "https://www.bitrefill.com/buy/lightning-channel")
      if (isMainnet && LNParams.cm.allHostedCommits.isEmpty && LNParams.currentBlockDay > 0) addFlowChip(footer.flow, getString(rpa_request_hc), R.drawable.border_yellow, _ => requestHostedChannel)
      chanList.addFooterView(footer.view)
      chanList.setAdapter(chanAdapter)
      chanList.setDividerHeight(0)
      chanList.setDivider(null)

      val window = 500.millis
      val stateEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.stateUpdateStream, window)
      val statusEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.statusUpdateStream, window)
      updateSubscription = stateEvents.merge(statusEvents).subscribe(_ => updateChanData.run).asSome
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }
  }

  private def requestHostedChannel: Unit = {
    HubActivity.requestHostedChannel
    finish
  }

  private def getBrandingInfos = for {
    ChanAndCommits(_: ChannelHosted, commits) <- csToDisplay
    brand <- WalletApp.extDataBag.tryGetBranding(commits.remoteInfo.nodeId).toOption
  } yield commits.remoteInfo.nodeId -> brand

  private def sumOrNothing(amt: MilliSatoshi, mainColor: String): String = {
    if (0L.msat != amt) WalletApp.denom.parsedWithSign(amt, mainColor, cardZero)
    else getString(chan_nothing)
  }

  private def closedBy(cd: DATA_CLOSING): Int = {
    if (cd.remoteCommitPublished.nonEmpty) ln_info_close_remote
    else if (cd.nextRemoteCommitPublished.nonEmpty) ln_info_close_remote
    else if (cd.futureRemoteCommitPublished.nonEmpty) ln_info_close_remote
    else if (cd.mutualClosePublished.nonEmpty) ln_info_close_coop
    else ln_info_close_local
  }

  private def peerInfo(info: RemoteNodeInfo): String = s"<strong>${info.nodeId.toString.take(16).humanFour}</strong><br>${info.address.toString}"

  private def confirmationBuilder(commits: Commitments, msg: CharSequence) = new AlertDialog.Builder(me).setTitle(commits.remoteInfo.address.toString).setMessage(msg)

  private def getChanByCommits(commits: Commitments) = csToDisplay.collectFirst { case cnc if cnc.commits.channelId == commits.channelId => cnc.chan }

  private def maxNormalReceivable = LNParams.cm.maxReceivable(LNParams.cm sortedReceivable LNParams.cm.allNormal)

  private def updateChanData: TimerTask = UITask {
    csToDisplay = LNParams.cm.all.values.flatMap(Channel.chanAndCommitsOpt).toList
    chanAdapter.notifyDataSetChanged
  }

  def bringChanOptions(options: Array[Spanned], cs: Commitments): View.OnClickListener = onButtonTap {
    val list = me selectorList new ArrayAdapter(me, android.R.layout.simple_expandable_list_item_1, options)
    new sheets.ChoiceBottomSheet(list, cs, me).show(getSupportFragmentManager, "unused-tag")
  }
}
