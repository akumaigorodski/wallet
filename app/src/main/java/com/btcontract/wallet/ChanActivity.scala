package com.btcontract.wallet

import java.util.{Date, TimerTask}

import android.graphics.{Bitmap, BitmapFactory}
import android.os.Bundle
import android.text.Spanned
import android.view.{View, ViewGroup}
import android.widget._
import androidx.appcompat.app.AlertDialog
import androidx.cardview.widget.CardView
import androidx.recyclerview.widget.RecyclerView
import com.btcontract.wallet.BaseActivity.StringOps
import com.btcontract.wallet.Colors._
import com.btcontract.wallet.R.string._
import com.chauthai.swipereveallayout.{SwipeRevealLayout, ViewBinderHelper}
import com.google.common.cache.LoadingCache
import com.indicator.ChannelIndicatorLine
import com.ornach.nobobutton.NoboButton
import com.softwaremill.quicklens._
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.channel._
import fr.acinq.eclair.wire.HostedChannelBranding
import immortan.ChannelListener.Malfunction
import immortan._
import immortan.crypto.Tools._
import immortan.utils.{BitcoinUri, InputParser, PaymentRequestExt, Rx}
import immortan.wire.HostedState
import rx.lang.scala.Subscription

import scala.concurrent.duration._
import scala.util.Try


object ChanActivity {
  def getHcState(hc: HostedCommits): String = {
    val preimages = hc.revealedFulfills.map(_.ourPreimage.toHex).mkString("\n")
    val hostedState = HostedState(hc.remoteInfo.nodeId, hc.remoteInfo.nodeSpecificPubKey, hc.lastCrossSignedState)
    val serializedHostedState = immortan.wire.ExtCodecs.hostedStateCodec.encode(value = hostedState).require.toHex
    WalletApp.app.getString(ln_hosted_chan_state).format(getDetails(hc, "n/a"), serializedHostedState, preimages)
  }

  def getDetails(cs: Commitments, fundingTxid: String): String = {
    val shortId = cs.updateOpt.map(_.shortChannelId.toString).getOrElse("unknown")
    val stamp = WalletApp.app.when(new Date(cs.startedAt), WalletApp.app.dateFormat)
    WalletApp.app.getString(ln_chan_details).format(cs.remoteInfo.nodeId.toString,
      cs.remoteInfo.nodeSpecificPubKey.toString, shortId, fundingTxid, stamp)
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
    val hcImageContainer: CardView = swipeWrap.findViewById(R.id.hcImageContainer).asInstanceOf[CardView]
    val hcImage: ImageView = swipeWrap.findViewById(R.id.hcImage).asInstanceOf[ImageView]
    val hcInfo: ImageView = swipeWrap.findViewById(R.id.hcInfo).asInstanceOf[ImageButton]

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
        val closeInfoRes = chan.data match {
          case _: DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT => ln_info_await_close
          case close: DATA_CLOSING if close.remoteCommitPublished.nonEmpty => ln_info_close_remote
          case close: DATA_CLOSING if close.nextRemoteCommitPublished.nonEmpty => ln_info_close_remote
          case close: DATA_CLOSING if close.futureRemoteCommitPublished.nonEmpty => ln_info_close_remote
          case close: DATA_CLOSING if close.mutualClosePublished.nonEmpty => ln_info_close_coop
          case _: DATA_CLOSING => ln_info_close_local
          case _ => ln_info_shutdown
        }

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
      // Hide image container at start, show it later if bitmap is fine
      hcInfo setOnClickListener onButtonTap(me browse "https://lightning-wallet.com/posts/scaling-ln-with-hosted-channels/")
      setVisMany(true -> hcBranding, false -> hcImageContainer, hc.overrideProposal.isDefined -> overrideProposal)

      for {
        HostedChannelBranding(_, pngIcon, contactInfo) <- brandOpt
        bitmapImage <- Try(pngIcon.get.toArray).map(hcImageMemo.get)
        _ = hcImage setOnClickListener onButtonTap(me browse contactInfo)
        _ = setVis(isVisible = true, hcImageContainer)
      } hcImage.setImageBitmap(bitmapImage)

      removeItem setOnClickListener onButtonTap {
        if (hc.localSpec.htlcs.nonEmpty) snack(chanContainer, getString(ln_hosted_chan_remove_impossible).html, R.string.dialog_ok, _.dismiss)
        else mkCheckForm(alert => runAnd(alert.dismiss)(me removeHc hc), none, confirmationBuilder(hc, getString(confirm_ln_hosted_chan_remove).html), dialog_ok, dialog_cancel)
      }

      overrideProposal setOnClickListener onButtonTap {
        val newBalance = hc.lastCrossSignedState.initHostedChannel.channelCapacityMsat - hc.overrideProposal.get.localBalanceMsat
        val current = WalletApp.denom.parsedWithSign(hc.availableForSend, cardIn, cardZero)
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
    try LNParams.cm.all.values.foreach(_.listeners -= me) catch none
    updateSubscription.foreach(_.unsubscribe)
    super.onDestroy
  }

  override def onChoiceMade(tag: AnyRef, pos: Int): Unit = (tag, pos) match {
    case (cs: NormalCommits, 0) => me share ChanActivity.getDetails(cs, cs.commitInput.outPoint.txid.toString)
    case (nc: HostedCommits, 0) => me share ChanActivity.getDetails(nc, fundingTxid = "n/a")
    case (hc: HostedCommits, 1) => me share ChanActivity.getHcState(hc)
    case (cs: NormalCommits, 1) => closeNcToWallet(cs)

    case (hc: HostedCommits, 2) =>
      val builder = confirmationBuilder(hc, getString(confirm_ln_hosted_chan_drain).html)
      mkCheckForm(alert => runAnd(alert.dismiss)(me drainHc hc), none, builder, dialog_ok, dialog_cancel)

    case (cs: NormalCommits, 2) => closeNcToAddress(cs)
    case (cs: Commitments, 3) => receiveIntoChan(cs)
    case _ =>
  }

  override def onException: PartialFunction[Malfunction, Unit] = {
    case (CMDException(reason, _: CMD_CLOSE), _, data: HasNormalCommitments) => chanError(data.channelId, reason, data.commitments.remoteInfo)
    case (CMDException(reason, _: CMD_HOSTED_STATE_OVERRIDE), _, hc: HostedCommits) => chanError(hc.channelId, reason, hc.remoteInfo)
  }

  def closeNcToWallet(cs: NormalCommits): Unit = {
    bringChainWalletChooser(normalChanActions.tail.head.toString) { wallet =>
      runFutureProcessOnUI(wallet.getReceiveAddresses, onFail) { addressResponse =>
        val pubKeyScript = LNParams.addressToPubKeyScript(addressResponse.firstAccountAddress)
        for (chan <- me getChanByCommits cs) chan process CMD_CLOSE(pubKeyScript.asSome, force = false)
      }
    }
  }

  def closeNcToAddress(cs: NormalCommits): Unit = {
    def confirmResolve(bitcoinUri: BitcoinUri): Unit = {
      val pubKeyScript = LNParams.addressToPubKeyScript(bitcoinUri.address)
      val builder = confirmationBuilder(cs, getString(confirm_ln_normal_chan_close_address).format(bitcoinUri.address.humanFour).html)
      def proceed: Unit = for (chan <- me getChanByCommits cs) chan process CMD_CLOSE(pubKeyScript.asSome, force = false)
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)
    }

    def resolveClosingAddress: Unit = InputParser.checkAndMaybeErase {
      case ext: PaymentRequestExt if ext.pr.fallbackAddress.isDefined => ext.pr.fallbackAddress.map(BitcoinUri.fromRaw).foreach(confirmResolve)
      case bitcoinUri: BitcoinUri if Try(LNParams addressToPubKeyScript bitcoinUri.address).isSuccess => confirmResolve(bitcoinUri)
      case _ => nothingUsefulTask.run
    }

    def onData: Runnable = UITask(resolveClosingAddress)
    val sheet = new sheets.OnceBottomSheet(me, getString(scan_btc_address).asSome, onData)
    callScanner(sheet)
  }

  def drainHc(hc: HostedCommits): Unit = {
    val relatedHc = getChanByCommits(hc).toList
    val maxSendable = LNParams.cm.maxSendable(relatedHc)
    val preimage = randomBytes32

    maxNormalReceivable match {
      case _ if maxSendable < LNParams.minPayment => snack(chanContainer, getString(ln_hosted_chan_drain_impossible_few_funds).html, R.string.dialog_ok, _.dismiss)
      case ncOpt if ncOpt.forall(_.maxReceivable < LNParams.minPayment) => snack(chanContainer, getString(ln_hosted_chan_drain_impossible_no_chans).html, R.string.dialog_ok, _.dismiss)
      case Some(csAndMax) =>
        val toSend = maxSendable.min(csAndMax.maxReceivable)
        val pd = PaymentDescription(split = None, label = getString(tx_ln_label_reflexive).asSome, semanticOrder = None, invoiceText = new String, toSelfPreimage = preimage.asSome)
        val prExt = LNParams.cm.makePrExt(toReceive = toSend, description = pd, allowedChans = csAndMax.commits, hash = Crypto.sha256(preimage), secret = randomBytes32)
        val cmd = LNParams.cm.makeSendCmd(prExt, toSend, allowedChans = relatedHc, typicalChainTxFee, capLNFeeToChain = false).modify(_.split.totalSum).setTo(toSend)
        WalletApp.app.quickToast(getString(dialog_lnurl_processing).format(me getString tx_ln_label_reflexive).html)
        replaceOutgoingPayment(prExt, pd, action = None, sentAmount = prExt.pr.amount.get)
        LNParams.cm.localSend(cmd)
    }
  }

  def receiveIntoChan(commits: Commitments): Unit = {
    lnReceiveGuard(getChanByCommits(commits).toList, chanContainer) {
      new OffChainReceiver(getChanByCommits(commits).toList, initMaxReceivable = Long.MaxValue.msat, initMinReceivable = 0L.msat) {
        override def getManager: RateManager = new RateManager(body, getString(dialog_add_description).asSome, dialog_visibility_sender, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
        override def getDescription: PaymentDescription = PaymentDescription(split = None, label = None, semanticOrder = None, invoiceText = manager.resultExtraInput getOrElse new String)
        override def processInvoice(prExt: PaymentRequestExt): Unit = goToWithValue(ClassNames.qrInvoiceActivityClass, prExt)
        override def getTitleText: String = getString(dialog_receive_ln)
      }
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
    val sheet = new sheets.OnceBottomSheet(me, getString(chan_open_scan).asSome, onData)
    callScanner(sheet)
  }

  def INIT(state: Bundle): Unit = {
    if (WalletApp.isAlive && LNParams.isOperational) {
      for (channel <- LNParams.cm.all.values) channel.listeners += me
      setContentView(R.layout.activity_chan)
      updateChanData.run

      val title = new TitleView(me getString title_chans)
      title.view.setOnClickListener(me onButtonTap finish)
      title.backArrow.setVisibility(View.VISIBLE)
      chanList.addHeaderView(title.view)

      val footer = new TitleView(me getString chan_open)
      addFlowChip(footer.flow, getString(chan_open_scan), R.drawable.border_blue, _ => scanNodeQr)
      if (LNParams.isMainnet) addFlowChip(footer.flow, getString(chan_open_lnbig), R.drawable.border_blue, _ => me browse "https://lnbig.com/#/open-channel")
      if (LNParams.isMainnet) addFlowChip(footer.flow, getString(chan_open_bitrefill), R.drawable.border_blue, _ => me browse "https://www.bitrefill.com/buy/lightning-channel")
      if (LNParams.isMainnet && LNParams.cm.allHostedCommits.isEmpty) addFlowChip(footer.flow, getString(rpa_request_hc), R.drawable.border_yellow, _ => requestHostedChannel)
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
