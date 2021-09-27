package com.btcontract.wallettest

import fr.acinq.eclair._
import com.btcontract.wallettest.R.string._
import com.btcontract.wallettest.Colors.{cardIn, cardOut, cardZero}
import immortan.utils.{WalletEventsCatcher, WalletEventsListener}
import fr.acinq.eclair.blockchain.CurrentBlockCount
import android.widget.LinearLayout
import java.net.InetSocketAddress
import android.os.Bundle
import android.view.View
import immortan.LNParams
import java.util.Date


class StatActivity extends BaseActivity { me =>
  lazy private[this] val statContainer = findViewById(R.id.settingsContainer).asInstanceOf[LinearLayout]

  private[this] val chainListener = new WalletEventsListener {
    override def onChainMasterSelected(event: InetSocketAddress): Unit = UITask(updateView).run
    override def onChainTipKnown(event: CurrentBlockCount): Unit = UITask(updateView).run
    override def onChainDisconnected: Unit = UITask(updateView).run
  }

  override def onDestroy: Unit = {
    val remove = WalletEventsCatcher.Remove(chainListener)
    LNParams.chainWallets.catcher ! remove
    super.onDestroy
  }

  def INIT(state: Bundle): Unit = {
    if (WalletApp.isAlive && LNParams.isOperational) {
      LNParams.chainWallets.catcher ! chainListener
      setContentView(R.layout.activity_settings)
      updateView
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }
  }

  def updateView: Unit = {
    statContainer.removeAllViewsInLayout
    val title = new TitleView(me getString settings_stats)
    title.view.setOnClickListener(me onButtonTap finish)
    title.backArrow.setVisibility(View.VISIBLE)
    statContainer.addView(title.view)

    WalletApp.txDataBag.db.txWrap {
      val txSummary = WalletApp.txDataBag.txSummary.filter(_.count > 0)
      val relaySummary = LNParams.cm.payBag.relaySummary.filter(_.count > 0)
      val paymentSummary = LNParams.cm.payBag.paymentSummary.filter(_.count > 0)
      val channelTxFeesSummary = LNParams.cm.chanBag.channelTxFeesSummary.filter(_.count > 0)

      val netTitle = new TitleView(me getString stats_title_network)
      val chainNode = WalletApp.currentChainNode.map(_.toString).getOrElse("n/a")
      val blockCount = if (LNParams.blockCount.get == 0L) "n/a" else LNParams.blockCount.get.toString
      statContainer.addView(netTitle.view)

      addFlowChip(netTitle.flow, getString(stats_item_chain_node).format(chainNode), R.drawable.border_gray)
      addFlowChip(netTitle.flow, getString(stats_item_chain_tip).format(blockCount), R.drawable.border_gray)

      if (LNParams.cm.all.nonEmpty) {
        val graphResync = WalletApp.app.when(new Date(LNParams.cm.pf.getLastNormalResyncStamp), WalletApp.app.dateFormat)
        val phcResync = WalletApp.app.when(new Date(LNParams.cm.pf.getLastTotalResyncStamp), WalletApp.app.dateFormat)
        addFlowChip(netTitle.flow, getString(stats_item_graph).format(graphResync), R.drawable.border_gray)
        addFlowChip(netTitle.flow, getString(stats_item_phc).format(phcResync), R.drawable.border_gray)
      }

      for (summary <- txSummary) {
        val slotTitle = new TitleView(me getString stats_title_chain)
        addFlowChip(slotTitle.flow, getString(stats_item_transactions).format(summary.count), R.drawable.border_gray)
        addFlowChip(slotTitle.flow, getString(stats_item_received) format WalletApp.denom.directedWithSign(summary.received.toMilliSatoshi, 0L.msat, cardOut, cardIn, cardZero, isIncoming = true), R.drawable.border_gray)
        addFlowChip(slotTitle.flow, getString(stats_item_sent) format WalletApp.denom.directedWithSign(0L.msat, summary.sent.toMilliSatoshi, cardOut, cardIn, cardZero, isIncoming = false), R.drawable.border_gray)
        addFlowChip(slotTitle.flow, getString(stats_item_fees) format WalletApp.denom.directedWithSign(0L.msat, summary.fees.toMilliSatoshi, cardOut, cardIn, cardZero, isIncoming = false), R.drawable.border_gray)
        statContainer.addView(slotTitle.view)
      }

      for (summary <- paymentSummary) {
        val slotTitle = new TitleView(me getString stats_title_ln)
        addFlowChip(slotTitle.flow, getString(stats_item_payments).format(summary.count), R.drawable.border_gray)
        addFlowChip(slotTitle.flow, getString(stats_item_received) format WalletApp.denom.directedWithSign(summary.received, 0L.msat, cardOut, cardIn, cardZero, isIncoming = true), R.drawable.border_gray)
        addFlowChip(slotTitle.flow, getString(stats_item_sent) format WalletApp.denom.directedWithSign(0L.msat, summary.sent, cardOut, cardIn, cardZero, isIncoming = false), R.drawable.border_gray)
        addFlowChip(slotTitle.flow, getString(stats_item_fees) format WalletApp.denom.directedWithSign(0L.msat, summary.fees, cardOut, cardIn, cardZero, isIncoming = false), R.drawable.border_gray)
        val feesSaved = WalletApp.denom.directedWithSign(summary.chainFees - summary.fees, 0L.msat, cardOut, cardIn, cardZero, summary.chainFees > summary.fees)
        addFlowChip(slotTitle.flow, getString(stats_item_fees_saved) format feesSaved, R.drawable.border_gray)
        statContainer.addView(slotTitle.view)
      }

      for (summary <- relaySummary) {
        val slotTitle = new TitleView(me getString stats_title_relays)
        addFlowChip(slotTitle.flow, getString(stats_item_relays).format(summary.count), R.drawable.border_gray)
        addFlowChip(slotTitle.flow, getString(stats_item_relayed) format WalletApp.denom.parsedWithSign(summary.relayed, cardIn, cardZero), R.drawable.border_gray)
        addFlowChip(slotTitle.flow, getString(stats_item_earned) format WalletApp.denom.directedWithSign(summary.earned, 0L.msat, cardOut, cardIn, cardZero, isIncoming = true), R.drawable.border_gray)
        statContainer.addView(slotTitle.view)
      }

      for (summary <- channelTxFeesSummary) {
        val slotTitle = new TitleView(me getString stats_title_chan_loss)
        addFlowChip(slotTitle.flow, getString(stats_item_transactions).format(summary.count), R.drawable.border_gray)
        addFlowChip(slotTitle.flow, getString(stats_item_fees) format WalletApp.denom.directedWithSign(0L.msat, summary.fees.toMilliSatoshi, cardOut, cardIn, cardZero, isIncoming = false), R.drawable.border_gray)
        statContainer.addView(slotTitle.view)
      }
    }
  }
}
