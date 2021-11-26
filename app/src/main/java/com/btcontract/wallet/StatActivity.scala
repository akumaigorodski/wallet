package com.btcontract.wallet

import android.os.Bundle
import android.view.View
import android.widget.LinearLayout
import com.btcontract.wallet.Colors.{cardIn, cardOut, cardZero}
import com.btcontract.wallet.R.string._
import fr.acinq.eclair._
import immortan.LNParams


class StatActivity extends BaseActivity { me =>
  lazy private[this] val statContainer = findViewById(R.id.settingsContainer).asInstanceOf[LinearLayout]

  def INIT(state: Bundle): Unit = {
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_settings)
      updateView
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }
  }

  def updateView: Unit = {
    val title = new TitleView(me getString settings_stats)
    title.view.setOnClickListener(me onButtonTap finish)
    title.backArrow.setVisibility(View.VISIBLE)
    statContainer.addView(title.view)

    WalletApp.txDataBag.db.txWrap {
      val txSummary = WalletApp.txDataBag.txSummary.filter(_.count > 0)
      val relaySummary = LNParams.cm.payBag.relaySummary.filter(_.count > 0)
      val paymentSummary = LNParams.cm.payBag.paymentSummary.filter(_.count > 0)
      val channelTxFeesSummary = LNParams.cm.chanBag.channelTxFeesSummary.filter(_.count > 0)

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
