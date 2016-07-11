package com.btcontract.wallet.lightning

import org.bitcoinj.core.{Coin, Transaction}
import rx.lang.scala.{Observable => Obs, Subscription}
import collection.JavaConverters.asScalaBufferConverter
import scala.concurrent.duration.DurationInt
import com.btcontract.wallet.helper.Insight
import com.btcontract.wallet.Utils.app
import org.bitcoinj


class ChainData {
  def watchTxDepthLocal(watchTxHash: String) = Obs.create[Int] { obs =>
    val lst = new bitcoinj.core.listeners.TransactionConfidenceEventListener {
      def onTransactionConfidenceChanged(w: bitcoinj.wallet.Wallet, tx: Transaction) =
        if (tx.getHashAsString == watchTxHash) obs onNext tx.getConfidence.getDepthInBlocks
    }

    app.kit.wallet addTransactionConfidenceEventListener lst
    Subscription(app.kit.wallet removeTransactionConfidenceEventListener lst)
  }

  def watchOutputSpentLocal(watchTxHash: String) = Obs.create[Transaction] { obs =>
    val recListener = new bitcoinj.wallet.listeners.WalletCoinsReceivedEventListener { me =>
      def spendsAnchor(tx: Transaction) = tx.getInputs.asScala.exists(_.getOutpoint.getHash.toString == watchTxHash)
      def onCoinsReceived(w: bitcoinj.wallet.Wallet, tx: Transaction, pb: Coin, nb: Coin) = if (me spendsAnchor tx) obs onNext tx
    }

    app.kit.wallet addCoinsReceivedEventListener recListener
    Subscription(app.kit.wallet addCoinsReceivedEventListener recListener)
  }

  def watchTxDepthRemote(data: AnchorTxData) = Insight.txs(data.address)
    .filter(_.txid == data.txId).map(_.confirmations).repeatWhen(_ delay 2.minute)
    .delay(1.minute)

  def watchOutputSpentRemote(data: AnchorTxData) = Insight.txs(data.address)
    .filter(tx => tx.vin.exists(_.txid == data.txId) && tx.confirmations >= 1)
    .repeatWhen(_ delay 20.minute)
}