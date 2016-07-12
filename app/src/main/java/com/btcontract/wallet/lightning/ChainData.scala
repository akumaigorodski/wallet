package com.btcontract.wallet.lightning

import org.bitcoinj.core._
import rx.lang.scala.{Observable => Obs, Subscription}
import collection.JavaConverters.asScalaBufferConverter
import scala.concurrent.duration.DurationInt
import com.btcontract.wallet.helper.Insight
import com.btcontract.wallet.Utils.app
import org.bitcoinj


object ChainData {
  def watchTxDepthLocal(watchTxHash: String) = Obs.create[Int] { obs =>
    val lst = new bitcoinj.core.listeners.TransactionConfidenceEventListener {
      def onTransactionConfidenceChanged(w: bitcoinj.wallet.Wallet, tx: Transaction) =
        if (tx.getHashAsString == watchTxHash) obs onNext tx.getConfidence.getDepthInBlocks
    }

    app.kit.wallet addTransactionConfidenceEventListener lst
    Subscription(app.kit.wallet removeTransactionConfidenceEventListener lst)
  }

  // On each new block look 50 txs back in search for a breach
  def watchOutputSpentLocal(watchTxHash: Sha256Hash) = Obs.create[Transaction] { obs =>
    def breach(tx: Transaction) = tx.getInputs.asScala.exists(_.getOutpoint.getHash == watchTxHash)

    val lst = new com.btcontract.wallet.MyPeerDataListener {
      def onBlocksDownloaded(peer: Peer, block: Block, fBlock: FilteredBlock, left: Int) = {
        app.kit.wallet.getRecentTransactions(50, false).asScala find breach foreach obs.onNext
      }
    }

    app.kit.peerGroup addBlocksDownloadedEventListener lst
    Subscription(app.kit.peerGroup removeBlocksDownloadedEventListener lst)
  }

  def watchTxDepthRemote(data: AnchorTxData) = Insight.txs(data.address)
    .filter(_.txid == data.txId).map(_.confirmations).repeatWhen(_ delay 2.minute)
    .delay(1.minute)

  def watchOutputSpentRemote(data: AnchorTxData) = Insight.txs(data.address)
    .filter(tx => tx.vin.exists(_.txid == data.txId) && tx.confirmations >= 1)
    .repeatWhen(_ delay 20.minute)
}