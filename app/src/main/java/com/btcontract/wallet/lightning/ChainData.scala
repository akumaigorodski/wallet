package com.btcontract.wallet.lightning

import org.bitcoinj.core._
import rx.lang.scala.{Observable => Obs, Subscription}
import com.btcontract.wallet.helper.{RichCursor, Insight}
import com.btcontract.wallet.Utils.{Bytes, app}
import scala.concurrent.duration.DurationInt
import org.bitcoinj.core.Utils.HEX
import thundercloud.Commitments
import org.bitcoinj


object ChainData {
  def watchTxDepthLocal(watchTxId: String) = Obs.create[Int] { obs =>
    val lst = new bitcoinj.core.listeners.TransactionConfidenceEventListener {
      def onTransactionConfidenceChanged(w: bitcoinj.wallet.Wallet, tx: Transaction) =
        if (tx.getHashAsString == watchTxId) obs onNext tx.getConfidence.getDepthInBlocks
    }

    app.kit.wallet addTransactionConfidenceEventListener lst
    Subscription(app.kit.wallet removeTransactionConfidenceEventListener lst)
  }

  def watchTxDepthRemote(address: String, txId: String) = Insight.txs(address)
    .filter(_.txid == txId).map(_.confirmations).repeatWhen(_ delay 2.minute)
    .delay(1.minute)

  def watchOutputSpentRemote(address: String, txId: String) = Insight.txs(address)
    .filter(tx => tx.vin.exists(_.txid == txId) && tx.confirmations >= 1)
    .repeatWhen(_ delay 20.minute)

  // Save txHex linked to a parent txid it spends
  def saveCommitTx(id: String, spend: Transaction) = {
    val serializedTransaction = HEX encode spend.bitcoinSerialize
    app.LNData.db.change(Commitments.newSql, serializedTransaction, id)
  }

  def getCommitTx(id: String) = {
    val cursor = app.LNData.db.select(Commitments.selectSql, id)
    def bytes2Tx(rawTxData: Bytes) = new Transaction(app.params, rawTxData)
    def rc2Bytes(rc: RichCursor) = HEX.decode(rc string Commitments.spendHex)
    RichCursor(cursor).closeAfter(_.toStream.headOption map rc2Bytes map bytes2Tx)
  }
}