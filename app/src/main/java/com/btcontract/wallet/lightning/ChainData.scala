package com.btcontract.wallet.lightning

import org.bitcoinj.core._
import rx.lang.scala.{Observable => Obs, Subscription}
import com.btcontract.wallet.helper.{RichCursor, Insight}
import com.btcontract.wallet.Utils.{Bytes, app}
import scala.concurrent.duration.DurationInt
import org.bitcoinj.core.Utils.HEX
import thundercloud.Commits
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

  def watchTxDepthRemote(c: Commitments) = Insight.txs(c.anchorAddressString)
    .filter(_.txid == c.anchorId).map(_.confirmations).repeatWhen(_ delay 2.minute)
    .delay(1.minute)

  def watchOutputSpentRemote(c: Commitments) = Insight.txs(c.anchorAddressString)
    .filter(tx => tx.vin.exists(_.txid == c.anchorId) && tx.confirmations > 0)
    .repeatWhen(_ delay 20.minute)

  // Save txHex linked to a parent txid it spends
//  def saveCommitTx(id: String, spend: Transaction) = {
//    val serializedTransaction = HEX encode spend.bitcoinSerialize
//    app.LNData.db.change(Commits.newSql, serializedTransaction, id)
//  }
//
//  def getCommitTx(id: String) = {
//    val cursor = app.LNData.db.select(Commits.selectSql, id)
//    def rc2Bytes(rc: RichCursor) = HEX decode rc.string(Commits.spendHex)
//    def bytes2Tx(rawTxData: Bytes) = new Transaction(app.params, rawTxData)
//    RichCursor(cursor).closeAfter(_.toStream.headOption map rc2Bytes map bytes2Tx)
//  }
}