package com.btcontract.wallet.lightning

import org.bitcoinj.core._
import rx.lang.scala.{Observable => Obs, Subscription}
import com.btcontract.wallet.helper.{RichCursor, Insight}
import scala.concurrent.duration.DurationInt
import com.btcontract.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import android.database.Cursor
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

  def watchOutputSpentRemote(c: Commitments) = Insight.txs(c.anchorAddressString)
    .filter(tx => tx.vin.exists(_.txid == c.anchorId) && tx.confirmations > 0)
    .repeatWhen(_ delay 20.minute)

  // Save a tx which spends a given parent txId and retrieve it from database
  def txCursor(parentId: String) = app.LNData.db.select(Commits.selectByParentTxIdSql, parentId)
  def saveTx(id: String, tx: Transaction) = app.LNData.db.change(Commits.newSql, HEX encode tx.bitcoinSerialize, id)
  def txOption(c: Cursor) = RichCursor(c).closeAfter(_.toStream.headOption.map(_ string Commits.commitSpendTx) map hex2Tx)
  def hex2Tx(txHex: String) = new Transaction(app.params, HEX decode txHex)
}