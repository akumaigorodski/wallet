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

  def saveTx(id: String, tx: Transaction) = {
    val punishTx = HEX encode tx.bitcoinSerialize
    app.LNData.db.change(Commits.newSql, punishTx, id)
  }

  def getTx(parentCommitTxId: String) = {
    def hex2Tx(hex: String) = new Transaction(app.params, HEX decode hex)
    val cursor = app.LNData.db.select(Commits.selectByParentTxIdSql, parentCommitTxId)
    RichCursor(cursor).closeAfter(_.toStream.headOption.map(_ string Commits.commitSpendTx) map hex2Tx)
  }
}