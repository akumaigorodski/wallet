package com.btcontract.wallet.lightning

import org.bitcoinj.core._
import rx.lang.scala.{Observable => Obs, Subscription}
import com.btcontract.wallet.helper.{Tx, RichCursor, Insight}
import scala.concurrent.duration.DurationInt
import com.btcontract.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import lncloud.Commits
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

  def watchTxDepthRemote(c: Commitments) =
    Insight.txs(app.getTo(c.anchorOutput).toString)
      .filter(_.txid == c.anchorId).map(_.confirmations)
      .repeatWhen(_ delay 2.minute)

  def watchAnchorSpentRemote(c: Commitments) = {
    // This should return revoked or unrevoked commit tx id
    def isSpend(tx: Tx) = tx.vin.exists(_.txid == c.anchorId) && tx.confirmations > 0
    Insight.txs(app.getTo(c.anchorOutput).toString).filter(isSpend).map(_.txid).repeatWhen(_ delay 20.minute)
  }

  // Punishment for revoked commit tx
  def saveTx(id: String, tx: Transaction) = {
    val punishTx = HEX encode tx.bitcoinSerialize
    app.LNData.db.change(Commits.newSql, id, punishTx)
  }

  def getTx(parentCommitTxId: String) = {
    def hex2Tx(hex: String) = new Transaction(app.params, HEX decode hex)
    val cursor = app.LNData.db.select(Commits.selectByParentTxIdSql, parentCommitTxId)
    RichCursor(cursor).closeAfter(_.toStream.headOption.map(_ string Commits.punishTx) map hex2Tx)
  }
}