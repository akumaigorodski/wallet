package immortan.sqlite

import java.lang.{Long => JLong}

import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.bitcoin.{ByteVector32, Satoshi, Transaction}
import fr.acinq.eclair.MilliSatoshi
import immortan.crypto.Tools.Fiat2Btc
import immortan.utils.ImplicitJsonFormats._
import immortan.{ChannelMaster, TxDescription, TxInfo}
import spray.json._

import scala.util.Try


case class TxSummary(fees: Satoshi, received: Satoshi, sent: Satoshi, count: Long)

class SQLiteTx(val db: DBInterface) {
  def listRecentTxs(limit: Int): RichCursor = db.select(TxTable.selectRecentSql, limit.toString)

  def listAllDescriptions: Map[String, TxDescription] =
    db.select(TxTable.selectRecentSql, 10000.toString).iterable { rc =>
      val description = to[TxDescription](rc string TxTable.description)
      (rc string TxTable.txid, description)
    }.toMap

  def addSearchableTransaction(search: String, txid: ByteVector32): Unit = {
    val newVirtualSqlPQ = db.makePreparedQuery(TxTable.newVirtualSql)
    db.change(newVirtualSqlPQ, search.toLowerCase, txid.toHex)
    newVirtualSqlPQ.close
  }

  def searchTransactions(rawSearchQuery: String): RichCursor = db.search(TxTable.searchSql, rawSearchQuery.toLowerCase)

  def updDescription(description: TxDescription, txid: ByteVector32): Unit = db txWrap {
    val updateDescriptionSqlPQ = db.makePreparedQuery(TxTable.updateDescriptionSql)
    db.change(updateDescriptionSqlPQ, description.toJson.compactPrint, txid.toHex)
    for (label <- description.label) addSearchableTransaction(label, txid)
    ChannelMaster.next(ChannelMaster.txDbStream)
    updateDescriptionSqlPQ.close
  }

  def updStatus(txid: ByteVector32, depth: Long, updatedStamp: Long, doubleSpent: Boolean): Unit = {
    db.change(TxTable.updStatusSql, depth: JLong, if (doubleSpent) 1L: JLong else 0L: JLong, updatedStamp: JLong, txid.toHex)
    ChannelMaster.next(ChannelMaster.txDbStream)
  }

  def removeByPub(xPub: ExtendedPublicKey): Unit = {
    db.change(TxTable.killByPubSql, xPub.publicKey.toString)
    ChannelMaster.next(ChannelMaster.txDbStream)
  }

  def txSummary: Try[TxSummary] =
    db.select(TxTable.selectSummarySql).headTry { rc =>
      TxSummary(fees = Satoshi(rc long 0), received = Satoshi(rc long 1), sent = Satoshi(rc long 2), count = rc long 3)
    }

  def addTx(tx: Transaction, depth: Long, received: Satoshi, sent: Satoshi, feeOpt: Option[Satoshi], xPub: ExtendedPublicKey,
            description: TxDescription, isIncoming: Long, balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc, stamp: Long): Unit = {

    val newSqlPQ = db.makePreparedQuery(TxTable.newSql)
    db.change(newSqlPQ, tx.toString, tx.txid.toHex, xPub.publicKey.toString /* WHICH WALLET IS IT FROM */, depth: JLong,
      received.toLong: JLong, sent.toLong: JLong, feeOpt.map(_.toLong: JLong).getOrElse(0L: JLong), stamp: JLong /* SEEN */, stamp: JLong /* UPDATED */,
      description.toJson.compactPrint, balanceSnap.toLong: JLong, fiatRateSnap.toJson.compactPrint, isIncoming: JLong, 0L: JLong /* NOT DOUBLE SPENT YET */)
    ChannelMaster.next(ChannelMaster.txDbStream)
    newSqlPQ.close
  }

  def toTxInfo(rc: RichCursor): TxInfo =
    TxInfo(txString = rc string TxTable.rawTx, txidString = rc string TxTable.txid, pubKeyString = rc string TxTable.pub, depth = rc long TxTable.depth,
      receivedSat = Satoshi(rc long TxTable.receivedSat), sentSat = Satoshi(rc long TxTable.sentSat), feeSat = Satoshi(rc long TxTable.feeSat), seenAt = rc long TxTable.seenAt,
      updatedAt = rc long TxTable.updatedAt, description = to[TxDescription](rc string TxTable.description), balanceSnapshot = MilliSatoshi(rc long TxTable.balanceMsat),
      fiatRatesString = rc string TxTable.fiatRates, incoming = rc long TxTable.incoming, doubleSpent = rc long TxTable.doubleSpent)
}
