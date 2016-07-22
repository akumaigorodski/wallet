package com.btcontract.wallet.lightning.thundercloud

import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import com.btcontract.wallet.Utils.none
import android.content.Context
import android.net.Uri


object ClearTokens extends Table {
  val (table, key, token, signature, limit) = ("blindtokens", "key", "token", "signature", 10)
  def newSql = s"INSERT OR IGNORE INTO $table ($key, $token, $signature) VALUES (?, ?, ?)"
  def killSql(keyId: Long) = s"DELETE FROM $table WHERE $id = $keyId"
  def selectSql = s"SELECT * FROM $table LIMIT $limit"

  def createSql = s"""CREATE TABLE $table ($id INTEGER PRIMARY KEY AUTOINCREMENT,
    $token TEXT NOT NULL, $signature TEXT NOT NULL, $key TEXT NOT NULL)"""
}

object EphemeralKeys extends Table {
  val (table, privKey, stamp, used, limit) = ("ephemeralkeys", "privkey", "stamp", "used", 20)
  def newSql(s: Long) = s"INSERT OR IGNORE INTO $table ($privKey, $stamp, $used) VALUES (?, $s, 0)"
  def killUsedOldSql = s"DELETE FROM $table WHERE $used = 1 AND $stamp < ${86400 * 1000 * 4}"
  def markUsedSql(keyId: Long) = s"UPDATE $table SET $used = 1 WHERE $id = $keyId"
  def selectUnusedSql = s"SELECT * FROM $table WHERE $used = 0 LIMIT $limit"
  def selectAllSql = s"SELECT * FROM $table"

  def createSql = s"""CREATE TABLE $table ($id INTEGER PRIMARY KEY AUTOINCREMENT,
    $privKey TEXT NOT NULL, $stamp INTEGER NOT NULL, $used INTEGER NOT NULL)"""
}

object Payments extends Table {
  val (waiting, paid, failed) = (1, 2, 3)
  // Waiting means we have to look outside of database (at received/sent HTLCs states)
  val strings = ("payments", "data", "incoming", "rhash", "r", "status", "identity", "stamp")
  val (table, data, incoming, rHash, rValue, status, identity, stamp) = strings
  def newSql(inc: Int, stp: Long, stat: Int) = s"""INSERT OR IGNORE INTO $table
    ($data, $incoming, $rHash, $rValue, $status, $identity, $stamp)
    VALUES (?, $inc, ?, ?, $stat, ?, $stp)"""

  // Identity here refers to Thundercloud inner message id, not to be mixed with HTLC id
  def createSql = s"""CREATE TABLE $table ($id INTEGER PRIMARY KEY AUTOINCREMENT, $data TEXT NOT NULL,
    $incoming INTEGER NOT NULL, $rHash TEXT, $rValue TEXT, $status INTEGER NOT NULL, $identity TEXT NOT NULL UNIQUE,
    $stamp INTEGER NOT NULL); CREATE INDEX idx$identity ON $table ($identity); CREATE INDEX idx$rValue ON $table ($rValue);
    CREATE INDEX idx$rHash ON $table ($rHash); CREATE INDEX idx$stamp ON $table ($stamp); COMMIT"""
}

object Commits extends Table {
  val strings = ("commits", "spendtx", "txid")
  val (table, commitSpendTx, txId) = strings

  def selectByParentTxIdSql = s"SELECT * FROM $table WHERE $txId = ?"
  def newSql = s"INSERT OR IGNORE INTO $table ($commitSpendTx, $txId) VALUES (?, ?)"
  def createSql = s"""CREATE TABLE $table ($id INTEGER PRIMARY KEY AUTOINCREMENT,
    $commitSpendTx TEXT NOTNULL, $txId TEXT NOTNULL UNIQUE);
    CREATE INDEX idx$txId ON $table ($txId); COMMIT"""
}

trait Table { val id = "_id" }
class OpenHelper(context: Context, name: String, version: Int)
extends SQLiteOpenHelper(context, name, null, version)
{
  val base = getWritableDatabase
  def onUpgrade(db: SQLiteDatabase, oldVer: Int, newVer: Int) = none
  def change(sql: String, params: String*) = base.execSQL(sql, params.toArray)
  def select(sql: String, params: String*) = base.rawQuery(sql, params.toArray)
  def sqlPath(tbl: String) = Uri parse s"sqlite://com.btcontract.wallet/table/$tbl"

  def onCreate(dbs: SQLiteDatabase) = {
    dbs execSQL EphemeralKeys.createSql
    dbs execSQL ClearTokens.createSql
    dbs execSQL Payments.createSql
    dbs execSQL Commits.createSql
  }
}