package com.btcontract.wallet.lightning.thundercloud

import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import com.btcontract.wallet.lightning.LNConstants
import com.btcontract.wallet.Utils.none
import android.content.Context


trait Table {
  val id = "_id"
}

object PendingBlindTokens extends Table {
  // params is a Json of List[BlindParams], tokens is a Json of List[BigInteger] clear tokens
  val (table, params, tokens, sesPubkey, rHash) = ("pblindtokens", "params", "tokens", "seskey", "rhash")
  def newSql = s"INSERT OR IGNORE INTO $table ($params, $tokens, $sesPubkey, $rHash) VALUES (?, ?, ?, ?)"
  def selectOneSql = s"SELECT * FROM $table WHERE $rHash = ? LIMIT 1"
  def killSql(key: Long) = s"DELETE FROM $table WHERE $id = $key"

  def createSql = s"""CREATE TABLE $table ($id INTEGER PRIMARY KEY AUTOINCREMENT,
    $params TEXT NOT NULL, $tokens TEXT NOT NULL, $sesPubkey TEXT NOT NULL,
    $rHash TEXT NOT NULL)"""
}

object BlindTokens extends Table {
  val (table, token, signature, key) = ("blindtokens", "token", "signature", "key")
  def newSql = s"INSERT OR IGNORE INTO $table ($token, $signature, $key) VALUES (?, ?, ?)"
  def selectOneSql = s"SELECT * FROM $table ORDER BY $id LIMIT 1"
  def killSql(key: Long) = s"DELETE FROM $table WHERE $id = $key"

  def createSql = s"""CREATE TABLE $table ($id INTEGER PRIMARY KEY AUTOINCREMENT,
    $token TEXT NOT NULL, $signature TEXT NOT NULL, $key TEXT NOT NULL)"""
}

object EphemeralKeys extends Table {
  val (table, privKey, stamp, used) = ("ephemeralkeys", "privkey", "stamp", "used")
  def newSql = s"INSERT OR IGNORE INTO $table ($privKey, $stamp, $used) VALUES (?, ?, ?)"
  def killSql(cutoff: Long) = s"DELETE FROM $table WHERE $used = 1 AND $stamp < $cutoff"
  def selectAllSql = s"SELECT FROM $table ORDER BY $id"

  def createSql = s"""CREATE TABLE $table ($id INTEGER PRIMARY KEY AUTOINCREMENT,
    $privKey TEXT NOT NULL, $stamp INTEGER NOT NULL, $used INTEGER NOT NULL)"""
}

object Payments extends Table {
  val (waiting, paid, failed) = (1, 2, 3)
  // Waiting means we have to look outside of database (at CHANGEs and received/sent HTLCs)
  val strings = ("payments", "data", "incoming", "rhash", "r", "status", "identity", "stamp")
  val (table, data, incoming, rHash, rValue, status, identity, stamp) = strings

  def newSql(inc: Int, stp: Long) = s"""INSERT OR IGNORE INTO $table ($data, $incoming, $rHash,
    $rValue, $status, $identity, $stamp) VALUES (?, $inc, ?, ?, $waiting, ?, $stp)"""

  // Identity here refers to Thundercloud, not to be mixed with HTLC id
  def createSql = s"""CREATE TABLE $table ($id INTEGER PRIMARY KEY AUTOINCREMENT,
    $data TEXT NOT NULL, $incoming INTEGER NOT NULL, $rHash TEXT NOT NULL, $rValue TEXT NOT NULL,
    $status INTEGER NOT NULL, $identity TEXT NOT NULL UNIQUE, $stamp INTEGER NOT NULL);
    CREATE INDEX idx$identity ON $table ($identity);
    CREATE INDEX idx$rValue ON $table ($rValue);
    CREATE INDEX idx$rHash ON $table ($rHash);
    CREATE INDEX idx$stamp ON $table ($stamp);
    COMMIT"""
}

class OpenHelper(context: Context, version: Int)
extends SQLiteOpenHelper(context, "lightning.db", null, version)
{
  lazy val db = getWritableDatabase
  def txWrap(run: => Unit) = try run finally db.endTransaction
  def onUpgrade(db: SQLiteDatabase, oldVer: Int, newVer: Int) = none
  def change(sql: String, params: String*) = db.execSQL(sql, params.toArray)
  def select(sql: String, params: String*) = db.rawQuery(sql, params.toArray)

  def onCreate(dbs: SQLiteDatabase) = {
    dbs execSQL PendingBlindTokens.createSql
    dbs execSQL EphemeralKeys.createSql
    dbs execSQL BlindTokens.createSql
    dbs execSQL Payments.createSql
  }
}