package com.btcontract.wallet.lightning.thundercloud

import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import com.btcontract.wallet.Utils.none
import android.content.Context


object ClearTokens extends Table {
  val (table, key, token, signature) = ("blindtokens", "key", "token", "signature")
  def newSql = s"INSERT OR IGNORE INTO $table ($key, $token, $signature) VALUES (?, ?, ?)"
  def killSql(keyId: Long) = s"DELETE FROM $table WHERE $id = $keyId"
  def selectOneSql = s"SELECT * FROM $table LIMIT 1"

  def createSql = s"""CREATE TABLE $table ($id INTEGER PRIMARY KEY AUTOINCREMENT,
    $token TEXT NOT NULL, $signature TEXT NOT NULL, $key TEXT NOT NULL)"""
}

object EphemeralKeys extends Table {
  val (table, privKey, stamp, used) = ("ephemeralkeys", "privkey", "stamp", "used")
  def newSql = s"INSERT OR IGNORE INTO $table ($privKey, $stamp, $used) VALUES (?, ?, 0)"
  def killSql(cutoff: Long) = s"DELETE FROM $table WHERE $used = 1 AND $stamp < $cutoff"
  def selectAllSql = s"SELECT * FROM $table"

  def createSql = s"""CREATE TABLE $table ($id INTEGER PRIMARY KEY AUTOINCREMENT,
    $privKey TEXT NOT NULL, $stamp INTEGER NOT NULL, $used INTEGER NOT NULL)"""
}

object Payments extends Table {
  val (waiting, paid, failed) = (1, 2, 3)
  // Waiting means we have to look outside of database (at CHANGEs and received/sent HTLCs)
  val strings = ("payments", "data", "incoming", "rhash", "r", "status", "identity", "stamp")
  val (table, data, incoming, rHash, rValue, status, identity, stamp) = strings
  def newSql(inc: Int, stp: Long, stat: Int) = s"""INSERT OR IGNORE INTO $table
    ($data, $incoming, $rHash, $rValue, $status, $identity, $stamp)
    VALUES (?, $inc, ?, ?, $stat, ?, $stp)"""

  // Identity here refers to Thundercloud inner message id, not to be mixed with HTLC id
  def createSql = s"""CREATE TABLE $table ($id INTEGER PRIMARY KEY AUTOINCREMENT, $data TEXT NOT NULL,
    $incoming INTEGER NOT NULL, $rHash TEXT, $rValue TEXT, $status INTEGER NOT NULL,
    $identity TEXT NOT NULL UNIQUE, $stamp INTEGER NOT NULL);
    CREATE INDEX idx$identity ON $table ($identity);
    CREATE INDEX idx$rValue ON $table ($rValue);
    CREATE INDEX idx$rHash ON $table ($rHash);
    CREATE INDEX idx$stamp ON $table ($stamp);
    COMMIT"""
}

trait Table { val id = "_id" }
class OpenHelper(context: Context, name: String, version: Int)
extends SQLiteOpenHelper(context, name, null, version)
{
  lazy val db = getWritableDatabase
  def txWrap(run: => Unit) = try run finally db.endTransaction
  def onUpgrade(db: SQLiteDatabase, oldVer: Int, newVer: Int) = none
  def change(sql: String, params: String*) = db.execSQL(sql, params.toArray)
  def select(sql: String, params: String*) = db.rawQuery(sql, params.toArray)

  def onCreate(dbs: SQLiteDatabase) = {
    dbs execSQL EphemeralKeys.createSql
    dbs execSQL ClearTokens.createSql
    dbs execSQL Payments.createSql
  }
}