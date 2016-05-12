package com.btcontract.wallet.lightning.thundercloud


trait Table {
  val id = "_id"
}

object PendingBlindTokens extends Table {
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