package com.btcontract.wallet.sqlite

import immortan.sqlite._
import android.database.sqlite._
import android.content.Context


class DBInterfaceSQLiteAndroidMisc(context: Context, name: String) extends SQLiteOpenHelper(context, name, null, 2) with DBInterfaceSQLiteAndroid {
  val base: SQLiteDatabase = getWritableDatabase

  def onCreate(dbs: SQLiteDatabase): Unit = {
    TxTable.createStatements.foreach(dbs.execSQL)
    ChannelTxFeesTable.createStatements.foreach(dbs.execSQL)
    ElectrumHeadersTable.createStatements.foreach(dbs.execSQL)
    ChainWalletTable.createStatements.foreach(dbs.execSQL)
    UsedDomainsTable.createStatements.foreach(dbs.execSQL)
    LNUrlPayTable.createStatements.foreach(dbs.execSQL)
    PaymentTable.createStatements.foreach(dbs.execSQL)
    RelayTable.createStatements.foreach(dbs.execSQL)
    DataTable.createStatements.foreach(dbs.execSQL)
  }

  def onUpgrade(dbs: SQLiteDatabase, v0: Int, v1: Int): Unit = {
    if (v1 == 2) LNUrlPayTable.createStatements.foreach(dbs.execSQL) // 1 -> 2 migration creates new LNURL-PAY table
  }
}