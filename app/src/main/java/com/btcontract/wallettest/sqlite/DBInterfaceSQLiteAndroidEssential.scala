package com.btcontract.wallettest.sqlite

import immortan.sqlite._
import android.database.sqlite._
import android.content.Context


class DBInterfaceSQLiteAndroidEssential(context: Context, name: String) extends SQLiteOpenHelper(context, name, null, 1) with DBInterfaceSQLiteAndroid {
  val base: SQLiteDatabase = getWritableDatabase

  def onCreate(dbs: SQLiteDatabase): Unit = {
    ChannelTable.createStatements.foreach(dbs.execSQL)
    HtlcInfoTable.createStatements.foreach(dbs.execSQL)
    PreimageTable.createStatements.foreach(dbs.execSQL)
  }

  def onUpgrade(dbs: SQLiteDatabase, v0: Int, v1: Int): Unit = {
    // Do nothing for now
  }
}
