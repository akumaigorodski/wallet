package com.btcontract.wallet.sqlite

import android.content.Context
import android.database.sqlite._
import immortan.sqlite._


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
