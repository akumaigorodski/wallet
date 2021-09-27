package com.btcontract.wallettest.sqlite

import immortan.sqlite._
import android.database.sqlite._
import android.content.Context


class DBInterfaceSQLiteAndroidGraph(context: Context, name: String) extends SQLiteOpenHelper(context, name, null, 1) with DBInterfaceSQLiteAndroid {
  val base: SQLiteDatabase = getWritableDatabase

  def onCreate(dbs: SQLiteDatabase): Unit = {
    NormalChannelAnnouncementTable.createStatements.foreach(dbs.execSQL)
    HostedChannelAnnouncementTable.createStatements.foreach(dbs.execSQL)

    NormalExcludedChannelTable.createStatements.foreach(dbs.execSQL)
    HostedExcludedChannelTable.createStatements.foreach(dbs.execSQL)

    NormalChannelUpdateTable.createStatements.foreach(dbs.execSQL)
    HostedChannelUpdateTable.createStatements.foreach(dbs.execSQL)
  }

  def onUpgrade(dbs: SQLiteDatabase, v0: Int, v1: Int): Unit = {
    // Do nothing for now
  }
}
