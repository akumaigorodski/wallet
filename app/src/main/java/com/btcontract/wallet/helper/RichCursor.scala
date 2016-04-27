package com.btcontract.wallet.helper

import com.btcontract.wallet.Utils.runAnd
import org.bitcoinj.core.Utils.HEX
import android.database.Cursor


class RichCursor(c: Cursor)
extends Iterable[Cursor]
{
  def iterator = new Iterator[Cursor] {
    def hasNext = c.getPosition < c.getCount - 1
    def next = runAnd(res = c) { c.moveToNext }
  }

  def closeAfter[T](body: RichCursor => T) = try body(this) finally c.close
  def orm[T](body: Cursor => T) = closeAfter(_.map(body).toList)

  // Transform cursor into value and close
  def toString(default: String) = closeAfter(_ => if (c.moveToFirst) c getString 0 else default)
  def toLong(default: Long) = closeAfter(_ => if (c.moveToFirst) c getLong 0 else default)
  def toInt(default: Int) = closeAfter(_ => if (c.moveToFirst) c getInt 0 else default)

  // Take value but do not close yet
  def string(key: String) = c.getString(c getColumnIndex key)
  def long(key: String) = c.getLong(c getColumnIndex key)
  def bytes(key: String) = HEX decode string(key)
}