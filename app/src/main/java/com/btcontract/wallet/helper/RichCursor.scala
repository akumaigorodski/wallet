package com.btcontract.wallet.helper

import android.content.{AsyncTaskLoader, CursorLoader, Context}
import android.database.{ContentObserver, Cursor}
import android.app.LoaderManager.LoaderCallbacks
import com.btcontract.wallet.Utils.runAnd
import com.btcontract.wallet.Utils.none
import org.bitcoinj.core.Utils.HEX
import android.os.Handler
import android.net.Uri


class RichCursor(c: Cursor) extends Iterable[RichCursor] { me =>
  def closeAfter[T](transform: RichCursor => T) = try transform(me) finally c.close
  def toString(default: String) = closeAfter(_ => if (c.moveToFirst) c getString 0 else default)
  def toLong(default: Long) = closeAfter(_ => if (c.moveToFirst) c getLong 0 else default)
  def toInt(default: Int) = closeAfter(_ => if (c.moveToFirst) c getInt 0 else default)
  def fillWindow = runAnd(me)(c.getCount)

  // Take value but do not close yet
  def string(key: String) = c.getString(c getColumnIndex key)
  def long(key: String) = c.getLong(c getColumnIndex key)
  def bytes(key: String) = HEX decode string(key)

  def iterator = new Iterator[RichCursor] {
    def hasNext = c.getPosition < c.getCount - 1
    def next = runAnd(me)(c.moveToNext)
  }
}

// Loading data with side effect
abstract class ReactLoader[T](ct: Context)
extends AsyncTaskLoader[Cursor](ct)
{
  def loadInBackground = getCursor match { case cursor =>
    consume(new RichCursor(cursor) map createItem)
    cursor
  }

  // Side effect consumer
  var consume: Iterable[T] => Unit
  def createItem(wrap: RichCursor): T
  def getCursor: Cursor
}

// Watch underlying data change events
abstract class ReactCallback(ct: Context)
extends LoaderCallbacks[Cursor]
{
  val observeTablePath: Uri
  def onLoaderReset(ldr: CursorLoader) = none
  def onLoadFinished(ldr: CursorLoader, cursor: Cursor) =
    cursor registerContentObserver new ContentObserver(new Handler) {
      cursor.setNotificationUri(ct.getContentResolver, observeTablePath)
      override def onChange(self: Boolean) = if (null != ldr) ldr.forceLoad
      override def deliverSelfNotifications = true
    }
}