package com.btcontract.wallet.helper

import android.content.{AsyncTaskLoader, CursorLoader, Context}
import android.database.{ContentObserver, Cursor}
import com.btcontract.wallet.Utils.{runAnd, none}
import android.app.LoaderManager.LoaderCallbacks
import java.math.BigInteger
import android.os.Handler
import android.net.Uri

case class RichCursor(c: Cursor) extends Iterable[RichCursor] { me =>
  def closeAfter[T](run: RichCursor => T) = try run(me) finally c.close
  def string(stringKey: String) = c.getString(c getColumnIndex stringKey)
  def long(longKey: String) = c.getLong(c getColumnIndex longKey)
  def bigInt(key: String) = new BigInteger(me string key)
  def fillWindow = runAnd(me)(c.getCount)

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
    consume(RichCursor(cursor) map createItem)
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