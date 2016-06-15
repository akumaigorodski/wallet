package com.btcontract.wallet.helper

import android.content.{AsyncTaskLoader, CursorLoader, Context}
import android.database.{ContentObserver, Cursor}
import android.app.LoaderManager.LoaderCallbacks
import com.btcontract.wallet.Utils.runAnd
import com.btcontract.wallet.Utils.none
import org.bitcoinj.core.Utils.HEX
import java.math.BigInteger
import android.os.Handler
import android.net.Uri


class RichCursor(val c: Cursor) extends Iterable[RichCursor] { me =>
  def closeAfter[T](work: RichCursor => T) = try work(me) finally c.close
  def string(key: String) = c.getString(c getColumnIndex key)
  def bigInt(key: String) = new BigInteger(me string key)
  def long(key: String) = c.getLong(c getColumnIndex key)
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