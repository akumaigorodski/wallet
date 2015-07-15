package com.btcontract.wallet

import android.graphics.{Rect, Canvas, Paint}
import org.bitcoinj.core.{ TransactionOutput => TxOut }


class CircleCoin(bgPaint: Paint, size: Float, text: String, value: Long, txPaint: Paint, out: TxOut)
extends AbstractCoin(bgPaint, size, text, value, txPaint, out)
{
  val rec = new Rect
  txtPaint.getTextBounds(text, 0, text.length, rec)
  private[this] val halfHeight = rec.height >> 1
  private[this] val halfWidth = rec.width >> 1

  def draw(canvas: Canvas, scale: Float) = {
    val visualVector = body.getPosition.mul(scale)
    canvas.drawCircle(visualVector.x, visualVector.y, coinSize * scale - 1, bgPaint)
    canvas.drawText(text, visualVector.x - halfWidth, visualVector.y + halfHeight, txtPaint)
  }
}

class BitCoin(bgPaint: Paint, size: Float, text: String, value: Long, txPaint: Paint, out: TxOut)
extends AbstractCoin(bgPaint, size, text, value, txPaint, out)
{
  val rec = new Rect
  txtPaint.getTextBounds(text, 0, text.length, rec)
  private[this] val logoHalfHeight = AbstractCoin.bitLogo.getHeight >> 1
  private[this] val logoHalfWidth = AbstractCoin.bitLogo.getWidth >> 1
  private[this] val halfHeight = rec.height >> 1
  private[this] val halfWidth = rec.width >> 1

  def draw(canvas: Canvas, scale: Float) = {
    val visualVector = body.getPosition.mul(scale)
    val shift = visualVector.x - logoHalfWidth - halfWidth
    canvas.drawCircle(visualVector.x, visualVector.y, coinSize * scale - 1, bgPaint)
    canvas.drawBitmap(AbstractCoin.bitLogo, shift + rec.width, visualVector.y - logoHalfHeight, txtPaint)
    canvas.drawText(text, shift, visualVector.y + halfHeight, txtPaint)
  }
}