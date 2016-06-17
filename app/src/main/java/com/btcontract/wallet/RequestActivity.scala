package com.btcontract.wallet

import R.string._
import android.graphics._
import java.io.{File, FileOutputStream}
import android.os.{Environment, Bundle}
import android.widget.{Button, ImageView}
import android.text.{TextPaint, StaticLayout}
import Utils.{app, humanAddr, btcHuman, appName}
import com.google.zxing.{BarcodeFormat, EncodeHintType}
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel
import com.google.zxing.qrcode.QRCodeWriter
import android.view.View.OnClickListener
import android.app.AlertDialog.Builder
import android.content.Intent
import android.view.View
import android.net.Uri
import java.util

import android.text.Layout.Alignment.ALIGN_NORMAL
import scala.language.implicitConversions
import Bitmap.Config.ARGB_8888
import Bitmap.createBitmap


object QRGen {
  val writer = new QRCodeWriter
  val hints = new util.Hashtable[EncodeHintType, Any]
  hints.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.H)
  hints.put(EncodeHintType.MARGIN, 1)

  def get(txt: String, size: Int): Bitmap = {
    val bitMatrix = writer.encode(txt, BarcodeFormat.QR_CODE, size, size, hints)
    val (wid, height) = (bitMatrix.getWidth, bitMatrix.getHeight)
    val pixels = new Array[Int](wid * height)

    for (y <- 0 until height) for (x <- 0 until wid)
      pixels(y * wid + x) = bitMatrix.get(x, y) match {
        case true => Color.BLACK case false => Color.WHITE
      }

    val qrBitmap = createBitmap(wid, height, ARGB_8888)
    qrBitmap.setPixels(pixels, 0, wid, 0, 0, wid, height)
    qrBitmap
  }
}

object FileOps {
  def shell(name: String) = {
    val path = Environment.getExternalStorageDirectory
    val dir = new File(path.getAbsolutePath + "/" + appName)
    if (!dir.exists) dir.mkdirs
    new File(dir, name)
  }
}

class RequestActivity extends TimerActivity { me =>
  def qrError(error: Throwable): Unit = new Builder(me).setMessage(err_general).show
  private[this] lazy val reqCode = findViewById(R.id.reqCode).asInstanceOf[ImageView]
  private[this] lazy val reqShare = findViewById(R.id.reqShare).asInstanceOf[Button]
  private[this] lazy val copyData = findViewById(R.id.copyData).asInstanceOf[Button]

  private[this] lazy val textBounds = getResources getDimensionPixelSize R.dimen.bitmap_text_bounds
  private[this] lazy val bottomSize = getResources getDimensionPixelSize R.dimen.bitmap_bottom_size
  private[this] lazy val topSize = getResources getDimensionPixelSize R.dimen.bitmap_top_size
  private[this] lazy val qrSize = getResources getDimensionPixelSize R.dimen.bitmap_qr_size
  private[this] lazy val btcAddress = me getString spend_address_hint

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    setContentView(R.layout.activity_request)

    app.TransData.value match {
      case Some(pay: PayData) =>
        val bottom = humanAddr(pay.adr)
        val top = pay.tc map btcHuman getOrElse btcAddress
        showInfo(drawAll(top, bottom), pay.string, pay.string)

      // Unknown format
      case _ => finish
    }
  }

  def showInfo(finalizer: Bitmap => Bitmap, msg: String, copy: String) = {
    val onCopy = new OnClickListener { def onClick(view: View) = app setBuffer copy }
    <(QRGen.get(msg, qrSize), qrError)(finalizer andThen setView)
    copyData setOnClickListener onCopy
  }

  def setView(bitmap: Bitmap) =
    reqShare setOnClickListener new View.OnClickListener {
      def onClick(button: View) = <(me saveImage bitmap, qrError) { file =>
        val share = new Intent setAction Intent.ACTION_SEND setType "image/png"
        me startActivity share.putExtra(Intent.EXTRA_STREAM, Uri fromFile file)
      }

      reqCode setImageBitmap bitmap
      reqShare setEnabled true
    }

  def drawAll(top: String, bot: String)(qrBitmap: Bitmap) = {
    val bitmap = createBitmap(qrSize, topSize + qrSize + bottomSize, ARGB_8888)
    val ypos = topSize + qrSize + bottomSize / 2
    val canvas = new Canvas(bitmap)
    val transRect = new Rect

    canvas drawColor 0xFFEEEEEE
    transRect.set(0, topSize, qrSize, topSize + qrSize)
    canvas.drawBitmap(qrBitmap, null, transRect, null)
    drawText(canvas, top, qrSize / 2, topSize / 2)
    drawText(canvas, bot, qrSize / 2, ypos)
    bitmap
  }

  def paint = {
    val newPaint = new TextPaint(Paint.ANTI_ALIAS_FLAG)
    newPaint setTextSize getResources.getDimensionPixelSize(R.dimen.text_small)
    newPaint setTypeface Typeface.create("Droid Sans", Typeface.NORMAL)
    newPaint setTextAlign Paint.Align.CENTER
    newPaint setStyle Paint.Style.FILL
    newPaint setColor Color.BLACK
    newPaint
  }

  def drawText(canvas: Canvas, text: String, x: Float, baseY: Float) = {
    val layout = new StaticLayout(text, paint, textBounds, ALIGN_NORMAL, 1f, 0f, false)
    val y = baseY - layout.getHeight / 2f

    canvas.save
    canvas.translate(x, y)
    layout draw canvas
    canvas.restore
  }

  def saveImage(bits: Bitmap) = {
    val imageFile = FileOps shell "qr.png"
    val stream = new FileOutputStream(imageFile)
    bits.compress(Bitmap.CompressFormat.PNG, 80, stream)
    stream.flush
    stream.close
    imageFile
  }
}