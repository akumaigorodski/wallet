package com.btcontract.wallet

import com.google.zxing.{BarcodeFormat, EncodeHintType}
import android.widget.{TextView, Button, ImageView}
import java.io.{FileOutputStream, File}
import android.os.{Environment, Bundle}
import android.graphics.{Color, Bitmap}

import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel
import com.google.zxing.qrcode.QRCodeWriter
import android.app.AlertDialog.Builder
import android.content.Intent
import android.view.View
import android.net.Uri
import java.util

import scala.language.implicitConversions
import Bitmap.Config.ARGB_8888


object QRGen {
  val writer = new QRCodeWriter
  val hints = new util.Hashtable[EncodeHintType, Any]
  hints.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.H)
  hints.put(EncodeHintType.MARGIN, 1)

  def get(txt: String, size: Int) = {
    val bitMatrix = writer.encode(txt, BarcodeFormat.QR_CODE, size, size, hints)
    val (wid, height) = (bitMatrix.getWidth, bitMatrix.getHeight)
    val pixles = new Array[Int](wid * height)

    for (y <- 0 until height) for (x <- 0 until wid)
      pixles(y * wid + x) = bitMatrix.get(x, y) match {
        case true => Color.BLACK case false => Color.WHITE
      }

    val qrBitmap = Bitmap.createBitmap(wid, height, ARGB_8888)
    qrBitmap.setPixels(pixles, 0, wid, 0, 0, wid, height)
    qrBitmap
  }
}

class RequestActivity extends TimerActivity { me =>
  def qrError(err: Throwable): Unit = new Builder(me).setMessage(R.string.err_qr_gen).show
  lazy val qrSize = getResources getDimensionPixelSize R.dimen.bitmap_qr_size
  lazy val address = findViewById(R.id.reqAddress).asInstanceOf[TextView]
  lazy val reqShare = findViewById(R.id.reqShare).asInstanceOf[Button]
  lazy val image = findViewById(R.id.reqCode).asInstanceOf[ImageView]
  lazy val enableShare = anyToRunnable(reqShare setEnabled true)

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle)
  {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_request)

    app.TransData.value match {
      case Some(info: PayData) =>
        <(QRGen.get(info.getURI, qrSize), qrError) { bits =>
          reqShare setOnClickListener new View.OnClickListener {
            def shareQRCodeImage = <(saveImage(bits), qrError) { file =>
              val share = new Intent setAction Intent.ACTION_SEND setType "image/png"
              me startActivity share.putExtra(Intent.EXTRA_STREAM, Uri fromFile file)
            }

            def onClick(v: View) = {
              timer.schedule(enableShare, 2000)
              reqShare setEnabled false
              shareQRCodeImage
            }
          }

          address setText info.html("", "#1BA2E0")
          image setImageBitmap bits
          enableShare.run
        }
      case _ => finish
    }
  }

  def saveImage(bits: Bitmap) = {
    val path = Environment.getExternalStorageDirectory
    val dir = new File(path.getAbsolutePath + "/" + Utils.appName)
    dir.mkdirs

    // Save file
    val imageFile = new File(dir, "qr.png")
    val outStream = new FileOutputStream(imageFile)
    bits.compress(Bitmap.CompressFormat.PNG, 90, outStream)
    outStream.flush
    outStream.close
    imageFile
  }
}