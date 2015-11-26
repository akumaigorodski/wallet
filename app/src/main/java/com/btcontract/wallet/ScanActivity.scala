package com.btcontract.wallet

import eu.livotov.zxscan.util.SoundPlayer
import android.widget.Toast
import android.os.Bundle

import R.string.{dialog_cancel, dialog_ok}
import com.journeyapps.barcodescanner._


class ScanActivity extends TimerActivity with BarcodeCallback {
  lazy val reader = findViewById(R.id.reader).asInstanceOf[BarcodeView]
  lazy val beepSoundPlayer = new SoundPlayer(this)

  // Initialize this activity, method is run once
  override def onCreate(savedInstState: Bundle) =
  {
    super.onCreate(savedInstState)
    setContentView(R.layout.activity_scan)
    reader decodeContinuous this
  }

  type Points = java.util.List[com.google.zxing.ResultPoint]
  override def possibleResultPoints(points: Points) = { /* nothing */ }
  override def onResume = Utils.wrap(super.onResume)(reader.resume)
  override def onPause = Utils.wrap(super.onPause)(reader.pause)

  def tryParse(text: String) = try {
    beepSoundPlayer.playRawResource(R.raw.beep, false)
    lastAttempt = System.currentTimeMillis
    app.TransData setValue text
    finish
  } catch app.TransData.onFail { errCode =>
    val alert = mkChoiceDialog(reader.resume, finish, dialog_ok, dialog_cancel)
    alert.setMessage(errCode).show setCanceledOnTouchOutside false
    Toast.makeText(app, text, Toast.LENGTH_LONG).show
  } finally reader.pause

  var lastAttempt = System.currentTimeMillis
  override def barcodeResult(res: BarcodeResult) = Option(res.getText) foreach {
    rawText => if (System.currentTimeMillis - lastAttempt > 4000) tryParse(rawText)
  }
}