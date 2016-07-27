package com.btcontract.wallet

import Utils.{wrap, app, none, runAnd}
import R.string.{dialog_cancel, dialog_ok}
import com.journeyapps.barcodescanner.BarcodeCallback
import com.journeyapps.barcodescanner.BarcodeResult
import com.journeyapps.barcodescanner.BarcodeView
import android.widget.Toast
import android.os.Bundle


class ScanActivity extends TimerActivity with BarcodeCallback { me =>
  lazy val reader = findViewById(R.id.reader).asInstanceOf[BarcodeView]
  lazy val beepSoundPlayer = new eu.livotov.zxscan.util.SoundPlayer(this)
  type Points = java.util.List[com.google.zxing.ResultPoint]
  var lastAttempt = System.currentTimeMillis

  def tryParse(text: String) = try {
    lastAttempt = System.currentTimeMillis
    beepSoundPlayer.playRawResource(R.raw.beep, false)
    app.TransData setValue text
    finish

    // Parsing error
  } catch app.TransData.onFail { err =>
    Toast.makeText(app, text, Toast.LENGTH_LONG).show
    mkForm(mkChoiceDialog(reader.resume, finish, dialog_ok,
      dialog_cancel) setMessage err, null, null)

    // Pause anyway
  } finally reader.pause

  // Only try to decode result if 3 seconds elapsed
  override def barcodeResult(res: BarcodeResult) = Option(res.getText) foreach {
    rawText => if (System.currentTimeMillis - lastAttempt > 3000) tryParse(rawText)
  }

  override def possibleResultPoints(pts: Points) = none
  override def onResume = wrap(super.onResume)(reader.resume)
  override def onPause = wrap(super.onPause)(reader.pause)

  // Initialize this activity, method is run once
  override def onCreate(savedInstState: Bundle) =
  {
    super.onCreate(savedInstState)
    setContentView(R.layout.activity_scan)
    reader decodeContinuous me
  }
}