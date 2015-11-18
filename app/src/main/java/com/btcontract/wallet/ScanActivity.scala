package com.btcontract.wallet

import R.string.{dialog_cancel, dialog_ok}
import com.dlazaro66.qrcodereaderview.QRCodeReaderView.OnQRCodeReadListener
import com.dlazaro66.qrcodereaderview.QRCodeReaderView
import eu.livotov.zxscan.util.SoundPlayer
import android.widget.Toast
import android.os.Bundle


class ScanActivity extends TimerActivity with OnQRCodeReadListener {
  lazy val reader = findViewById(R.id.reader).asInstanceOf[QRCodeReaderView]
  lazy val beepSoundPlayer = new SoundPlayer(this)
  type Points = Array[android.graphics.PointF]

  // Initialize this activity, method is run once
  override def onCreate(savedInstState: Bundle) =
  {
    super.onCreate(savedInstState)
    setContentView(R.layout.activity_scan)
    reader setOnQRCodeReadListener this
  }

  def tryParse(text: String) = try {
    beepSoundPlayer.playRawResource(R.raw.beep, false)
    lastAttempt = System.currentTimeMillis
    app.TransData setValue text
    finish
  } catch app.TransData.onFail { errCode =>
    val alert = mkChoiceDialog(reader.getCameraManager.startPreview, finish, dialog_ok, dialog_cancel)
    alert.setMessage(errCode).show setCanceledOnTouchOutside false
    Toast.makeText(app, text, Toast.LENGTH_LONG).show
    reader.getCameraManager.stopPreview
  }

  var lastAttempt = System.currentTimeMillis
  override def onQRCodeRead(text: String, points: Points) =
    if (System.currentTimeMillis - lastAttempt > 4000) tryParse(text)

  override def QRCodeNotFoundOnCamImage = { /* do nothing */ }
  override def onResume = Utils.wrap(reader.getCameraManager.startPreview)(super.onResume)
  override def onPause = Utils.wrap(reader.getCameraManager.stopPreview)(super.onPause)
  override def cameraNotFound = finish
}