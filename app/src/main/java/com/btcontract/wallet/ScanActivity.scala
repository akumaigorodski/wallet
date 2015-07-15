package com.btcontract.wallet

import R.string.{dialog_cancel, err_again}
import eu.livotov.labs.android.camview.CAMView
import eu.livotov.zxscan.decoder.zxing.ZXDecoder
import eu.livotov.zxscan.util.SoundPlayer
import CAMView.CAMViewListener
import android.widget.Toast
import android.os.Bundle


class ScanActivity extends TimerActivity with CAMViewListener {
  lazy val cam = findViewById(R.id.zxScanCamera).asInstanceOf[CAMView]
  lazy val beepSoundPlayer = new SoundPlayer(this)
  lazy val qrDecoder = new ZXDecoder

  // Initialize this activity, method is run once
  override def onCreate(savedInstState: Bundle) =
  {
    super.onCreate(savedInstState)
    setContentView(R.layout.activity_scan)
    cam setCamViewListener this
    cam.start
  }

  def tryParse(data: String) = try {
    beepSoundPlayer.playRawResource(R.raw.beep, false)
    app.PaymentInformation setOutput data
    finish
  } catch app.PaymentInformation.onFail { errCode =>
    val alert = showChoiceAlert(cam.start, finish, err_again, dialog_cancel)
    alert.setMessage(errCode).show setCanceledOnTouchOutside false
    Toast.makeText(app, data, Toast.LENGTH_LONG).show
  } finally cam.stop

  // decode may return null, hence Option
  override def onDestroy = Utils.wrap(super.onDestroy)(cam.stop)
  def onPreviewData(imageBytes: Array[Byte], width: Int, height: Int) =
    Option apply qrDecoder.decode(imageBytes, width, height) foreach tryParse
}