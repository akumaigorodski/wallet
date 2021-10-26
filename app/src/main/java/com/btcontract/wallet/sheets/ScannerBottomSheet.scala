package com.btcontract.wallet.sheets

import android.os.Bundle
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.{ImageButton, TextView}
import androidx.appcompat.view.ContextThemeWrapper
import com.btcontract.wallet.{BaseActivity, R, WalletApp}
import com.google.android.material.bottomsheet.BottomSheetDialogFragment
import com.journeyapps.barcodescanner.{BarcodeCallback, BarcodeResult, BarcodeView}
import com.sparrowwallet.hummingbird.{ResultType, UR, URDecoder}
import immortan.crypto.Tools
import immortan.utils.InputParser


abstract class ScannerBottomSheet(host: BaseActivity, instructionOpt: Option[String] = None) extends BottomSheetDialogFragment with BarcodeCallback {
  var barcodeReader: BarcodeView = _
  var flashlight: ImageButton = _
  var instruction: TextView = _

  def pauseBarcodeReader: Unit = Tools.runAnd(barcodeReader setTorch false)(barcodeReader.pause)
  def resumeBarcodeReader: Unit = Tools.runAnd(barcodeReader decodeContinuous this)(barcodeReader.resume)

  type Points = java.util.List[com.google.zxing.ResultPoint]
  override def possibleResultPoints(points: Points): Unit = Tools.none
  override def onDestroy: Unit = Tools.runAnd(barcodeReader.stopDecoding)(super.onStop)
  override def onResume: Unit = Tools.runAnd(resumeBarcodeReader)(super.onResume)
  override def onStop: Unit = Tools.runAnd(pauseBarcodeReader)(super.onStop)

  override def onCreateView(inflater: LayoutInflater, container: ViewGroup, state: Bundle): View = {
    val contextThemeWrapper = new ContextThemeWrapper(host, R.style.AppTheme)
    val inflatorExt = inflater.cloneInContext(contextThemeWrapper)
    inflatorExt.inflate(R.layout.sheet_scanner, container, false)
  }

  override def onViewCreated(view: View, savedState: Bundle): Unit = {
    instruction = view.findViewById(R.id.instruction).asInstanceOf[TextView]
    barcodeReader = view.findViewById(R.id.reader).asInstanceOf[BarcodeView]
    flashlight = view.findViewById(R.id.flashlight).asInstanceOf[ImageButton]
    flashlight setOnClickListener host.onButtonTap(toggleTorch)

    instructionOpt foreach { instructionText =>
      host.setVis(isVisible = true, instruction)
      instruction.setText(instructionText)
    }
  }

  def toggleTorch: Unit = {
    val currentTag = flashlight.getTag.asInstanceOf[Int]

    if (currentTag != R.drawable.flashlight_on) {
      flashlight.setImageResource(R.drawable.flashlight_on)
      flashlight.setTag(R.drawable.flashlight_on)
      barcodeReader.setTorch(true)
    } else {
      flashlight.setImageResource(R.drawable.flashlight_off)
      flashlight.setTag(R.drawable.flashlight_off)
      barcodeReader.setTorch(false)
    }
  }
}

class OnceBottomSheet(host: BaseActivity, instructionOpt: Option[String], onScan: Runnable) extends ScannerBottomSheet(host, instructionOpt) {
  private[this] var lastAttempt: Long = System.currentTimeMillis

  def successfulScan(result: Any): Unit = Tools.runAnd(dismiss)(onScan.run)
  def failedScan(error: Throwable): Unit = WalletApp.app.quickToast(error.getMessage)

  override def barcodeResult(scanningResult: BarcodeResult): Unit = for {
    text <- Option(scanningResult.getText) if System.currentTimeMillis - lastAttempt > 2000
    _ = host.runInFutureProcessOnUI(InputParser.recordValue(text), failedScan)(successfulScan)
  } lastAttempt = System.currentTimeMillis
}

class URBottomSheet(host: BaseActivity, onUr: UR => Unit) extends ScannerBottomSheet(host, None) {
  private[this] val decoder = new URDecoder

  override def barcodeResult(scanningResult: BarcodeResult): Unit = {
    decoder.receivePart(scanningResult.getText)

    Option(decoder.getResult) foreach { res =>
      if (res.resultType == ResultType.SUCCESS) onUr(res.ur)
      else host.onFail(res.error)
      dismiss
    }

    val progress = decoder.getEstimatedPercentComplete
    host.setVis(isVisible = progress > 0D, instruction)
    val percent = (progress * 100).floor.toLong
    instruction.setText(s"$percent%")
  }
}
