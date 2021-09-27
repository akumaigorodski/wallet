package com.btcontract.wallettest

import com.btcontract.wallettest.Colors._
import immortan.{ChannelMaster, LNParams}
import immortan.utils.{InputParser, PaymentRequestExt}
import android.widget.{ImageView, RelativeLayout, TextView}
import com.btcontract.wallettest.BaseActivity.StringOps
import androidx.transition.TransitionManager
import fr.acinq.bitcoin.ByteVector32
import immortan.fsm.IncomingRevealed
import immortan.crypto.Tools.none
import android.os.Bundle
import android.view.View


class QRInvoiceActivity extends QRActivity with ExternalDataChecker { me =>
  lazy private[this] val activityQRInvoiceMain = findViewById(R.id.activityQRInvoiceMain).asInstanceOf[RelativeLayout]
  lazy private[this] val invoiceQrCaption = findViewById(R.id.invoiceQrCaption).asInstanceOf[TextView]
  lazy private[this] val invoiceSuccess = findViewById(R.id.invoiceSuccess).asInstanceOf[ImageView]
  lazy private[this] val qrViewHolder = new QRViewHolder(me findViewById R.id.invoiceQr)

  private var hashOfInterest: ByteVector32 = ByteVector32.Zeroes

  private val subscription = ChannelMaster.inFinalized
    .collect { case revealed: IncomingRevealed => revealed }
    .filter(_.fullTag.paymentHash == hashOfInterest)
    .subscribe(_ => markFulfilled)

  def markFulfilled: Unit = UITask {
    TransitionManager.beginDelayedTransition(activityQRInvoiceMain)
    invoiceSuccess.setVisibility(View.VISIBLE)
  }.run

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_qr_lightning_invoice)
      invoiceQrCaption.setText(getString(R.string.dialog_receive_ln).html)
      checkExternalData(noneRunnable)
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  def showInvoice(prExt: PaymentRequestExt): Unit =
    runInFutureProcessOnUI(QRActivity.get(prExt.raw.toUpperCase, qrSize), onFail) { bitmap =>
      val amountHuman = WalletApp.denom.parsedWithSign(prExt.pr.amount.get, cardIn, totalZero)
      def share: Unit = runInFutureProcessOnUI(shareData(bitmap, prExt.raw), onFail)(none)
      qrViewHolder.qrCopy setOnClickListener onButtonTap(WalletApp.app copy prExt.raw)
      qrViewHolder.qrCode setOnClickListener onButtonTap(WalletApp.app copy prExt.raw)
      qrViewHolder.qrShare setOnClickListener onButtonTap(share)
      qrViewHolder.qrLabel setText amountHuman.html
      qrViewHolder.qrCode setImageBitmap bitmap
      hashOfInterest = prExt.pr.paymentHash
    }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case paymentRequestExt: PaymentRequestExt => showInvoice(paymentRequestExt)
    case _ => finish
  }

  override def onDestroy: Unit = {
    subscription.unsubscribe
    super.onDestroy
  }
}
