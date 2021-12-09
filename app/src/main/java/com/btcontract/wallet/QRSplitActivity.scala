package com.btcontract.wallet

import android.os.Bundle
import android.widget.TextView
import com.btcontract.wallet.BaseActivity.StringOps
import com.btcontract.wallet.Colors._
import com.ornach.nobobutton.NoboButton
import immortan.crypto.Tools.none
import immortan.utils.InputParser
import immortan.{LNParams, SplitParams}


class QRSplitActivity extends QRActivity with ExternalDataChecker with HasTypicalChainFee { me =>
  lazy private[this] val splitQrCaption = findViewById(R.id.splitQrCaption).asInstanceOf[TextView]
  lazy private[this] val splitQrPay = findViewById(R.id.splitQrPay).asInstanceOf[NoboButton]
  lazy private[this] val qrViewHolder = new QRViewHolder(me findViewById R.id.splitQr)
  lazy private[this] val dialogPay = getString(R.string.dialog_ok)

  override def PROCEED(state: Bundle): Unit = {
    setContentView(R.layout.activity_qr_split_invoice)
    val splitCaption = getString(R.string.dialog_split_ln)
    splitQrCaption setText splitCaption.format(new String).html
    checkExternalData(noneRunnable)
  }

  def showSplitInvoice(sp: SplitParams): Unit = {
    val nextSplitLink = sp.prExt.withNewSplit(sp.cmd.split.myPart)
    val leftHuman = WalletApp.denom.parsedWithSign(sp.prExt.splitLeftover - sp.cmd.split.myPart, cardIn, totalZero)
    val mySplitHuman = WalletApp.denom.parsedWithSign(sp.cmd.split.myPart, cardIn, totalZero)
    splitQrPay.setText(s"$dialogPay $mySplitHuman".html)

    splitQrPay setOnClickListener onButtonTap {
      // It is assumed that many users start sending their splits at about the same time
      replaceOutgoingPayment(sp.prExt, sp.description, sp.action, sentAmount = sp.cmd.split.myPart)
      LNParams.cm.localSend(sp.cmd)
      finish
    }

    runInFutureProcessOnUI(QRActivity.get(nextSplitLink.toUpperCase, qrSize), onFail) { qrBitmap =>
      def share: Unit = runInFutureProcessOnUI(shareData(qrBitmap, nextSplitLink), onFail)(none)
      setVis(isVisible = false, qrViewHolder.qrEdit)

      qrViewHolder.qrLabel setText getString(R.string.dialog_ln_left).format(s"<br>$leftHuman").html
      qrViewHolder.qrCopy setOnClickListener onButtonTap(WalletApp.app copy nextSplitLink)
      qrViewHolder.qrCode setOnClickListener onButtonTap(WalletApp.app copy nextSplitLink)
      qrViewHolder.qrShare setOnClickListener onButtonTap(share)
      qrViewHolder.qrCode setImageBitmap qrBitmap
    }
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case splitParams: SplitParams => showSplitInvoice(splitParams)
    case _ => finish
  }
}
