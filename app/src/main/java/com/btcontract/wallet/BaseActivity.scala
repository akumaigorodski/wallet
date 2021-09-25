package com.btcontract.wallet

import immortan._
import R.string._
import android.widget._
import fr.acinq.eclair._
import fr.acinq.bitcoin._
import immortan.crypto.Tools._
import com.softwaremill.quicklens._
import com.btcontract.wallet.Colors._

import java.lang.{Integer => JInt}
import scala.util.{Failure, Success}
import android.view.{View, ViewGroup}
import java.io.{File, FileOutputStream}
import android.graphics.{Bitmap, Color}
import android.content.{DialogInterface, Intent}
import android.text.{Editable, Spanned, TextWatcher}
import com.google.common.cache.{Cache, CacheBuilder}
import com.google.zxing.{BarcodeFormat, EncodeHintType}
import androidx.core.content.{ContextCompat, FileProvider}
import fr.acinq.eclair.blockchain.fee.{FeeratePerByte, FeeratePerKw}
import com.google.android.material.snackbar.{BaseTransientBottomBar, Snackbar}
import immortan.utils.{Denomination, InputParser, PaymentRequestExt, ThrottledWork}
import fr.acinq.eclair.blockchain.electrum.ElectrumEclairWallet
import com.cottacush.android.currencyedittext.CurrencyEditText
import com.google.android.material.textfield.TextInputLayout
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel
import com.btcontract.wallet.BaseActivity.StringOps
import concurrent.ExecutionContext.Implicits.global
import fr.acinq.eclair.transactions.Transactions
import androidx.appcompat.widget.AppCompatButton
import androidx.recyclerview.widget.RecyclerView
import com.google.android.material.slider.Slider
import android.graphics.Bitmap.Config.ARGB_8888
import androidx.appcompat.app.AppCompatActivity
import fr.acinq.eclair.wire.ChannelReestablish
import android.text.method.LinkMovementMethod
import fr.acinq.eclair.payment.PaymentRequest
import com.google.zxing.qrcode.QRCodeWriter
import androidx.appcompat.app.AlertDialog
import org.apmem.tools.layouts.FlowLayout
import scala.language.implicitConversions
import android.content.pm.PackageManager
import android.view.View.OnClickListener
import androidx.core.app.ActivityCompat
import java.util.concurrent.TimeUnit
import immortan.LNParams.WalletExt
import rx.lang.scala.Subscription
import scala.concurrent.Future
import android.os.Bundle
import android.net.Uri


object BaseActivity {
  implicit class StringOps(source: String) {
    def html: Spanned = android.text.Html.fromHtml(source)
    def humanFour: String = source.grouped(4).mkString(s"\u0020")

    def short: String = {
      val secondFirst = source.slice(4, 8)
      val doubleSmall = "<sup><small><small>&#8230;</small></small></sup>"
      s"${source take 4}&#160;$secondFirst&#160;$doubleSmall&#160;${source takeRight 4}"
    }
  }

  def totalBalance: MilliSatoshi = {
    val ln = Channel.totalBalance(LNParams.cm.all.values)
    ln + LNParams.chainWallets.totalBalance
  }
}

object Colors {
  val cardIn: String = "#" + WalletApp.app.getResources.getString(R.color.colorAccent).substring(3)
  val cardOut: String = "#" + WalletApp.app.getResources.getString(R.color.cardOutText).substring(3)
  val cardZero: String = "#" + WalletApp.app.getResources.getString(R.color.cardZeroText).substring(3)
  val totalZero: String = "#" + WalletApp.app.getResources.getString(R.color.totalZeroText).substring(3)
  val btcCardZero: String = "#" + WalletApp.app.getResources.getString(R.color.btcCardZeroText).substring(3)
  val lnCardZero: String = "#" + WalletApp.app.getResources.getString(R.color.lnCardZeroText).substring(3)
}

trait ExternalDataChecker {
  val noneRunnable: Runnable = new Runnable { def run: Unit = none }
  def checkExternalData(onNothing: Runnable): Unit
}

trait ChoiceReceiver {
  def onChoiceMade(tag: AnyRef, pos: Int): Unit
}

trait BaseActivity extends AppCompatActivity { me =>
  val nothingUsefulTask: Runnable = UITask(WalletApp.app quickToast error_nothing_useful)
  val timer: java.util.Timer = new java.util.Timer

  val goTo: Class[_] => Any = target => {
    this startActivity new Intent(me, target)
    InputParser.DoNotEraseRecordedValue
  }

  val exitTo: Class[_] => Any = target => {
    this startActivity new Intent(me, target)
    runAnd(InputParser.DoNotEraseRecordedValue)(finish)
  }

  override def onCreate(savedActivityState: Bundle): Unit = {
    Thread setDefaultUncaughtExceptionHandler new UncaughtHandler(me)
    super.onCreate(savedActivityState)
    INIT(savedActivityState)
  }

  override def onDestroy: Unit = {
    super.onDestroy
    timer.cancel
  }

  def INIT(state: Bundle): Unit

  // Helpers

  def browse(url: String): Unit = me startActivity new Intent(Intent.ACTION_VIEW, Uri parse url)

  def bringRateDialog(view: View): Unit = {
    val marketUri = Uri.parse(s"market://details?id=$getPackageName")
    WalletApp.app.prefs.edit.putBoolean(WalletApp.SHOW_RATE_US, false).commit
    me startActivity new Intent(Intent.ACTION_VIEW, marketUri)
    if (null != view) view.setVisibility(View.GONE)
  }

  def share(text: CharSequence): Unit = startActivity {
    val shareAction = new Intent setAction Intent.ACTION_SEND
    shareAction.setType("text/plain").putExtra(Intent.EXTRA_TEXT, text)
  }
  
  def viewRecoveryCode: Unit = {
    val content = new TitleView(me getString settings_view_revocery_phrase_ext)
    new AlertDialog.Builder(me).setView(content.asDefView).show

    for (mnemonicWord ~ mnemonicIndex <- LNParams.secret.mnemonic.zipWithIndex) {
      val item = s"<font color=$cardZero>${mnemonicIndex + 1}</font> $mnemonicWord"
      addFlowChip(content.flow, item, R.drawable.border_green)
    }
  }

  // Snackbar

  var currentSnackbar: Option[Snackbar] = Option.empty
  def removeCurrentSnack: java.util.TimerTask = UITask {
    currentSnackbar.foreach { snackBar =>
      currentSnackbar = None
      snackBar.dismiss
    }
  }

  def snack(parent: View, msg: CharSequence, actionRes: Int, fun: Snackbar => Unit): Unit = try {
    val bottomSnackbar: Snackbar = Snackbar.make(parent, msg, BaseTransientBottomBar.LENGTH_INDEFINITE)
    bottomSnackbar.getView.findViewById(com.google.android.material.R.id.snackbar_text).asInstanceOf[TextView].setMaxLines(15)

    val listener = onButtonTap {
      currentSnackbar = None
      fun(bottomSnackbar)
    }

    bottomSnackbar.setAction(actionRes, listener).show
    currentSnackbar = Some(bottomSnackbar)
  } catch none

  def cancellingSnack(parent: View, subscription: Subscription, msg: CharSequence): Unit = {
    def cancel(snack: Snackbar): Unit = runAnd(subscription.unsubscribe)(snack.dismiss)
    snack(parent, msg, dialog_cancel, cancel)
  }

  // Listener helpers

  def onButtonTap(fun: => Unit): OnClickListener = new OnClickListener {
    def onClick(view: View): Unit = fun
  }

  def onTextChange(fun: String => Unit): TextWatcher = new TextWatcher {
    override def onTextChanged(c: CharSequence, x: Int, y: Int, z: Int): Unit = fun(c.toString)
    override def beforeTextChanged(s: CharSequence, x: Int, y: Int, z: Int): Unit = none
    override def afterTextChanged(e: Editable): Unit = none
  }

  def runInFutureProcessOnUI[T](fun: => T, no: Throwable => Unit)(ok: T => Unit): Unit = runFutureProcessOnUI[T](Future(fun), no)(ok)

  def runFutureProcessOnUI[T](fun: Future[T], no: Throwable => Unit)(ok: T => Unit): Unit = fun onComplete {
    case Success(result) => UITask(ok apply result).run case Failure(error) => UITask(no apply error).run
  }

  def setVis(isVisible: Boolean, view: View): Unit = {
    val nextMode = if (isVisible) View.VISIBLE else View.GONE
    if (view.getVisibility != nextMode) view.setVisibility(nextMode)
  }

  def setVisMany(items: (Boolean, View)*): Unit =
    for (isVisible ~ view <- items) setVis(isVisible, view)

  def UITask(fun: => Any): java.util.TimerTask = {
    val runnableExec = new Runnable { override def run: Unit = fun }
    new java.util.TimerTask { def run: Unit = me runOnUiThread runnableExec }
  }

  def selectorList(listAdapter: ListAdapter): ListView = {
    val list = getLayoutInflater.inflate(R.layout.frag_selector_list, null).asInstanceOf[ListView]
    list.setAdapter(listAdapter)
    list
  }

  // Builders

  def clickableTextField(view: View): TextView = {
    val field: TextView = view.asInstanceOf[TextView]
    field setMovementMethod LinkMovementMethod.getInstance
    field
  }

  def titleBodyAsViewBuilder(title: View, body: View): AlertDialog.Builder = new AlertDialog.Builder(me).setCustomTitle(title).setView(body)
  def onFail(error: String): Unit = UITask(me showForm titleBodyAsViewBuilder(null, error.asDefView).setPositiveButton(dialog_ok, null).create).run
  def onFail(error: Throwable): Unit = onFail(error.toString)

  def getPositiveButton(alert: AlertDialog): Button = alert.getButton(DialogInterface.BUTTON_POSITIVE)
  def getNegativeButton(alert: AlertDialog): Button = alert.getButton(DialogInterface.BUTTON_NEGATIVE)
  def getNeutralButton(alert: AlertDialog): Button = alert.getButton(DialogInterface.BUTTON_NEUTRAL)

  def mkCheckForm(ok: AlertDialog => Unit, no: => Unit, bld: AlertDialog.Builder, okRes: Int, noRes: Int): AlertDialog = {
    // Create alert dialog where NEGATIVE button removes a dialog AND calls a respected provided function
    // both POSITIVE and NEGATIVE buttons may be omitted by providing -1 as their resource ids
    if (-1 != noRes) bld.setNegativeButton(noRes, null)
    if (-1 != okRes) bld.setPositiveButton(okRes, null)
    val alert = showForm(bld.create)

    val posAct = onButtonTap {
      ok(alert)
    }

    val negAct = onButtonTap {
      alert.dismiss
      no
    }

    if (-1 != noRes) getNegativeButton(alert) setOnClickListener negAct
    if (-1 != okRes) getPositiveButton(alert) setOnClickListener posAct
    alert
  }

  def mkCheckFormNeutral(ok: AlertDialog => Unit, no: => Unit, neutral: AlertDialog => Unit,
                         bld: AlertDialog.Builder, okRes: Int, noRes: Int, neutralRes: Int): AlertDialog = {

    if (-1 != neutralRes) bld.setNeutralButton(neutralRes, null)
    val alert = mkCheckForm(ok, no, bld, okRes, noRes)

    val neutralAct = onButtonTap {
      neutral(alert)
    }

    // Extend base dialog with a special NEUTRAL button, may be omitted by providing -1
    if (-1 != neutralRes) getNeutralButton(alert) setOnClickListener neutralAct
    alert
  }

  def showForm(alertDialog: AlertDialog): AlertDialog = {
    // Display dialog taking into account that host activity may not be there
    // Popup window is not cancellable on touch outside and may be width-limited

    try {
      alertDialog.show
      // First, limit popup window width in case if this is a tablet, then make sure it does not blow up if called on destroyed activity
      if (WalletApp.app.scrWidth > 2.3) alertDialog.getWindow.setLayout(WalletApp.app.maxDialog.toInt, ViewGroup.LayoutParams.WRAP_CONTENT)
      clickableTextField(alertDialog findViewById android.R.id.message)
      alertDialog.setCanceledOnTouchOutside(false)
    } catch none
    alertDialog
  }

  // Scanner

  final val scannerRequestCode = 101

  def callScanner(sheet: sheets.ScannerBottomSheet): Unit = {
    val allowed = ContextCompat.checkSelfPermission(me, android.Manifest.permission.CAMERA) == PackageManager.PERMISSION_GRANTED
    if (!allowed) ActivityCompat.requestPermissions(me, Array(android.Manifest.permission.CAMERA), scannerRequestCode)
    else sheet.show(getSupportFragmentManager, "scanner-bottom-sheet-fragment")
  }

  def addFlowChip(flow: FlowLayout, chipText: String, backgroundRes: Int, shareText: Option[String] = None): TextView =
    addFlowChip(flow, chipText, backgroundRes, defText => shareText.orElse(defText.asSome) foreach share)

  def addFlowChip(flow: FlowLayout, chipText: String, backgroundRes: Int, onTap: String => Unit): TextView = {
    val text = getLayoutInflater.inflate(R.layout.frag_chip_text, flow, false).asInstanceOf[TextView]
    text setOnClickListener onButtonTap(onTap apply text.getText.toString)
    text.setBackgroundResource(backgroundRes)
    flow.setVisibility(View.VISIBLE)
    text.setText(chipText.html)
    flow.addView(text)
    text
  }

  def showKeys(input: EditText): Unit = {
    // Popup forms can't show keyboard immediately due to animation, so delay it a bit
    def process: Unit = runAnd(input.requestFocus)(WalletApp.app showKeys input)
    timer.schedule(UITask(process), 225)
  }

  def singleInputPopup: (View, TextInputLayout, EditText) = {
    val container: View = getLayoutInflater.inflate(R.layout.frag_hint_input, null, false)
    val extraInputLayout = container.findViewById(R.id.extraInputLayout).asInstanceOf[TextInputLayout]
    val extraInput = container.findViewById(R.id.extraInput).asInstanceOf[EditText]
    (container, extraInputLayout, extraInput)
  }

  // Rich popup title

  implicit class TitleView(titleText: String) {
    val view: LinearLayout = getLayoutInflater.inflate(R.layout.frag_top_tip, null).asInstanceOf[LinearLayout]
    val flow: FlowLayout = view.findViewById(R.id.tipExtraTags).asInstanceOf[FlowLayout]
    val backArrow: ImageView = view.findViewById(R.id.backArrow).asInstanceOf[ImageView]
    val tipTitle: TextView = clickableTextField(view findViewById R.id.tipTitle)
    tipTitle.setText(titleText.html)

    def asDefView: LinearLayout = {
      view.setBackgroundColor(0x22AAAAAA)
      view
    }

    def asFlowOnly: LinearLayout = {
      view.setBackgroundColor(0x00000000)
      tipTitle.setVisibility(View.GONE)
      flow.setPadding(0, 0, 0, 0)
      view
    }

    def asColoredView(colorRes: Int): LinearLayout = {
      val resultColor = ContextCompat.getColor(me, colorRes)
      view.setBackgroundColor(resultColor)
      view
    }
  }

  // Fiat / BTC converter

  class RateManager(val content: ViewGroup, extraText: Option[String], visHintRes: Int, rates: Fiat2Btc, fiatCode: String) {
    val fiatInputAmount: CurrencyEditText = content.findViewById(R.id.fiatInputAmount).asInstanceOf[CurrencyEditText]
    val inputAmount: CurrencyEditText = content.findViewById(R.id.inputAmount).asInstanceOf[CurrencyEditText]

    val fiatInputAmountHint: TextView = content.findViewById(R.id.fiatInputAmountHint).asInstanceOf[TextView]
    val inputAmountHint: TextView = content.findViewById(R.id.inputAmountHint).asInstanceOf[TextView]

    val hintFiatDenom: TextView = clickableTextField(content findViewById R.id.hintFiatDenom)
    val hintDenom: TextView = clickableTextField(content findViewById R.id.hintDenom)

    val extraInputOption: TextView = content.findViewById(R.id.extraInputOption).asInstanceOf[TextView]
    val extraInputVisibility: TextView = content.findViewById(R.id.extraInputVisibility).asInstanceOf[TextView]
    val extraInputLayout: TextInputLayout = content.findViewById(R.id.extraInputLayout).asInstanceOf[TextInputLayout]
    val extraInput: EditText = content.findViewById(R.id.extraInput).asInstanceOf[EditText]

    val attachIdentity: CheckBox = content.findViewById(R.id.attachIdentity).asInstanceOf[CheckBox]

    def updateButton(button: Button, isEnabled: Boolean): Unit = {
      val alpha = if (isEnabled) 1F else 0.3F
      button.setEnabled(isEnabled)
      button.setAlpha(alpha)
    }

    def updateText(value: MilliSatoshi): Unit = {
      val amount = WalletApp.denom.fromMsat(value)
      inputAmount.setText(amount.toString)
      updateFiatInput
    }

    def bigDecimalFrom(input: CurrencyEditText, times: Long = 1L): BigDecimal = BigDecimal(input.getNumericValueBigDecimal) * times
    def resultMsat: MilliSatoshi = MilliSatoshi(bigDecimalFrom(inputAmount, times = WalletApp.denom.factor).toLong)
    def resultExtraInput: Option[String] = Option(extraInput.getText.toString).map(trimmed).filter(_.nonEmpty)

    def updatedFiatFromBtc: String =
      WalletApp.msatInFiat(rates, fiatCode)(resultMsat)
        .filter(0D.!=).map(_.toString)
        .getOrElse(null)

    def updatedBtcFromFiat: String =
      WalletApp.currentRate(rates, fiatCode)
        .map(perBtc => bigDecimalFrom(fiatInputAmount) / perBtc)
        .filter(0D.!=).map(Denomination.btcBigDecimal2MSat)
        .map(WalletApp.denom.fromMsat).map(_.toString)
        .getOrElse(null)

    def updateFiatInput: Unit = {
      fiatInputAmount setText updatedFiatFromBtc
      fiatInputAmount setMaxNumberOfDecimalDigits 2
    }

    def updateBtcInput: Unit = {
      inputAmount setText updatedBtcFromFiat
      inputAmount setMaxNumberOfDecimalDigits 8
    }

    extraText match {
      case Some(hintText) =>
        val revealExtraInputListener = onButtonTap {
          extraInputLayout.setVisibility(View.VISIBLE)
          extraInputOption.setVisibility(View.GONE)
          showKeys(extraInput)
        }

        extraInputLayout.setHint(hintText)
        extraInputOption.setText(hintText)
        extraInputVisibility.setText(visHintRes)
        extraInputOption.setOnClickListener(revealExtraInputListener)
        extraInputVisibility.setOnClickListener(revealExtraInputListener)

      case None =>
        extraInputOption.setVisibility(View.GONE)
        extraInputVisibility.setVisibility(View.GONE)
    }

    fiatInputAmount addTextChangedListener onTextChange { _ => if (fiatInputAmount.hasFocus) updateBtcInput }
    inputAmount addTextChangedListener onTextChange { _ => if (inputAmount.hasFocus) updateFiatInput }
    inputAmountHint setText WalletApp.denom.sign.toUpperCase
    fiatInputAmountHint setText fiatCode.toUpperCase
    fiatInputAmount setLocale Denomination.locale
    inputAmount setLocale Denomination.locale
  }

  class FeeView[T](val content: View) {
    val feeRate: TextView = content.findViewById(R.id.feeRate).asInstanceOf[TextView]
    val txIssues: TextView = content.findViewById(R.id.txIssues).asInstanceOf[TextView]
    val bitcoinFee: TextView = content.findViewById(R.id.bitcoinFee).asInstanceOf[TextView]
    val fiatFee: TextView = content.findViewById(R.id.fiatFee).asInstanceOf[TextView]

    val customFeerate: Slider = content.findViewById(R.id.customFeerate).asInstanceOf[Slider]
    val customFeerateOption: TextView = content.findViewById(R.id.customFeerateOption).asInstanceOf[TextView]
    var worker: ThrottledWork[String, T] = _
    var rate: FeeratePerKw = _

    def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = {
      feeRate setText getString(dialog_fee_sat_vbyte).format(FeeratePerByte(rate).feerate.toLong).html
      setVisMany(feeOpt.isDefined -> bitcoinFee, feeOpt.isDefined -> fiatFee, showIssue -> txIssues)

      feeOpt.foreach { fee =>
        val humanFee = WalletApp.denom.parsedWithSign(fee, cardIn, cardZero).html
        fiatFee setText WalletApp.currentMsatInFiatHuman(fee).html
        bitcoinFee setText humanFee
      }
    }

    customFeerateOption setOnClickListener onButtonTap {
      val currentFeerate = FeeratePerByte(rate).feerate.toLong
      customFeerate.setValueTo(currentFeerate * 10)
      customFeerate.setValue(currentFeerate)
      customFeerate.setValueFrom(1L)

      customFeerateOption setVisibility View.GONE
      customFeerate setVisibility View.VISIBLE
    }

    customFeerate addOnChangeListener new Slider.OnChangeListener {
      override def onValueChange(slider: Slider, value: Float, fromUser: Boolean): Unit = {
        val feeratePerByte = FeeratePerByte(value.toLong.sat)
        rate = FeeratePerKw(feeratePerByte)
        worker addWork "SLIDER-CHANGE"
      }
    }
  }

  // Guards and send/receive helpers

  def lnSendGuard(prExt: PaymentRequestExt, container: View)(onOK: Option[MilliSatoshi] => Unit): Unit = LNParams.cm.checkIfSendable(prExt.pr.paymentHash) match {
    case _ if !LNParams.cm.all.values.exists(Channel.isOperationalOrWaiting) => snack(container, getString(error_ln_no_chans).html, dialog_ok, _.dismiss)
    case _ if !LNParams.cm.all.values.exists(Channel.isOperational) => snack(container, getString(error_ln_waiting).html, dialog_ok, _.dismiss)
    case _ if !prExt.pr.features.allowPaymentSecret => snack(container, getString(error_ln_send_no_secret).html, dialog_ok, _.dismiss)

    case _ if LNParams.cm.sortedSendable(LNParams.cm.all.values).last.commits.availableForSend < LNParams.minPayment =>
      val reserve = -LNParams.cm.sortedSendable(LNParams.cm.all.values).head.commits.availableForSend
      val reserveHuman = WalletApp.denom.parsedWithSign(reserve, cardIn, cardZero)
      val msg = getString(error_ln_send_reserve).format(reserveHuman).html
      snack(container, msg, dialog_ok, _.dismiss)

    case _ if prExt.pr.amount.exists(_ < LNParams.minPayment) =>
      val requestedHuman = WalletApp.denom.parsedWithSign(prExt.pr.amount.get, cardIn, cardZero)
      val minHuman = WalletApp.denom.parsedWithSign(LNParams.minPayment, cardIn, cardZero)
      val msg = getString(error_ln_send_small).format(requestedHuman, minHuman).html
      snack(container, msg, dialog_ok, _.dismiss)

    case _ if prExt.hasSplitIssue => snack(container, getString(error_ln_send_split).html, dialog_ok, _.dismiss)
    case _ if prExt.pr.isExpired => snack(container, getString(error_ln_send_expired).html, dialog_ok, _.dismiss)
    case Some(PaymentInfo.NOT_SENDABLE_IN_FLIGHT) => snack(container, getString(error_ln_send_in_flight).html, dialog_ok, _.dismiss)
    case Some(PaymentInfo.NOT_SENDABLE_SUCCESS) => snack(container, getString(error_ln_send_done_already).html, dialog_ok, _.dismiss)
    case _ if prExt.pr.prefix != PaymentRequest.prefixes(LNParams.chainHash) => snack(container, getString(error_ln_send_network).html, dialog_ok, _.dismiss)
    case _ => onOK(prExt.pr.amount map Denomination.satCeil)
  }

  def lnReceiveGuard(container: View)(onOk: => Unit): Unit = LNParams.cm.sortedReceivable(LNParams.cm.all.values).lastOption match {
    case _ if !LNParams.cm.all.values.exists(Channel.isOperationalOrWaiting) => snack(container, getString(error_ln_no_chans).html, dialog_ok, _.dismiss)
    case _ if !LNParams.cm.all.values.exists(Channel.isOperational) => snack(container, getString(error_ln_waiting).html, dialog_ok, _.dismiss)
    case None => snack(container, getString(error_ln_receive_no_update).html, dialog_ok, _.dismiss)

    case Some(cnc) =>
      if (cnc.commits.availableForReceive < 0L.msat) {
        val reserveHuman = WalletApp.denom.parsedWithSign(-cnc.commits.availableForReceive, cardIn, cardZero)
        snack(container, getString(error_ln_receive_reserve).format(reserveHuman).html, dialog_ok, _.dismiss)
      } else onOk
  }

  abstract class OffChainSender(val maxSendable: MilliSatoshi, val minSendable: MilliSatoshi) extends HasTypicalChainFee {
    val body: android.view.ViewGroup = getLayoutInflater.inflate(R.layout.frag_input_off_chain, null).asInstanceOf[android.view.ViewGroup]
    lazy val manager = new RateManager(body, getString(dialog_add_ln_label).asSome, dialog_visibility_private, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
    val alert: AlertDialog

    val canSendFiatHuman: String = WalletApp.currentMsatInFiatHuman(maxSendable)
    val canSendHuman: String = WalletApp.denom.parsedWithSign(maxSendable, cardIn, cardZero)
    manager.hintFiatDenom.setText(getString(dialog_up_to).format(canSendFiatHuman).html)
    manager.hintDenom.setText(getString(dialog_up_to).format(canSendHuman).html)

    manager.inputAmount addTextChangedListener onTextChange { _ =>
      manager.updateButton(getNeutralButton(alert), isNeutralEnabled)
      manager.updateButton(getPositiveButton(alert), isPayEnabled)
    }

    // Load graph while user is looking at payment form
    LNParams.cm.pf process PathFinder.CMDLoadGraph

    def neutral(alert: AlertDialog): Unit
    def send(alert: AlertDialog): Unit
    def isNeutralEnabled: Boolean
    def isPayEnabled: Boolean

    def baseSendNow(prExt: PaymentRequestExt, alert: AlertDialog): Unit = {
      val cmd = LNParams.cm.makeSendCmd(prExt, manager.resultMsat, LNParams.cm.all.values.toList, typicalChainTxFee, WalletApp.capLNFeeToChain).modify(_.split.totalSum).setTo(manager.resultMsat)
      val pd = PlainDescription(split = None, label = manager.resultExtraInput, semanticOrder = None, proofTxid = None, invoiceText = prExt.descriptionOrEmpty)
      replaceOutgoingPayment(prExt, pd, action = None, sentAmount = cmd.split.myPart)
      LNParams.cm.localSend(cmd)
      alert.dismiss
    }

    def proceedSplit(prExt: PaymentRequestExt, origAmount: MilliSatoshi, alert: AlertDialog): Unit = {
      val cmd = LNParams.cm.makeSendCmd(prExt, manager.resultMsat, LNParams.cm.all.values.toList, typicalChainTxFee, WalletApp.capLNFeeToChain).modify(_.split.totalSum).setTo(origAmount)
      val pd = PlainDescription(split = cmd.split.asSome, label = manager.resultExtraInput, semanticOrder = None, proofTxid = None, invoiceText = prExt.descriptionOrEmpty)
      InputParser.value = SplitParams(prExt, action = None, pd, cmd, typicalChainTxFee)
      me goTo ClassNames.qrSplitActivityClass
      alert.dismiss
    }
  }

  abstract class OffChainReceiver(initMaxReceivable: MilliSatoshi, initMinReceivable: MilliSatoshi) {
    val CommitsAndMax(cs, maxReceivable) = LNParams.cm.maxReceivable(LNParams.cm sortedReceivable LNParams.cm.all.values).get
    val body: ViewGroup = getLayoutInflater.inflate(R.layout.frag_input_off_chain, null).asInstanceOf[ViewGroup]
    val manager: RateManager = getManager

    // It's important to cut down any msat leftover here, otherwise payment may become undeliverable
    val finalMaxReceivable: MilliSatoshi = initMaxReceivable.min(maxReceivable).truncateToSatoshi.toMilliSatoshi
    val finalMinReceivable: MilliSatoshi = initMinReceivable.min(finalMaxReceivable).max(LNParams.minPayment)
    val canReceiveHuman: String = WalletApp.denom.parsedWithSign(finalMaxReceivable, cardIn, cardZero)
    val canReceiveFiatHuman: String = WalletApp.currentMsatInFiatHuman(finalMaxReceivable)

    def receive(alert: AlertDialog): Unit = {
      val preimage: ByteVector32 = randomBytes32
      val description: PaymentDescription = getDescription
      val prExt = LNParams.cm.makePrExt(toReceive = manager.resultMsat, description, allowedChans = cs, hash = Crypto sha256 preimage)
      LNParams.cm.payBag.replaceIncomingPayment(prExt, preimage, description, BaseActivity.totalBalance, LNParams.fiatRates.info.rates)
      WalletApp.app.showStickyNotification(incoming_notify_title, incoming_notify_body, manager.resultMsat)
      processInvoice(prExt)
      alert.dismiss
    }

    val alert: AlertDialog = {
      def setMax(alert1: AlertDialog): Unit = manager.updateText(finalMaxReceivable)
      val builder = titleBodyAsViewBuilder(getTitleText.asColoredView(R.color.cardLightning), manager.content)
      mkCheckFormNeutral(receive, none, setMax, builder, dialog_ok, dialog_cancel, dialog_max)
    }

    manager.inputAmount addTextChangedListener onTextChange { _ =>
      val withinBounds = finalMinReceivable <= manager.resultMsat && finalMaxReceivable >= manager.resultMsat
      manager.updateButton(button = getPositiveButton(alert), isEnabled = withinBounds)
    }

    manager.hintFiatDenom.setText(getString(dialog_up_to).format(canReceiveFiatHuman).html)
    manager.hintDenom.setText(getString(dialog_up_to).format(canReceiveHuman).html)
    manager.updateButton(getPositiveButton(alert), isEnabled = false)

    def getTitleText: String
    def getManager: RateManager
    def getDescription: PaymentDescription
    def processInvoice(prExt: PaymentRequestExt): Unit
  }
}

trait HasTypicalChainFee {
  lazy val typicalChainTxFee: MilliSatoshi = {
    val target = LNParams.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget
    val feerate = LNParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(target)
    // Should not be used by long-lived instances since this info is getting outdated
    Transactions.weight2fee(feerate, weight = 750).toMilliSatoshi
  }

  def replaceOutgoingPayment(ext: PaymentRequestExt, description: PaymentDescription, action: Option[PaymentAction], sentAmount: MilliSatoshi, seenAt: Long = System.currentTimeMillis): Unit =
    LNParams.cm.payBag.replaceOutgoingPayment(ext, description, action, sentAmount, BaseActivity.totalBalance, LNParams.fiatRates.info.rates, typicalChainTxFee, seenAt)
}

trait ChanErrorHandlerActivity extends BaseActivity { me =>
  // Activities extending from this trait process unknown channel errors by default, also can be configured to handle other types of channel-related exceptions
  val channelErrors: Cache[ByteVector32, JInt] = CacheBuilder.newBuilder.expireAfterAccess(30, TimeUnit.SECONDS).maximumSize(500).build[ByteVector32, JInt]
  val MAX_ERROR_COUNT_WITHIN_WINDOW = 4

  def chanUnknown(worker: CommsTower.Worker, reestablish: ChannelReestablish): Unit = UITask {
    val errorCount = Option(channelErrors getIfPresent reestablish.channelId).getOrElse(0: JInt)
    if (errorCount >= MAX_ERROR_COUNT_WITHIN_WINDOW) return

    def break(alert: AlertDialog): Unit = runAnd(alert.dismiss)(worker requestRemoteForceClose reestablish.channelId)
    val msg = getString(error_channel_unknown).format(reestablish.channelId.toHex, worker.info.nodeSpecificPubKey.toString, worker.info.nodeId.toString).html
    val builder = new AlertDialog.Builder(me).setCustomTitle(getString(error_channel).asDefView).setCancelable(true).setMessage(msg)
    mkCheckFormNeutral(_.dismiss, share(msg), break, builder, dialog_ok, dialog_share, dialog_break)
    channelErrors.put(reestablish.channelId, errorCount + 1)
  }.run

  def chanError(chanId: ByteVector32, msg: String, info: RemoteNodeInfo): Unit = UITask {
    val errorCount = Option(channelErrors getIfPresent chanId).getOrElse(default = 0: JInt)
    val builder = new AlertDialog.Builder(me).setCustomTitle(getString(error_channel).asDefView).setMessage(msg.html)
    if (errorCount < MAX_ERROR_COUNT_WITHIN_WINDOW) mkCheckForm(_.dismiss, share(msg), builder, dialog_ok, dialog_share)
    channelErrors.put(chanId, errorCount + 1)
  }.run
}

trait QRActivity extends BaseActivity { me =>
  lazy val qrSize: Int = getResources.getDimensionPixelSize(R.dimen.qr_size)

  def shareData(bitmap: Bitmap, bech32: String): Unit = {
    val paymentRequestFilePath = new File(getCacheDir, "images")
    if (!paymentRequestFilePath.isFile) paymentRequestFilePath.mkdirs
    val out = new FileOutputStream(s"$paymentRequestFilePath/qr.png")
    bitmap.compress(Bitmap.CompressFormat.PNG, 85, out)
    out.close

    val savedFile = new File(paymentRequestFilePath, "qr.png")
    val fileURI = FileProvider.getUriForFile(me, "com.btcontract.wallet", savedFile)
    val share = new Intent setAction Intent.ACTION_SEND setType "text/plain" addFlags Intent.FLAG_GRANT_READ_URI_PERMISSION
    share.putExtra(Intent.EXTRA_TEXT, bech32).putExtra(Intent.EXTRA_STREAM, fileURI).setDataAndType(fileURI, getContentResolver getType fileURI)
    me startActivity Intent.createChooser(share, "Choose an app")
  }

  class QRViewHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) {
    val qrCode: ImageView = itemView.findViewById(R.id.qrCode).asInstanceOf[ImageView]
    val qrLabel: TextView = itemView.findViewById(R.id.qrLabel).asInstanceOf[TextView]
    val qrShare: AppCompatButton = itemView.findViewById(R.id.qrShare).asInstanceOf[AppCompatButton]
    val qrCopy: AppCompatButton = itemView.findViewById(R.id.qrCopy).asInstanceOf[AppCompatButton]
  }
}

object QRActivity {
  val writer = new QRCodeWriter
  val hints = new java.util.Hashtable[EncodeHintType, Any]
  hints.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.M)
  hints.put(EncodeHintType.MARGIN, 1)

  def get(data: String, size: Int): Bitmap = {
    val bitMatrix = writer.encode(data, BarcodeFormat.QR_CODE, size, size, hints)
    val (width, height) = (bitMatrix.getWidth, bitMatrix.getHeight)
    val pixels = new Array[Int](width * height)

    for {
      xPos <- 0 until width
      yPos <- 0 until height
      isBlack = bitMatrix.get(xPos, yPos)
      color = if (isBlack) Color.BLACK else Color.WHITE
    } pixels(yPos * width + xPos) = color

    val qrBitmap = Bitmap.createBitmap(width, height, ARGB_8888)
    qrBitmap.setPixels(pixels, 0, size, 0, 0, width, height)
    qrBitmap
  }
}

class ItemsWithMemory[T <: TransactionDetails] {
  private[this] var isFirstCall: Boolean = true
  var lastItems: Iterable[T] = Iterable.empty
  var idPool: Set[String] = Set.empty
  var lastDelta: Int = 0

  def setItems(items: Iterable[T] = Nil): Unit = {
    val newIdentifiers = items.map(_.identity).toSet
    val newDelta = if (isFirstCall) 0 else newIdentifiers.diff(idPool).size
    idPool ++= newIdentifiers
    lastDelta += newDelta
    isFirstCall = false
    lastItems = items
  }
}

abstract class ChainWalletCards(host: BaseActivity) { self =>
  private var cardViews: List[ChainCard] = Nil

  def init(walletExt: WalletExt): Unit = {
    cardViews = List.fill(walletExt.wallets.size)(new ChainCard)
    cardViews.map(_.view).foreach(holder.addView)
  }

  def update(walletExt: WalletExt): Unit = {
    // We have old views but updated wallet objects, redraw UI to show an updated data
    cardViews.zip(walletExt.wallets) foreach { case card ~ wallet => card updateView wallet }
  }

  class ChainCard {
    val view: View = host.getLayoutInflater.inflate(R.layout.frag_chain_card, null)
    val chainLabel: TextView = view.findViewById(R.id.chainLabel).asInstanceOf[TextView]
    val chainContainer: View = view.findViewById(R.id.chainContainer).asInstanceOf[View]
    val chainWrap: View = view.findViewById(R.id.chainWrap).asInstanceOf[View]

    val chainBalanceWrap: LinearLayout = view.findViewById(R.id.chainBalanceWrap).asInstanceOf[LinearLayout]
    val chainBalanceFiat: TextView = view.findViewById(R.id.chainBalanceFiat).asInstanceOf[TextView]
    val chainBalance: TextView = view.findViewById(R.id.chainBalance).asInstanceOf[TextView]

    val receiveBitcoinTip: ImageView = view.findViewById(R.id.receiveBitcoinTip).asInstanceOf[ImageView]
    val showMenuTip: ImageView = view.findViewById(R.id.showMenuTip).asInstanceOf[ImageView]

    def updateView(wallet: ElectrumEclairWallet): Unit = {
      def onTap: Unit = if (wallet.info.core.isRemovable) onLegacyWalletTap(wallet) else onModernWalletTap(wallet)
      val backgroundRes = if (wallet.info.core.isRemovable) R.color.cardBitcoinLegacy else R.color.cardBitcoinModern
      chainBalance.setText(WalletApp.denom.parsedWithSign(wallet.info.lastBalance.toMilliSatoshi, cardIn, btcCardZero).html)
      chainBalanceFiat.setText(WalletApp currentMsatInFiatHuman wallet.info.lastBalance.toMilliSatoshi)

      host.setVis(isVisible = !wallet.info.core.isRemovable && wallet.info.lastBalance == 0L.sat, receiveBitcoinTip)
      host.setVis(isVisible = wallet.info.core.isRemovable && wallet.info.lastBalance == 0L.sat, showMenuTip)
      host.setVis(isVisible = wallet.info.lastBalance != 0L.sat, chainBalanceWrap)

      chainWrap.setOnClickListener(host onButtonTap onTap)
      chainContainer.setBackgroundResource(backgroundRes)
      chainLabel.setText(wallet.info.label)
    }
  }

  def onLegacyWalletTap(wallet: ElectrumEclairWallet): Unit
  def onModernWalletTap(wallet: ElectrumEclairWallet): Unit
  def holder: LinearLayout
}
