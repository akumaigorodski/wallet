package com.btcontract.wallet

import Utils._
import R.string._
import android.text._
import android.view._
import android.widget._
import org.bitcoinj.core._
import org.bitcoinj.core.listeners._

import android.widget.RadioGroup.OnCheckedChangeListener
import info.hoang8f.android.segmented.SegmentedGroup
import concurrent.ExecutionContext.Implicits.global
import android.view.inputmethod.InputMethodManager
import org.bitcoinj.crypto.KeyCrypterException
import android.view.View.OnClickListener
import org.bitcoinj.store.SPVBlockStore
import android.app.AlertDialog.Builder
import android.util.DisplayMetrics
import org.bitcoinj.uri.BitcoinURI
import scala.collection.mutable
import scala.concurrent.Future
import android.net.Uri

import org.bitcoinj.wallet.{SendRequest, Wallet}
import org.bitcoinj.wallet.Wallet.{ExceededMaxTransactionSize, CouldNotAdjustDownwards}
import org.bitcoinj.wallet.listeners.{WalletChangeEventListener, WalletCoinsReceivedEventListener, WalletCoinsSentEventListener}

import R.id.{amtInSat, amtInBtc, amtInBit, typeUSD, typeEUR, typeCNY}
import com.btcontract.wallet.helper.{RandomGenerator, Fee, FiatRates}
import android.text.method.{LinkMovementMethod, DigitsKeyListener}
import android.content.{DialogInterface, Context, Intent}
import java.text.{DecimalFormatSymbols, DecimalFormat}
import java.util.{Locale, Timer, TimerTask}
import scala.util.{Failure, Success, Try}
import android.app.{Dialog, Activity}

import DialogInterface.BUTTON_POSITIVE
import ViewGroup.LayoutParams.WRAP_CONTENT
import InputMethodManager.HIDE_NOT_ALWAYS
import Transaction.MIN_NONDUST_OUTPUT
import Context.INPUT_METHOD_SERVICE


object Utils { me =>
  type Bytes = Array[Byte]
  type TryCoin = Try[Coin]
  type Rates = Map[String, Double]
  type Pays = mutable.Buffer[PayData]
  type Outputs = mutable.Buffer[TransactionOutput]

  // Cannot have lazy var so use this construct
  var startupAppReference: WalletApp = null
  lazy val app = startupAppReference

  val passType = InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_PASSWORD
  val textType = InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD

  val Seq(strDollar, strEuro, strYuan) = List("dollar", "euro", "yuan")
  val fiatMap = Map(typeUSD -> strDollar, typeEUR -> strEuro, typeCNY -> strYuan)
  val revFiatMap = Map(strDollar -> typeUSD, strEuro -> typeEUR, strYuan -> typeCNY)

  val appName = "Bitcoin"
  val nullFail = Failure(null)
  val rand = new RandomGenerator
  lazy val sumIn = app getString txs_sum_in
  lazy val sumOut = app getString txs_sum_out

  // Various denom format rules
  val locale = new Locale("en", "US")
  val baseFiat = new DecimalFormat("#.##")
  val baseBtc = new DecimalFormat("#.########")
  val baseSat = new DecimalFormat("###,###,###")
  val baseBit = new DecimalFormat("#,###,###.##")
  val symbols = new DecimalFormatSymbols(locale)

  baseFiat setDecimalFormatSymbols symbols
  baseBit setDecimalFormatSymbols symbols
  baseSat setDecimalFormatSymbols symbols
  baseBtc setDecimalFormatSymbols symbols

  def sat(coin: Coin) = baseSat format coin.value
  def bit(coin: Coin) = baseBit format BigDecimal(coin.value) / 100
  def btc(coin: Coin) = baseBtc format BigDecimal(coin.value) / 100000000

  // App wide utility functions
  def btcHuman(coin: Coin) = app getString input_alt_btc format btc(coin)
  def wrap(run: => Unit)(go: => Unit) = try go catch none finally run
  def humanAddr(adr: Address) = s"$adr" grouped 4 mkString "\u0020"
  def runAnd[T](result: T)(action: => Any) = { action; result }
  def none: PartialFunction[Any, Unit] = { case _ => }

  // Fiat rates related functions, all transform a Try monad
  def currentFiatName = app.prefs.getString(AbstractKit.CURRENCY, "dollar")
  def currentRate = for (rates <- FiatRates.rates) yield rates(currentFiatName)

  // Iff we have rates and amount then fiat price
  def inFiat(tc: TryCoin) = currentRate flatMap { rt =>
    for (coin <- tc) yield coin.getValue * rt / 100000000
  }

  def fiatSign(amt: Double) = baseFiat format amt match {
    case amount if currentFiatName == strYuan => s"$amount CNY"
    case amount if currentFiatName == strEuro => s"$amount €"
    case amount => s"&#36;$amount"
  }
}

// Info stack manager
abstract class InfoActivity extends AnimatorActivity { me =>
  val tracker = new WalletChangeEventListener with WalletCoinsReceivedEventListener with WalletCoinsSentEventListener with TransactionConfidenceEventListener {
    def onCoinsReceived(w: Wallet, tx: Transaction, pb: Coin, nb: Coin) = if (nb isGreaterThan pb) anim(me getString tx_received format btc(nb subtract pb), Informer.RECEIVED)
    def onTransactionConfidenceChanged(w: Wallet, tx: Transaction) = if (tx.getConfidence.getDepthInBlocks == 1) anim(getString(tx_1st_conf), Informer.TXCONFIRMED)
    def onCoinsSent(w: Wallet, tx: Transaction, pb: Coin, nb: Coin) = anim(me getString tx_sent format btc(pb subtract nb), Informer.DECSEND)
    def onWalletChanged(w: Wallet) = none
  }

  // Peers listeners
  class CatchTracker extends MyPeerDataListener {
    def onBlocksDownloaded(peer: Peer, block: Block, fBlock: FilteredBlock, left: Int) = {
      app.kit.peerGroup addBlocksDownloadedEventListener new NextTracker(blocksNumberLeftOnStart = left)
      app.kit.peerGroup removeBlocksDownloadedEventListener this
    }
  }

  class NextTracker(blocksNumberLeftOnStart: Int) extends MyPeerDataListener {
    def onBlocksDownloaded(peer: Peer, block: Block, fBlock: FilteredBlock, left: Int) = {
      if (blocksNumberLeftOnStart > 144) update(howManyBlocksLeftInPlainText format left, Informer.SYNC)
      if (left < 1) add(getString(info_progress_done), Informer.SYNC).timer.schedule(me del Informer.SYNC, 5000)
      if (left < 1) app.kit.peerGroup removeBlocksDownloadedEventListener this
      if (left < 1) app.kit.wallet saveToFile app.walletFile
      runOnUiThread(ui)
    }

    val howManyBlocksLeftInPlainText = me getString info_progress
    val howManyBlocksLeftOnStart = howManyBlocksLeftInPlainText format blocksNumberLeftOnStart
    if (blocksNumberLeftOnStart > 144) add(howManyBlocksLeftOnStart, Informer.SYNC)
  }

  val constListener = new PeerConnectedEventListener with PeerDisconnectedEventListener {
    def onPeerDisconnected(p: Peer, pc: Int) = me runOnUiThread update(mkTxt, Informer.PEERS).ui
    def onPeerConnected(p: Peer, pc: Int) = me runOnUiThread update(mkTxt, Informer.PEERS).ui
    def mkTxt = app.plurOrZero(peersInfoOpts, app.kit.peerGroup.numConnectedPeers)
  }

  lazy val peersInfoOpts = getResources getStringArray R.array.info_peers
  lazy val requestOpts = getResources getStringArray R.array.dialog_request

  // Activity lifecycle listeners management
  override def onOptionsItemSelected(m: MenuItem) = runAnd(true) {
    if (m.getItemId == R.id.actionRequestPayment) mkRequestForm
    else if (m.getItemId == R.id.actionSettings) mkSetsForm
    else if (m.getItemId == R.id.actionBuyCoins) {

      val payTo = app.kit.currentAddress.toString
      val msg = Html fromHtml getString(buy_info).format(payTo)
      mkForm(negBld(dialog_cancel), me getString action_buy, msg)
    }
  }

  override def onCreateOptionsMenu(menu: Menu) = runAnd(true) {
    getMenuInflater.inflate(R.menu.transactions_ops, menu)
  }

  // Top bar reactions
  def goQRScan(top: View) = me goTo classOf[ScanActivity]
  def goLNWallet(top: View) = me goTo classOf[LNTxsActivity]

  def doReceive(top: View) = {
    val payData = PayData(app.kit.currentAddress, nullFail)
    app.TransData.value = Option apply payData
    me goTo classOf[RequestActivity]
  }

  def doPay(top: View): SpendManager = {
    val content = getLayoutInflater.inflate(R.layout.frag_input_spend, null, false)
    val address = content.findViewById(R.id.addressData).asInstanceOf[EditText]
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), null, content)
    val man = new RateManager(content)

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener new OnClickListener {
      def onClick(proceed: View) = man.result match {
        case Failure(seemsAmountIsEmpty) => toast(dialog_sum_empty)
        case Success(cn) if Try(btcAddr).isFailure => toast(dialog_addr_wrong)
        case Success(cn) if cn isLessThan MIN_NONDUST_OUTPUT => toast(dialog_sum_dusty)
        case _ => rm(alert)(next.showForm)
      }
    }

    def next = new CompletePay(me) {
      def errorAction = doPay(top) set pay
      val pay = PayData(btcAddr, man.result)
      val title = pay pretty sumOut

      def confirm = {
        add(me getString tx_announce, Informer.DECSEND).ui.run
        <(announceTransaction, errorReact)(none)
      }
    }

    def btcAddr = app getTo address.getText.toString
    new SpendManager(address, man)
  }

  def mkRequestForm = {
    val requestText = me getString action_request_payment
    val content = getLayoutInflater.inflate(R.layout.frag_input_receive, null)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), requestText, content)
    val man = new RateManager(content)

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener new OnClickListener {
      def onClick(posButtonView: View) = rm(alert) {
        val pay = PayData(app.kit.currentAddress, man.result)
        val listCon = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
        val adapter = new ArrayAdapter(me, R.layout.frag_center_text, R.id.textItem, requestOpts)
        val dialog = mkForm(negBld(dialog_cancel), Html fromHtml pay.pretty(sumIn), listCon)

        listCon setOnItemClickListener onTap { position =>
          def next = choose(position, pay.string)
          rm(dialog)(next)
        }

        app.TransData.value = Option(pay)
        listCon setAdapter adapter
      }
    }
  }

  def mkSetsForm: Unit = {
    val form = getLayoutInflater.inflate(R.layout.frag_settings, null)
    val dialog = mkForm(me negBld dialog_back, Html fromHtml getString(read_settings), form)
    val rescanWallet = form.findViewById(R.id.rescanWallet).asInstanceOf[Button]
    val viewMnemonic = form.findViewById(R.id.viewMnemonic).asInstanceOf[Button]
    val changePass = form.findViewById(R.id.changePass).asInstanceOf[Button]

    rescanWallet setOnClickListener new OnClickListener {
      def onClick(restoreWalletView: View) = rm(dialog)(openForm)

      def openForm = checkPass { pass =>
        val dialog = mkChoiceDialog(go, mkSetsForm, dialog_ok, dialog_back)
        mkForm(dialog setMessage sets_rescan_ok, null, null)
      }

      def go = try {
        app.chainFile.delete
        app.kit.wallet.reset
        app.kit.store = new SPVBlockStore(app.params, app.chainFile)
        app.kit useCheckPoints app.kit.wallet.getEarliestKeyCreationTime
        app.kit.wallet saveToFile app.walletFile
      } catch none finally System exit 0
    }

    changePass setOnClickListener new OnClickListener {
      def onClick(changePassView: View) = rm(dialog)(openForm)

      def openForm = checkPass { oldPass =>
        shortCheck(password_new, password_too_short) { newPass =>
          <(rotatePassword, _ => System exit 0)(_ => me toast sets_password_ok)
          add(app getString pass_changing, Informer.CODECHECK).ui.run
          timer.schedule(me del Informer.CODECHECK, 5000)

          def rotatePassword = {
            app.kit.wallet decrypt oldPass
            app.kit encryptWallet newPass
          }
        }
      }
    }

    viewMnemonic setOnClickListener new OnClickListener {
      def onClick(viewMnemonicView: View) = rm(dialog)(openForm)
      val warn = me getString sets_noscreen

      def openForm: Unit = passPlus(mkSetsForm) { password =>
        def mk(txt: String) = new Builder(me) setCustomTitle warn setMessage txt
        <(Mnemonic decrypt password, _ => wrong)(seed => mk(Mnemonic text seed).show)
      }
    }

    // Check wallet password and inform if wrong
    def wrong = wrap(me toast password_wrong)(mkSetsForm)
    def checkPass(next: String => Unit) = passPlus(mkSetsForm) { txt =>
      <(app.kit.wallet checkPassword txt, _ => wrong)(if (_) next(txt) else wrong)
    }

    // Check password length before proceeding
    def shortCheck(txtRes: Int, short: Int)(next: String => Unit) = {
      val (passwordAsk, secret) = generatePasswordPromptView(textType, txtRes)
      def check = if (secret.getText.length >= 6) next(secret.getText.toString) else toast(short)
      mkForm(mkChoiceDialog(check, mkSetsForm, dialog_ok, dialog_back), null, passwordAsk)
    }
  }
}

abstract class AnimatorActivity extends TimerActivity {
  lazy val ui = anyToRunnable(getActionBar setSubtitle infos.head.value)
  private[this] var currentAnimation = Option.empty[TimerTask]
  private[this] var infos = List.empty[Informer]

  def anim(text: String, infoType: Int) = {
    new Anim(app.kit.currentBalance, getActionBar.getTitle.toString)
    add(text, infoType).timer.schedule(this del infoType, 25000)
    runOnUiThread(ui)
  }

  // Informer CRUD
  def del(delTag: Int) = uiTask {
    infos = infos.filterNot(_.tag == delTag)
    ui
  }

  def add(text: String, addTag: Int) = {
    infos = new Informer(text, addTag) :: infos
    this
  }

  def update(text: String, tag: Int) = {
    for (inf <- infos if inf.tag == tag) inf.value = text
    this
  }

  // Title text animation
  class Anim(amt: Coin, curText: String) extends Runnable {
    val txt = if (amt.isZero) getString(wallet_empty) else btc(amt)
    val max = scala.math.max(txt.length, curText.length)
    var index = 1

    override def run = {
      getActionBar setTitle s"${txt take index}${curText drop index}".trim
      if (index < max) index += 1 else for (an <- currentAnimation) an.cancel
    }

    for (an <- currentAnimation) an.cancel
    currentAnimation = Some apply uiTask(this)
    timer.schedule(currentAnimation.get, 0, 125)
  }

  // Password checking popup
  def passPlus(back: => Unit)(next: String => Unit) = {
    val (passAsk, secret) = generatePasswordPromptView(passType, password_old)
    mkForm(mkChoiceDialog(infoAndNext, back, dialog_next, dialog_back), null, passAsk)

    def infoAndNext = {
      add(app getString pass_checking, Informer.CODECHECK).ui.run
      timer.schedule(this del Informer.CODECHECK, 2500)
      next(secret.getText.toString)
    }
  }
}

abstract class TimerActivity extends Activity { me =>
  val goTo: Class[_] => Unit = me startActivity new Intent(me, _)
  val exitTo: Class[_] => Unit = goto => wrap(finish)(goTo apply goto)
  val timer = new Timer

  // Screen size in inches and prefs reference
  lazy val maxDialog = metrics.densityDpi * 2.1
  lazy val scrWidth = metrics.widthPixels.toDouble / metrics.densityDpi
  lazy val scrHeight = metrics.heightPixels.toDouble / metrics.densityDpi
  lazy val metrics = new DisplayMetrics match { case metrix =>
    getWindowManager.getDefaultDisplay getMetrics metrix
    metrix
  }

  // Timer utilities and toast
  override def onDestroy = wrap(super.onDestroy)(timer.cancel)
  implicit def anyToRunnable(process: => Unit): Runnable = new Runnable { def run = process }
  implicit def uiTask(process: => Runnable): TimerTask = new TimerTask { def run = me runOnUiThread process }
  def toast(message: Int) = Toast.makeText(app, message, Toast.LENGTH_LONG).show

  // Run computation in Future, deal with results on UI thread
  def <[T](fun: => T, no: Throwable => Unit)(ok: T => Unit) = <<(Future(fun), no)(ok)
  def <<[T](future: Future[T], no: Throwable => Unit)(ok: T => Unit) = future onComplete {
    case Success(rs) => runOnUiThread(ok apply rs) case Failure(ex) => runOnUiThread(no apply ex)
  }

  // Option dialog popup
  def choose(pos: Int, txt: String) =
    if (pos == 0) me goTo classOf[RequestActivity]
    else if (pos == 1) app setBuffer txt
    else share(txt)

  def share(text: String) = startActivity {
    val sendIntent = new Intent setType "text/plain"
    sendIntent.putExtra(Intent.EXTRA_TEXT, text)
    sendIntent.setAction(Intent.ACTION_SEND)
  }

  // Basis for dialog forms
  implicit def str2View(res: CharSequence): LinearLayout = {
    val view = getLayoutInflater.inflate(R.layout.frag_top_tip, null).asInstanceOf[LinearLayout]
    val textField = view.findViewById(R.id.actionTip).asInstanceOf[TextView]
    textField setMovementMethod LinkMovementMethod.getInstance
    textField setText res
    view
  }

  def rm(prev: Dialog)(fun: => Unit) = {
    timer.schedule(me anyToRunnable fun, 120)
    prev.dismiss
  }

  def generatePasswordPromptView(inpType: Int, txt: Int) = {
    val passAsk = getLayoutInflater.inflate(R.layout.frag_changer, null).asInstanceOf[LinearLayout]
    val secretInputField = passAsk.findViewById(R.id.secretInput).asInstanceOf[EditText]
    passAsk.findViewById(R.id.secretTip).asInstanceOf[TextView] setText txt
    secretInputField setInputType inpType
    (passAsk, secretInputField)
  }

  def negBld(neg: Int) = new Builder(me).setNegativeButton(neg, null)
  def negPosBld(neg: Int, pos: Int) = negBld(neg).setPositiveButton(pos, null)

  def mkForm(builder: Builder, title: View, content: View) = {
    val alertDialog = builder.setCustomTitle(title).setView(content).show
    if (scrWidth > 2.3) alertDialog.getWindow.setLayout(maxDialog.toInt, WRAP_CONTENT)
    alertDialog setCanceledOnTouchOutside false
    alertDialog
  }

  def mkChoiceDialog(ok: => Unit, no: => Unit, okRes: Int, noRes: Int) = {
    val cancel = new DialogInterface.OnClickListener { def onClick(x: DialogInterface, w: Int) = no }
    val again = new DialogInterface.OnClickListener { def onClick(x: DialogInterface, w: Int) = ok }
    new Builder(me).setPositiveButton(okRes, again).setNegativeButton(noRes, cancel)
  }

  def hideKeys(run: => Unit) = try {
    timer.schedule(me anyToRunnable run, 50)
    val mgr = getSystemService(INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
    mgr.hideSoftInputFromWindow(getCurrentFocus.getWindowToken, HIDE_NOT_ALWAYS)
  } catch none

  // Wrapper for a ListView onClick listener
  def onTap(go: Int => Unit) = new AdapterView.OnItemClickListener {
    def onItemClick(p: AdapterView[_], view: View, pos: Int, id: Long) = go(pos)
  }
}

class Spinner(tv: TextView) extends Runnable {
  override def run = tv.getText match { case text =>
    if (text.length > 8) tv setText "★" else tv setText s"$text★"
  }
}

class RateManager(content: View) { me =>
  val bitInput = content.findViewById(R.id.inputAmount).asInstanceOf[EditText]
  val fiatInput = content.findViewById(R.id.fiatInputAmount).asInstanceOf[EditText]
  val fiatType = content.findViewById(R.id.fiatType).asInstanceOf[SegmentedGroup]
  val bitType = content.findViewById(R.id.bitType).asInstanceOf[SegmentedGroup]

  val memoMap = Map(amtInBtc -> "btc", amtInBit -> "bit", amtInSat -> "sat")
  val memoRevMap = Map("btc" -> amtInBtc, "bit" -> amtInBit, "sat" -> amtInSat)
  val inputMap: Map[Int, Coin => String] = Map(amtInBtc -> btc, amtInBit -> bit, amtInSat -> sat)
  val hintMap = Map(amtInBtc -> input_hint_btc, amtInBit -> input_hint_bit, amtInSat -> input_hint_sat)
  val charMap = Map(amtInBtc -> ".0123456789", amtInBit -> ".,0123456789", amtInSat -> ",0123456789")
  def setSum(tc: TryCoin) = tc map inputMap(bitType.getCheckedRadioButtonId) map bitInput.setText
  def memoMode = memoRevMap apply app.prefs.getString(AbstractKit.BTC_DENOMINATION, "bit")
  def result = Try apply norm(bitType.getCheckedRadioButtonId)

  def norm(state: Int) = bitInput.getText.toString.replace(",", "") match {
    case raw if amtInBit == state => Coin valueOf (raw.toDouble * 100).toLong
    case raw if amtInBtc == state => Coin parseCoin raw
    case raw => Coin valueOf raw.toLong
  }

  val bitListener = new TextChangedWatcher {
    def upd = fiatInput.setText(inFiat(result) map baseFiat.format getOrElse null)
    def onTextChanged(s: CharSequence, st: Int, b: Int, c: Int) = if (bitInput.hasFocus) upd
    // Should have focus because may be filled automatically from QR or link
    bitInput.requestFocus
  }

  val fiatListener = new TextChangedWatcher {
    def upd = me setSum inBtc.map(Coin valueOf _.toLong) getOrElse bitInput.setText(null)
    def inBtc = currentRate.map(fiatInput.getText.toString.replace(",", "").toDouble / _ * 100000000)
    def onTextChanged(s: CharSequence, st: Int, b: Int, c: Int) = if (fiatInput.hasFocus) upd
  }

  bitType setOnCheckedChangeListener new OnCheckedChangeListener {
    def onCheckedChanged(radioGroup: RadioGroup, checkedButton: Int) = {
      bitInput setKeyListener DigitsKeyListener.getInstance(charMap apply bitType.getCheckedRadioButtonId)
      bitInput.setText(Try apply norm(memoMode) map inputMap(bitType.getCheckedRadioButtonId) getOrElse null)
      app.prefs.edit.putString(AbstractKit.BTC_DENOMINATION, memoMap apply bitType.getCheckedRadioButtonId).commit
      bitInput setHint hintMap(bitType.getCheckedRadioButtonId)
    }
  }

  fiatType setOnCheckedChangeListener new OnCheckedChangeListener {
    def onCheckedChanged(radioGroupView: RadioGroup, newFiatName: Int) = {
      app.prefs.edit.putString(AbstractKit.CURRENCY, fiatMap apply newFiatName).commit
      if (fiatInput.hasFocus) fiatListener.upd else bitListener.upd
      fiatInput setHint currentFiatName
    }
  }

  fiatInput addTextChangedListener fiatListener
  bitInput addTextChangedListener bitListener
  fiatType check revFiatMap(currentFiatName)
  bitType check memoMode
}

class SpendManager(address: EditText, man: RateManager) { me =>
  def setAddressValue(adr: Address) = address setText adr.toString

  def set(uri: BitcoinURI) = {
    me setAddressValue uri.getAddress
    man setSum Try(uri.getAmount)
  }

  def set(data: PayData) = {
    me setAddressValue data.adr
    man setSum data.tc
  }
}

case class PayData(adr: Address, tc: TryCoin) {
  def route(sumDirection: String) = sumDirection format humanAddr(adr)
  def tryUri = for (cn <- tc) yield BitcoinURI.convertToBitcoinURI(adr, cn, null, null)
  def string = tryUri getOrElse adr.toString

  def pretty(way: String) = {
    val bitcoin = tc.map(cn => s"${this route way}<br><br>${Utils btcHuman cn}") getOrElse route(way)
    val fiat = inFiat(tc).map(amt => s"<br><font color=#999999>≈ ${Utils fiatSign amt}</font>")
    bitcoin + fiat.getOrElse(new String)
  }
}

abstract class CompletePay(host: AnimatorActivity) {
  val (passAsk, secretField) = host.generatePasswordPromptView(passType, wallet_password)
  val form = host.getLayoutInflater.inflate(R.layout.frag_input_spend_confirm, null)
  val choiceList = form.findViewById(R.id.choiceList).asInstanceOf[ListView]
  val dialog = host.mkChoiceDialog(confirm, none, dialog_pay, dialog_cancel)

  // Wrap fee rates with human readable text
  val feeDefault = host getString fee_default format sumOut.format(Utils btc Fee.default)
  val feeLive = host getString fee_live format sumOut.format(Utils btc Fee.rate)
  val infos = feeLive :: feeDefault :: Nil map Html.fromHtml
  val slot = android.R.layout.select_dialog_singlechoice

  val pay: PayData
  val title: String
  def errorAction
  def confirm

  def showForm = {
    form.asInstanceOf[LinearLayout].addView(passAsk, 0)
    host.mkForm(dialog, host.str2View(Html fromHtml title), form)
    choiceList setAdapter new ArrayAdapter(host, slot, infos.toArray)
    choiceList.setItemChecked(0, true)
  }

  def announceTransaction = {
    val all = app.kit.currentBalance subtract Fee.default isLessThan pay.tc.get
    val request = if (all) SendRequest emptyWallet pay.adr else SendRequest.to(pay.adr, pay.tc.get)
    request.feePerKb = if (choiceList.getCheckedItemPosition == 0) Fee.rate else Fee.default
    request.aesKey = app.kit.wallet.getKeyCrypter deriveKey secretField.getText.toString

    app.kit.wallet completeTx request
    // Block until at least one peer confirms it got our request
    app.kit.peerGroup.broadcastTransaction(request.tx, 1).broadcast.get
  }

  def errorReact(exc: Throwable): Unit = exc match {
    case _: InsufficientMoneyException => onError(app getString err_low_funds format pay.tc.get)
    case _: ExceededMaxTransactionSize => onError(app getString err_transaction_too_large)
    case _: CouldNotAdjustDownwards => onError(app getString err_empty_shrunk)
    case _: KeyCrypterException => onError(app getString err_pass)
    case _: Throwable => onError(app getString err_general)
  }

  def onError(errorMessage: String) = try {
    val info = host.mkChoiceDialog(errorAction, none, dialog_ok, dialog_cancel)
    host.mkForm(info setMessage errorMessage, null, null)
    host.del(Informer.DECSEND).run
  } catch none
}

abstract class TextChangedWatcher extends TextWatcher {
  override def beforeTextChanged(s: CharSequence, x: Int, y: Int, z: Int) = none
  override def afterTextChanged(editableCharSequence: Editable) = none
}

trait MyPeerDataListener extends PeerDataEventListener {
  def onChainDownloadStarted(peer: Peer, blocksLeft: Int) = none
  def onPreMessageReceived(peer: Peer, message: Message) = message
  def getData(peer: Peer, m: GetDataMessage) = null
}