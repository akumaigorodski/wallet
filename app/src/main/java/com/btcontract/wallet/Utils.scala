package com.btcontract.wallet

import android.view.animation.AccelerateDecelerateInterpolator
import android.widget.RadioGroup.OnCheckedChangeListener
import info.hoang8f.android.segmented.SegmentedGroup
import android.view.inputmethod.InputMethodManager
import android.widget.AbsListView.OnScrollListener
import com.github.kevinsawicki.http.HttpRequest
import org.bitcoinj.crypto.KeyCrypterException
import android.text.method.DigitsKeyListener
import android.view.View.OnClickListener
import org.bitcoinj.store.SPVBlockStore
import android.app.AlertDialog.Builder
import android.util.DisplayMetrics
import org.bitcoinj.uri.BitcoinURI
import scala.collection.mutable
import scala.concurrent.Future
import org.json.JSONObject
import android.net.Uri

import R.string.{dialog_ok, dialog_next, dialog_cancel, dialog_back, err_again, wallet_password, password_old}
import R.string.{input_hint_btc, input_hint_sat, input_tip_sat, input_tip_btc, tx_1st_conf, pass_checking}
import R.id.{amtInSat, amtInBtc, inputAmount, inputBottom, typeUSD, typeEUR, typeCNY}

import android.content.DialogInterface.{OnDismissListener, BUTTON_POSITIVE, BUTTON_NEGATIVE}
import android.content.{DialogInterface, SharedPreferences, Context, Intent}
import org.bitcoinj.core.Wallet.{ExceededMaxTransactionSize => TxTooLarge}
import org.bitcoinj.core.Wallet.{CouldNotAdjustDownwards, SendRequest}
import org.bitcoinj.core.{InsufficientMoneyException => NoFunds}
import java.text.{DecimalFormatSymbols, DecimalFormat}
import android.animation.{ValueAnimator, Animator}
import java.util.{Locale, Timer, TimerTask}
import scala.util.{Failure, Success, Try}
import android.app.{Dialog, Activity}

import com.btcontract.wallet.Utils._
import org.bitcoinj.core._
import android.widget._
import android.view._
import android.text._

import concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import InputMethodManager.HIDE_NOT_ALWAYS
import ViewGroup.FOCUS_BLOCK_DESCENDANTS
import Transaction.MIN_NONDUST_OUTPUT
import Context.INPUT_METHOD_SERVICE


object Utils {
  type TryCoin = Try[Coin]
  type Strs = Array[String]
  type PayDatas = List[PayData]
  type Coins = List[AbstractCoin]
  type Outputs = mutable.Buffer[TransactionOutput]

  val separator = " "
  val appName = "Bitcoin"
  val emptyString = new String

  val rand = new scala.util.Random
  val locale = new Locale("en", "US")
  val symbols = new DecimalFormatSymbols(locale)
  val baseSat = new DecimalFormat("###,###,###")
  val baseBtc = new DecimalFormat("#.########")
  val baseFiat = new DecimalFormat("#.##")

  baseFiat setDecimalFormatSymbols symbols
  baseSat setDecimalFormatSymbols symbols
  baseBtc setDecimalFormatSymbols symbols

  val interpolator = new AccelerateDecelerateInterpolator
  val passType = InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_PASSWORD
  val textType = InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD

  // Value will be privded on startup
  var satTemplate, btcTemplate: String = null
  def inpInBtc(sat: Double) = baseBtc format sat / 100000000
  def tipInBtc(sat: Double) = satTemplate format inpInBtc(sat)
  def tipInSat(sat: Double) = btcTemplate format baseSat.format(sat)
  def humanSum(sat: Double) = tipInBtc(sat) + "<br>" + tipInSat(sat)
  def fmt(sat: Long) = "฿\u00A0" + inpInBtc(sat.toDouble)

  def none: PartialFunction[Any, Unit] = { case _ => }
  def wrap(run: => Unit)(go: => Unit) = try go catch none finally run
  def randBtw(start: Float, end: Float) = start + rand.nextFloat * (end - start)
  def humanAddr(address: String) = address grouped 4 mkString separator
}

// Info stack manager

abstract class InfoActivity extends TimerActivity { me =>
  class WalletListener extends AbstractWalletEventListener {
    override def onTransactionConfidenceChanged(w: Wallet, tx: Transaction) =
      if (tx.getConfidence.getDepthInBlocks == 1) say(app getString tx_1st_conf, Informer.TXCONFIRMED)

    override def onCoinsSent(w: Wallet, tx: Transaction, pb: Coin, nb: Coin) =
      say(app getString R.string.tx_sent format fmt(pb.subtract(nb).getValue), Informer.DECSEND)

    override def onCoinsReceived(w: Wallet, tx: Transaction, pb: Coin, nb: Coin) = if (nb isGreaterThan pb)
      say(app getString R.string.tx_received format fmt(nb.subtract(pb).getValue), Informer.RECEIVED)

    override def onReorganize(w: Wallet) =
      say(app getString R.string.reorg, Informer.REORG)

    def say(text: String, infoType: Int) = {
      new Anim(app.kit.currentBalance, getActionBar.getTitle.toString)
      add(text, infoType).timer.schedule(me del infoType, 25000)
      runOnUiThread(ui)
    }
  }

  // Peers listeners

  class CatchUpTracker extends DownloadProgressTracker {
    override def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = {
      val nextTracker = if (left > 100) new ManyBlocksTracker(left) else new FewBlocksTracker
      app.kit.peerGroup addEventListener nextTracker
      app.kit.peerGroup removeEventListener this
    }
  }

  class FewBlocksTracker extends CatchUpTracker {
    override def onBlocksDownloaded(peer: Peer, block: Block, fBlock: FilteredBlock, left: Int) = {
      if (left < 2) add(getString(R.string.info_progress_done), Informer.SYNC).timer.schedule(me del Informer.SYNC, 5000)
      if (left < 2) app.kit.peerGroup removeEventListener this
      runOnUiThread(ui)
    }
  }

  // Display sync process to user if there's way too many blocks
  class ManyBlocksTracker(blocksLeftOnStart: Int) extends FewBlocksTracker {
    override def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) =
    {
      // Update count and then maybe finalize the process
      update(infoProgress format left, Informer.SYNC)
      super.onBlocksDownloaded(p, b, fb, left)
    }

    // Add informer so it can be updated later
    val infoProgress = getString(R.string.info_progress)
    add(infoProgress format blocksLeftOnStart, Informer.SYNC)
  }

  lazy val constantListener = new DownloadProgressTracker {
    val peersInfoOpts = getResources getStringArray R.array.info_peers
    def mkTxt = app.plurOrZero(peersInfoOpts, app.kit.peerGroup.numConnectedPeers)
    override def onPeerDisconnected(p: Peer, pc: Int) = me runOnUiThread update(mkTxt, Informer.PEERS).ui
    override def onPeerConnected(p: Peer, pc: Int) = me runOnUiThread update(mkTxt, Informer.PEERS).ui
    override def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) =
      if (left < 1) app.kit.wallet saveToFile app.walletFile
  }

  var infos = List.empty[Informer]
  var currentAnimation = Option.empty[TimerTask]
  val ui = anyToRunnable(getActionBar setSubtitle infos.head.value)
  lazy val opts = getResources getStringArray R.array.dialog_request
  lazy val sendNum = getResources getStringArray R.array.action_send
  lazy val sendMemo = getResources getStringArray R.array.action_send_memo

  // Menu and overrides

  override def onOptionsItemSelected(mi: MenuItem) = {
    val decideActionToTake: PartialFunction[Int, Unit] = {
      case R.id.actionScanQRCode => me goTo classOf[ScanActivity]
      case R.id.actionTxHistory => me goTo classOf[TxsActivity]
      case android.R.id.home => me goTo classOf[WalletActivity]
      case R.id.actionRequestPayment => mkRequestForm
      case R.id.actionSettings => mkSettingsForm
      case R.id.actionSendMoney => mkPayForm

      case R.id.actionConverter =>
        val bld = negPosBld(dialog_cancel, dialog_next)
        val (alert, inputManager) = me mkConverterForm bld

        // Convinient way to open a pay form with an amount from converter
        alert getButton BUTTON_POSITIVE setOnClickListener new OnClickListener {
          def onClick(v: View) = rm(alert)(mkPayForm.man setAmount inputManager.result)
        }
    }

    decideActionToTake(mi.getItemId)
    super.onOptionsItemSelected(mi)
  }

  override def onResume = {
    app.TransData.value match {
      case Some(vs: Address) => mkPayForm setAddressValue vs
      case Some(vs: BitcoinURI) => mkPayForm set vs
      case _ =>
    }

    // Clear value to prevent form popup
    app.TransData.value = None
    super.onResume
  }

  // Empty stack if user switches pages or closes a form
  override def onBackPressed = wrap(super.onBackPressed)(emptify)
  def emptify = app.TransData.payments = Nil

  // CRUD for informers

  def del(delTag: Int) = uiTask {
    infos = infos.filterNot(_.tag == delTag)
    ui
  }

  def update(txt: String, tag: Int) = {
    for (inf <- infos if inf.tag == tag) inf.value = txt
    this
  }

  def add(tx: String, tag: Int) = {
    infos = new Informer(tx, tag) :: infos
    this
  }

  // Balance animation

  class Anim(amt: Long, curText: String) extends Runnable {
    val txt = if (amt > 0) fmt(amt) else getString(R.string.wallet_empty)
    val max = Math.max(txt.length, curText.length)
    var index = 1

    override def run = {
      getActionBar setTitle s"${txt take index}${curText drop index}".trim
      if (index < max) index += 1 else for (an <- currentAnimation) an.cancel
    }

    for (an <- currentAnimation) an.cancel
    currentAnimation = Some apply uiTask(this)
    timer.schedule(currentAnimation.get, 0, 125)
  }

  // Concrete dialogs

  def mkPayForm = {
    def size = app.TransData.payments.size
    def checkout = confirmPay(app.TransData.payments)

    // Prepare form elements
    val content = getLayoutInflater.inflate(R.layout.frag_input_spend, null)
    val txt = if (size < sendNum.length) sendNum(size) else sendNum.last format size + 1
    val alert = if (size < 1) makeForm(dialog_cancel, null) else makeForm(dialog_back, checkout)

    // Wire up interface
    val denomControl = new DenomControl(prefs, content)
    val man = new AmountInputManager(denomControl)
    val spendManager = new SpendManager(man)

    def makeForm(res: Int, no: => Unit) = {
      val alrt = mkForm(negPosBld(res, dialog_next), txt, content)
      alrt getButton BUTTON_POSITIVE setOnClickListener new OnClickListener { def onClick(v: View) = recordData }
      alrt getButton BUTTON_NEGATIVE setOnClickListener new OnClickListener { def onClick(v: View) = no }
      alrt
    }

    def recordData = man.result match {
      case Failure(emptyAmount) => toast(R.string.dialog_sum_empty)
      case Success(coin) if coin.value > app.kit.currentBalance => toast(R.string.dialog_sum_big)
      case Success(coin) if coin.isLessThan(MIN_NONDUST_OUTPUT) => toast(R.string.dialog_sum_dusty)
      case tc if savePay(tc).isSuccess => rm(alert)(checkout)
      case _ => toast(R.string.dialog_addr_wrong)
    }

    def savePay(tc: TryCoin) = Try {
      // Address checksum may fail and nothing will be recorded
      val addr = new Address(app.params, spendManager.address.getText.toString)
      app.TransData.payments = (PayData(tc, addr) :: app.TransData.payments).distinct
    }

    spendManager
  }

  def confirmPay(pays: PayDatas): Unit = {
    // Prepare payment info in readable forms
    val number = app.plurOrZero(sendMemo, pays.size)
    val totalSum = (0L /: pays)(_ + _.tc.get.value).toDouble
    val txAnnounce = me getString R.string.tx_announce
    val inSatBtc = humanSum(totalSum)
    val inSat = tipInSat(totalSum)

    // Make all the needed views
    val con = str2View(Html fromHtml s"$number<br><br>$inSatBtc")
    val (passAsk, secret) = generatePasswordPromptView(passType, wallet_password)
    val dialog = showChoiceAlert(confirm, emptify, dialog_ok, dialog_cancel)
    val alert = dialog.setCustomTitle(con).setView(passAsk).show

    // Wire everything up
    alert setCanceledOnTouchOutside false
    con addView getLayoutInflater.inflate(R.layout.frag_top_send, null)
    val addNewAddress = con.findViewById(R.id.addNewAddress).asInstanceOf[Button]
    val scanQRPicture = con.findViewById(R.id.scanQRPicture).asInstanceOf[Button]
    val viewTxDetails = con.findViewById(R.id.viewTxDetails).asInstanceOf[Button]

    addNewAddress setOnClickListener new OnClickListener {
      override def onClick(v: View): Unit = rm(alert)(mkPayForm)
    }

    scanQRPicture setOnClickListener new OnClickListener {
      override def onClick(v: View): Unit = rm(alert)(me goTo scanner)
      def scanner = classOf[ScanActivity]
    }

    viewTxDetails setOnClickListener new OnClickListener {
      override def onClick(v: View): Unit = rm(alert)(none)
    }

    def confirm = {
      add(txAnnounce, Informer.DECSEND).ui.run
      <(sendMoney, react)(none)
      emptify
    }

    def sendMoney = {
      // If no money left & one payee then empty this wallet
      val all = totalSum > app.kit.currentBalance - feePerKb(feeBase).value
      val request = if (all & pays.size < 2) SendRequest.emptyWallet(pays.head.adr) else makeReq
      request.aesKey = app.kit.wallet.getKeyCrypter deriveKey secret.getText.toString
      request.feePerKb = feePerKb(feeBase)

      // Make signed transaction
      app.kit.wallet completeTx request
      // Block until at least 1 peer confirms this transaction
      app.kit.peerGroup.broadcastTransaction(request.tx, 1).broadcast.get
    }

    def makeReq = new Transaction(app.params) match { case txn =>
      for (PayData(Success(cn), adr) <- pays) txn.addOutput(cn, adr)
      SendRequest forTx txn
    }

    def react(exception: Throwable) = exception match {
      case e: CouldNotAdjustDownwards => onError(app getString R.string.err_empty_shrunk)
      case e: TxTooLarge => onError(app getString R.string.err_transaction_too_large)
      case e: NoFunds => onError(app getString R.string.err_low_funds format inSat)
      case e: KeyCrypterException => onError(app getString R.string.err_pass)
      case e: Throwable => onError(app getString R.string.err_general)
    }

    def onError(msg: String) = try {
      val info = showChoiceAlert(confirmPay(pays), none, err_again, dialog_cancel)
      info.setMessage(msg).show setCanceledOnTouchOutside false
      del(Informer.DECSEND).run
    } catch none
  }

  def mkRequestForm = {
    val requestText = me getString R.string.action_request_payment
    val content = getLayoutInflater.inflate(R.layout.frag_input_receive, null)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), requestText, content)
    val ok = alert getButton BUTTON_POSITIVE

    // Wire up request interface
    val denomController = new DenomControl(prefs, content)
    val man = new AmountInputManager(denomController)

    ok setOnClickListener new OnClickListener {
      def onClick(positiveButtonView: View) = rm(alert) {
        val pay = PayData(man.result, app.kit.currentAddress)
        val titleText = requestText + "<br><br>" + pay.text("#1BA2E0")

        // Wire up options list adapter
        val listCon = getLayoutInflater.inflate(R.layout.frag_center_list, null)
        val listView = listCon.findViewById(R.id.textCenterView).asInstanceOf[ListView]
        val adapter = new ArrayAdapter(me, R.layout.frag_center_text, R.id.textItem, opts)
        val dialog = mkForm(me negBld dialog_cancel, Html fromHtml titleText, listCon)

        def share = startActivity {
          val sendIntent = new Intent setType "text/plain"
          sendIntent.putExtra(Intent.EXTRA_TEXT, pay.getURI)
          sendIntent.setAction(Intent.ACTION_SEND)
        }

        listView setOnItemClickListener new AdapterView.OnItemClickListener {
          def onItemClick(parent: AdapterView[_], view: View, pos: Int, itemId: Long) = rm(dialog) {
            pos match { case 0 => me goTo classOf[RequestActivity] case 1 => app setBuffer pay.getURI case _ => share }
          }
        }

        app.TransData.value = Option(pay)
        listView setAdapter adapter
      }
    }
  }

  def mkSettingsForm = {
    val fee = fmt(feePerKb(feeBase).getValue)
    val feeTitle = me getString R.string.sets_fee
    val title = me getString R.string.action_settings
    val form = getLayoutInflater.inflate(R.layout.frag_settings, null)
    val dialog = mkForm(me negBld dialog_cancel, title, form)

    val rescanChain = form.findViewById(R.id.rescanBlockchain).asInstanceOf[Button]
    val askForPass = form.findViewById(R.id.askForPassword).asInstanceOf[CheckBox]
    val viewMnemonic = form.findViewById(R.id.viewMnemonic).asInstanceOf[Button]
    val changePass = form.findViewById(R.id.changePass).asInstanceOf[Button]
    val setCode = form.findViewById(R.id.setCode).asInstanceOf[Button]
    val setFee = form.findViewById(R.id.setFee).asInstanceOf[Button]
    val about = form.findViewById(R.id.about).asInstanceOf[Button]

    setFee setText Html.fromHtml(s"$feeTitle <font color=#e31300>$fee</font>")
    askForPass setChecked prefs.getBoolean(AbstractKit.PASSWORD_ASK_STARTUP, false)
    askForPass setOnCheckedChangeListener new CompoundButton.OnCheckedChangeListener {
      def onCheckedChanged(buttonView: CompoundButton, isChecked: Boolean) = update(isChecked)
      def update = prefs.edit.putBoolean(AbstractKit.PASSWORD_ASK_STARTUP, _: Boolean).commit
    }

    setFee setOnClickListener new OnClickListener {
      override def onClick(view: View) = rm(dialog) {
        val feePerKilobytePicker = new NumberPicker(me)

        feePerKilobytePicker setFormatter new NumberPicker.Formatter {
          def format(feeFactor: Int) = fmt(feePerKb(feeFactor).getValue)
          feePerKilobytePicker setMaxValue 10
          feePerKilobytePicker setMinValue 1
        }

        def go = prefs.edit.putInt(AbstractKit.FEE_FACTOR, feePerKilobytePicker.getValue).commit
        val pickerAlert = showChoiceAlert(go, none, dialog_ok, dialog_cancel) setView feePerKilobytePicker
        pickerAlert.setCustomTitle(feeTitle).show setInverseBackgroundForced false
        feePerKilobytePicker setDescendantFocusability FOCUS_BLOCK_DESCENDANTS
        feePerKilobytePicker setValue rawFeeFactor
      }
    }

    rescanChain setOnClickListener new OnClickListener {
      def wrong = wrap(openForm)(me toast R.string.password_wrong)
      def onClick(view: View) = rm(dialog)(openForm)

      def openForm: Unit = checkPass(wrong) { _ =>
        val alert = showChoiceAlert(go, none, dialog_ok, dialog_cancel)
        alert.setMessage(R.string.sets_rescan_ok).show setInverseBackgroundForced false
      }

      def go = try {
        app.chainFile.delete
        app.kit.wallet.reset
        app.kit.store = new SPVBlockStore(app.params, app.chainFile)
        app.kit useCheckPoints app.kit.wallet.getEarliestKeyCreationTime
        app.kit.wallet saveToFile app.walletFile
      } catch none finally System exit 0
    }

    setCode setOnClickListener new OnClickListener {
      def wrong = wrap(openForm)(me toast R.string.password_wrong)
      def onClick(setCodeView: View) = rm(dialog)(openForm)

      def openForm: Unit = checkPass(wrong) { _ =>
        shortCheck(R.string.destruct_new, R.string.destruct_too_short) { code =>
          val okMessage = me getString R.string.sets_destruct_ok format code
          prefs.edit.putString(AbstractKit.DESTRUCT_CODE, code).commit
          Toast.makeText(app, okMessage, Toast.LENGTH_LONG).show
        }
      }
    }

    changePass setOnClickListener new OnClickListener {
      def wrong = wrap(openForm)(me toast R.string.password_wrong)
      def onClick(changePassView: View) = rm(dialog)(openForm)

      def openForm: Unit = checkPass(wrong) { oldPass =>
        shortCheck(R.string.password_new, R.string.password_too_short) { newPass =>
          <(next, criticalError => System exit 0)(_ => me toast R.string.sets_password_ok)
          add(app getString R.string.pass_changing, Informer.CODECHANGE).ui.run
          timer.schedule(me del Informer.CODECHANGE, 5000)

          def next = {
            app.kit.wallet decrypt oldPass
            app.kit encryptWallet newPass
          }
        }
      }
    }

    viewMnemonic setOnClickListener new OnClickListener {
      def wrong = wrap(openForm)(me toast R.string.password_wrong)
      def onClick(viewMnemonicView: View) = rm(dialog)(openForm)

      def openForm: Unit = passPlus { txt =>
        val crypter = app.kit.wallet.getKeyCrypter
        val builder = new Builder(me).setCustomTitle(me getString R.string.sets_noscreen)
        <(app.kit.wallet.getKeyChainSeed.decrypt(crypter, txt, crypter deriveKey txt), _ => wrong) {
          chainSeed => builder.setMessage(TextUtils.join(separator, chainSeed.getMnemonicCode).toUpperCase).show
        }
      }
    }

    about setOnClickListener new OnClickListener {
      def site = Uri parse "http://btcontract.github.io/VisualBitcoinWallet/"
      def onClick(view: View) = me startActivity new Intent(Intent.ACTION_VIEW, site)
    }

    def checkPass(no: => Unit)(next: String => Unit) = passPlus { txt =>
      <(app.kit.wallet checkPassword txt, _ => no)(if (_) try next(txt) catch none else no)
    }

    def passPlus(next: String => Unit) = {
      val (passAsk, secretView) = generatePasswordPromptView(passType, password_old)
      val dialog = showChoiceAlert(informUserAndRunNext, none, dialog_next, dialog_cancel)
      dialog.setView(passAsk).show setCanceledOnTouchOutside false

      def informUserAndRunNext = {
        add(app getString pass_checking, Informer.CODECHANGE).ui.run
        timer.schedule(me del Informer.CODECHANGE, 2500)
        next apply secretView.getText.toString
      }
    }

    def shortCheck(txt: Int, short: Int)(next: String => Unit) = {
      val (passAsk, secretView) = generatePasswordPromptView(textType, txt)
      val dialog = showChoiceAlert(checkInputLength, none, dialog_ok, dialog_cancel)
      dialog.setView(passAsk).show setCanceledOnTouchOutside false

      def checkInputLength =
        if (secretView.getText.length < 8) toast(short)
        else next(secretView.getText.toString)
    }
  }

  // Fee settings and calculation
  def rawFeeFactor = prefs.getInt(AbstractKit.FEE_FACTOR, 2)
  def feeBase = rawFeeFactor match { case raw => if (raw < 1 | raw > 10) 2 else raw }
  def feePerKb(base: Int) = SendRequest.DEFAULT_FEE_PER_KB multiply base * 5
}

// Timer manager
// Activity switcher

abstract class TimerActivity extends Activity { me =>
  val goTo: Class[_] => Unit = me startActivity new Intent(me, _)
  val exitTo: Class[_] => Unit = goto => wrap(finish)(goTo apply goto)
  lazy val prefs = app.getSharedPreferences("prefs", Context.MODE_PRIVATE)
  lazy val app = getApplication.asInstanceOf[WalletApp]
  val timer = new Timer

  // Scaled screen width
  lazy val scrWidth = new DisplayMetrics match { case metrics =>
    getWindowManager.getDefaultDisplay getMetrics metrics
    metrics.widthPixels.toDouble / metrics.densityDpi
  }

  // Navigation related methods and timer cancel
  override def onDestroy = wrap(super.onDestroy)(timer.cancel)
  def toast(message: Int) = Toast.makeText(app, message, Toast.LENGTH_LONG).show
  implicit def anyToRunnable(process: => Unit): Runnable = new Runnable { def run = process }
  implicit def uiTask(process: => Runnable): TimerTask = new TimerTask { def run = me runOnUiThread process }

  // Run in Future, process results on UI thread
  def <[T](fun: => T, no: Throwable => Unit)(ok: T => Unit) = Future(fun) onComplete {
    case Success(rs) => runOnUiThread(ok apply rs) case Failure(ex) => runOnUiThread(no apply ex)
  }

  // Basis for dialog forms

  implicit def str2View(res: CharSequence): LinearLayout = {
    val view = getLayoutInflater.inflate(R.layout.frag_top_tip, null)
    view.findViewById(R.id.actionTip).asInstanceOf[TextView] setText res
    view.asInstanceOf[LinearLayout]
  }

  def rm(prev: Dialog)(fun: => Unit) = {
    timer.schedule(me anyToRunnable fun, 45)
    prev.dismiss
  }

  def generatePasswordPromptView(txType: Int, txt: Int) = {
    val passAsk = getLayoutInflater.inflate(R.layout.frag_changer, null)
    val secret = passAsk.findViewById(R.id.secretInput).asInstanceOf[EditText]
    passAsk.findViewById(R.id.secretTip).asInstanceOf[TextView] setText txt
    secret setInputType txType
    (passAsk, secret)
  }

  def negBld(neg: Int) = new Builder(me).setNegativeButton(neg, null)
  def negPosBld(neg: Int, pos: Int) = negBld(neg).setPositiveButton(pos, null)

  def mkForm(builder: Builder, title: View, content: View) = {
    val alertDialog = builder.setCustomTitle(title).setView(content).show
    alertDialog setCanceledOnTouchOutside false
    alertDialog
  }

  def showChoiceAlert(ok: => Unit, no: => Unit, okRes: Int, noRes: Int) = {
    val cancel = new DialogInterface.OnClickListener { def onClick(x: DialogInterface, w: Int) = no }
    val again = new DialogInterface.OnClickListener { def onClick(x: DialogInterface, w: Int) = ok }
    new Builder(me).setPositiveButton(okRes, again).setNegativeButton(noRes, cancel)
  }

  // Currency converter

  def mkConverterForm(tpl: Builder) = {
    val content = getLayoutInflater.inflate(R.layout.frag_rates, null)
    val alert = mkForm(tpl, me getString R.string.action_converter, content)
    val fiatInput = content.findViewById(R.id.fiatInputAmount).asInstanceOf[EditText]
    val fiatType = content.findViewById(R.id.fiatType).asInstanceOf[SegmentedGroup]
    var loadRatesTask: TimerTask = null
    var rates: Rates = null

    // Get cached data and initialize elements
    val hintMap = Map(typeUSD -> "Dollar", typeEUR -> "Euro", typeCNY -> "Yuan")
    val inputHintMemo = prefs.getString(AbstractKit.CURRENCY, "Dollar")
    val cache = prefs.getString(AbstractKit.RATES_JSON, emptyString)
    val denomControl = new DenomControl(prefs, content)
    val man = new AmountInputManager(denomControl)

    val btcListener = new TextChangedWatcher {
      def convert(amt: Double) = amt * rates.typeMap(fiatType.getCheckedRadioButtonId) / 100000000
      def update = fiatInput.setText(man.amt map man.normMap(denomControl.m) map convert map baseFiat.format getOrElse emptyString)
      def onTextChanged(charSequence: CharSequence, start: Int, count: Int, after: Int) = if (man.input.hasFocus) update
    }

    val fiatListener = new TextChangedWatcher {
      def convert(amount: Double) = amount / rates.typeMap(fiatType.getCheckedRadioButtonId) * 100000000
      def sum = Try(fiatInput.getText.toString.replace(",", "").toDouble) map convert map (Coin valueOf _.toLong)
      def onTextChanged(s: CharSequence, x: Int, y: Int, z: Int) = if (fiatInput.hasFocus) update
      def update = man.setAmount(sum) orElse Try(man.input setText emptyString)
    }

    val changeListener = new OnCheckedChangeListener {
      def onCheckedChanged(rGroup: RadioGroup, cb: Int) = {
        val currentHint = hintMap(fiatType.getCheckedRadioButtonId)
        prefs.edit.putString(AbstractKit.CURRENCY, currentHint).commit
        if (man.input.hasFocus) btcListener.update else fiatListener.update
        fiatInput setHint currentHint
      }
    }

    // Loading and working with JSON
    def fromJSON(rawData: String) = {
      val json = new JSONObject(rawData)
      val usd = json getJSONObject "USD" getDouble "last"
      val eur = json getJSONObject "EUR" getDouble "last"
      val cny = json getJSONObject "CNY" getDouble "last"
      Rates(usd, eur, cny)
    }

    def getRatesData = HttpRequest.get("https://blockchain.info/ticker").body
    def loadRates: Unit = <(getRatesData, _ => loadAgain) { json =>
      prefs.edit.putString(AbstractKit.RATES_JSON, json).commit
      rates = Try apply fromJSON(json) getOrElse rates
      changeListener.onCheckedChanged(null, 100)
      loadAgain
    }

    def loadAgain = {
      loadRatesTask = me anyToRunnable loadRates
      timer.schedule(loadRatesTask, 30000)
    }

    // Initialize everything
    alert setOnDismissListener new OnDismissListener {
      def onDismiss(dialog: DialogInterface) = loadRatesTask.cancel
    }

    rates = Try apply fromJSON(cache) getOrElse Rates(0, 0, 0)
    loadRatesTask = me anyToRunnable loadRates
    loadRatesTask.run

    man.input addTextChangedListener btcListener
    fiatInput addTextChangedListener fiatListener
    fiatType setOnCheckedChangeListener changeListener
    fiatType check hintMap.map(_.swap).apply(inputHintMemo)
    (alert, man)
  }

  def hideKeys(run: => Unit) = try {
    timer.schedule(me anyToRunnable run, 250)
    val mgr = getSystemService(INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
    mgr.hideSoftInputFromWindow(getCurrentFocus.getWindowToken, HIDE_NOT_ALWAYS)
  } catch none
}

// Spinner

class Spinner(tv: TextView) extends Runnable {
  override def run = tv.getText match { case text =>
    if (text.length > 8) tv setText "★" else tv setText s"$text★"
  }
}

// Amount and Spend managers

class AmountInputManager(val dc: DenomControl) {
  val inputMap: Map[Int, Double => String] = Map(amtInSat -> baseSat.format, amtInBtc -> inpInBtc)
  val normMap: Map[Int, Double => Double] = Map(amtInBtc -> (_ * 100000000), amtInSat -> identity)
  val tipMap: Map[Int, Double => String] = Map(amtInBtc -> tipInSat, amtInSat -> tipInBtc)
  val tipBaseMap = Map(amtInBtc -> input_tip_sat, amtInSat -> input_tip_btc)
  val hintMap = Map(amtInBtc -> input_hint_btc, amtInSat -> input_hint_sat)
  val charMap = Map(amtInBtc -> ".0123456789", amtInSat -> ",0123456789")
  val input = dc.v.findViewById(inputAmount).asInstanceOf[EditText]
  val alt = dc.v.findViewById(inputBottom).asInstanceOf[TextView]

  def amt = Try(input.getText.toString.replace(",", "").toDouble)
  def result: TryCoin = amt map normMap(dc.m) map (_.toLong) map Coin.valueOf
  def setAmount(tc: TryCoin) = tc map (_.getValue.toDouble) map inputMap(dc.m) map input.setText

  input addTextChangedListener new TextChangedWatcher {
    override def onTextChanged(s: CharSequence, x: Int, y: Int, z: Int) = tip getOrElse base
    def tip = amt map normMap(dc.m) map tipMap(dc.m) map alt.setText
    def base = alt setText tipBaseMap(dc.m)
  }

  dc.radios setOnCheckedChangeListener new OnCheckedChangeListener {
    def onCheckedChanged(radioGroup: RadioGroup, checkedButton: Int) = {
      input.setText(amt map normMap(dc.nowMode) map inputMap(dc.m) getOrElse null)
      input setKeyListener DigitsKeyListener.getInstance(charMap apply dc.m)
      input setHint hintMap(dc.m)
      dc.updMode
    }
  }

  // Set default input mode
  dc.radios check dc.nowMode
}

class DenomControl(prefs: SharedPreferences, val v: View) {
  val radios = v.findViewById(R.id.inputType).asInstanceOf[SegmentedGroup]
  def updMode = prefs.edit.putBoolean(AbstractKit.BTC_OR_SATOSHI, amtInBtc == m).commit
  def nowIsBtc = prefs.getBoolean(AbstractKit.BTC_OR_SATOSHI, true)
  def nowMode = if (nowIsBtc) amtInBtc else amtInSat
  def m = radios.getCheckedRadioButtonId
}

class SpendManager(val man: AmountInputManager) {
  val address = man.dc.v.findViewById(R.id.addressData).asInstanceOf[EditText]
  def setAddressValue: Address => Unit = address setText _.toString

  def set(uri: BitcoinURI) = {
    this setAddressValue uri.getAddress
    man setAmount Try(uri.getAmount)
  }

  def set(data: PayData) = {
    this setAddressValue data.adr
    man setAmount data.tc
  }
}

case class PayData(tc: TryCoin, adr: Address) {
  // https://medium.com/message/hello-future-pastebin-readers-39d9b4eb935f
  // Do not use labels or memos because your history will bite you otherwise
  def getURI = BitcoinURI.convertToBitcoinURI(adr, tc getOrElse null, null, null)

  def text(addrColor: String) = {
    val base = s"<font color=$addrColor>${Utils humanAddr adr.toString}</font>"
    tc map (_.getValue.toDouble) map humanSum map (vs => s"$base<br><br>$vs") getOrElse base
  }
}

case class Rates(usd: Double, eur: Double, cny: Double) {
  val typeMap = Map(typeUSD -> usd, typeEUR -> eur, typeCNY -> cny)
}

abstract class AnimListener extends ValueAnimator
with ValueAnimator.AnimatorUpdateListener
with Animator.AnimatorListener
{
  override def onAnimationCancel(an: Animator) = { /* nothing */ }
  override def onAnimationRepeat(an: Animator) = { /* nothing */ }
  override def onAnimationStart(an: Animator) = { /* nothing */ }
  setInterpolator(interpolator)
  addUpdateListener(this)
  setIntValues(100, 0)
  addListener(this)
  setDuration(750)
}

abstract class TextChangedWatcher extends TextWatcher {
  override def beforeTextChanged(s: CharSequence, x: Int, y: Int, z: Int) = { /* nothing */ }
  override def afterTextChanged(s: Editable) = { /* nothing */ }
}

abstract class HolderCallback extends SurfaceHolder.Callback {
  override def surfaceChanged(holder: SurfaceHolder, fmt: Int, wd: Int, ht: Int) = { /* nothing */ }
  override def surfaceDestroyed(surfaceHolder: SurfaceHolder) = { /* nothing */ }
}

abstract class ScrollListener extends OnScrollListener {
  def onScroll(v: AbsListView, first: Int, visible: Int, total: Int) = { /* nothing */ }
}