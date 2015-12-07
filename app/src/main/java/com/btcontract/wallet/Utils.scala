package com.btcontract.wallet

import org.bitcoinj.wallet.DeterministicSeed
import concurrent.ExecutionContext.Implicits.global
import android.view.animation.AccelerateDecelerateInterpolator
import android.widget.RadioGroup.OnCheckedChangeListener
import info.hoang8f.android.segmented.SegmentedGroup
import android.view.inputmethod.InputMethodManager
import android.widget.AbsListView.OnScrollListener
import org.bitcoinj.crypto.KeyCrypterException
import android.text.method.DigitsKeyListener
import android.view.View.OnClickListener
import org.bitcoinj.store.SPVBlockStore
import android.app.AlertDialog.Builder
import android.util.DisplayMetrics
import org.bitcoinj.uri.BitcoinURI
import scala.collection.mutable
import scala.concurrent.Future
import android.net.Uri

import R.id.{amtInSat, amtInBtc, inputAmount, inputBottom, typeUSD, typeEUR, typeCNY}
import R.string.{tx_announce, wallet_password, password_old, pass_checking, tx_1st_conf}
import R.string.{input_dollar, input_euro, input_yuan, input_hint_btc, input_hint_sat}
import R.string.{dialog_ok, dialog_next, dialog_cancel, dialog_back, dialog_pay}
import org.bitcoinj.core.Wallet.{ExceededMaxTransactionSize => TxTooLarge}
import org.bitcoinj.core.Wallet.{CouldNotAdjustDownwards, SendRequest}
import org.bitcoinj.core.{InsufficientMoneyException => NoFunds}
import android.content.{DialogInterface, Context, Intent}
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

import DialogInterface.BUTTON_POSITIVE
import ViewGroup.LayoutParams.WRAP_CONTENT
import scala.language.implicitConversions
import InputMethodManager.HIDE_NOT_ALWAYS
import ViewGroup.FOCUS_BLOCK_DESCENDANTS
import Transaction.MIN_NONDUST_OUTPUT
import Context.INPUT_METHOD_SERVICE


object Utils {
  type TryCoin = Try[Coin]
  type Coins = List[AbstractCoin]
  type Rates = Map[String, Double]
  type PayDatas = mutable.Buffer[PayData]
  type Outputs = mutable.Buffer[TransactionOutput]
  var startupAppReference: WalletApp = null
  lazy val app = startupAppReference

  val interpolator = new AccelerateDecelerateInterpolator
  val passType = InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_PASSWORD
  val textType = InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD

  val Seq(strDollar, strEuro, strYuan) = List("Dollar", "Euro", "Yuan")
  val fiatSignMap = Map(strDollar -> "$%1$s", strEuro -> "€%1$s", strYuan -> "¥%1$s")
  val fiatTextMap = Map(strDollar -> input_dollar, strEuro -> input_euro, strYuan -> input_yuan)
  val revFiatMap = Map(strDollar -> typeUSD, strEuro -> typeEUR, strYuan -> typeCNY)
  val fiatMap = Map(typeUSD -> strDollar, typeEUR -> strEuro, typeCNY -> strYuan)

  val separator = " "
  val appName = "Bitcoin"
  val emptyString = new String
  val rand = new scala.util.Random
  val baseFiat = new DecimalFormat("#.##")
  val baseBtc = new DecimalFormat("#.########")
  val baseSat = new DecimalFormat("###,###,###")
  lazy val sumIn = app getString R.string.txs_sum_in
  lazy val sumOut = app getString R.string.txs_sum_out
  lazy val btcTemplate = app getString R.string.input_alt_btc
  lazy val satTemplate = app getString R.string.input_alt_sat

  val locale = new Locale("en", "US")
  val symbols = new DecimalFormatSymbols(locale)
  baseFiat setDecimalFormatSymbols symbols
  baseSat setDecimalFormatSymbols symbols
  baseBtc setDecimalFormatSymbols symbols

  def inpInBtc(coin: Coin) = baseBtc format BigDecimal(coin.value) / 100000000
  def inpInSat(coin: Coin) = baseSat format coin.value
  def fmt(cn: Coin) = "฿\u00A0" + inpInBtc(cn)

  def humanSum(coin: Coin) = {
    val btc = btcTemplate format inpInBtc(coin)
    val sat = satTemplate format inpInSat(coin)
    s"$btc<br>$sat"
  }

  def denom(cn: Coin)(implicit dec: DenomControl, zero: CharSequence) =
    if (cn.isZero) zero else if (dec.m == R.id.amtInBtc) fmt(cn) else inpInSat(cn)

  def none: PartialFunction[Any, Unit] = { case _ => }
  def wrap(run: => Unit)(go: => Unit) = try go catch none finally run
  def randBtw(start: Float, end: Float) = start + rand.nextFloat * (end - start)
  def humanAddr(adr: Address) = adr.toString grouped 4 mkString separator
}

// Info stack manager

abstract class InfoActivity extends TimerActivity { me =>
  class WalletListener extends AbstractWalletEventListener {
    override def onTransactionConfidenceChanged(w: Wallet, tx: Transaction) =
      if (tx.getConfidence.getDepthInBlocks == 1) say(app getString tx_1st_conf, Informer.TXCONFIRMED)

    override def onCoinsSent(w: Wallet, tx: Transaction, pb: Coin, nb: Coin) =
      say(app getString R.string.tx_sent format fmt(pb subtract nb), Informer.DECSEND)

    override def onReorganize(wallet: Wallet) = say(app getString R.string.reorg, Informer.REORG)
    override def onCoinsReceived(w: Wallet, tx: Transaction, pb: Coin, nb: Coin) = if (nb isGreaterThan pb)
      say(app getString R.string.tx_received format fmt(nb subtract pb), Informer.RECEIVED)

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
  lazy val memo = getResources getStringArray R.array.action_send_memo
  lazy val requestOpts = getResources getStringArray R.array.dialog_request

  // Bottom menu actions
  def doSend(view: View) = {
    val hasPays = app.TransData.payments.isEmpty
    if (hasPays) mkPayForm else new PayPass
  }

  def doReceive(view: View) = Failure(null) match { case fail =>
    val payData = PayData(app.kit.currentAddress, fail)
    app.TransData.value = Option apply payData
    me goTo classOf[RequestActivity]
  }

  def goQRScan(view: View) = me goTo classOf[ScanActivity]
  def goCoins(view: View) = me exitTo classOf[WalletActivity]
  def goHistory(view: View) = me exitTo classOf[TxsActivity]

  // Fee settings and calculation, ask pass on startup
  def rawFeeFactor = prefs.getInt(AbstractKit.FEE_FACTOR, 2)
  def feePerKb(base: Int) = SendRequest.DEFAULT_FEE_PER_KB multiply base * 5
  def feeBase = rawFeeFactor match { case raw => if (raw < 1 | raw > 10) 2 else raw }

  // Activity lifecycle listeners management
  override def onOptionsItemSelected(mi: MenuItem) = {
    val decideActionToTake: PartialFunction[Int, Unit] = {
      case R.id.actionAddresses => me goTo classOf[AdrsActivity]
      case R.id.actionRequestPayment => mkRequestForm
      case R.id.actionConverter => mkConverterForm
      case R.id.actionSettings => mkSetsForm

      case R.id.lockWalletRightNow =>
        wrap(app.kit.stopAsync)(me setPassAsk true)
        me exitTo classOf[MainActivity]
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

    // Clear value right away
    app.TransData.value = None
    super.onResume
  }

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

  class Anim(amt: Coin, curText: String) extends Runnable {
    val txt = if (amt.isZero) getString(R.string.wallet_empty) else fmt(amt)
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

  // Concrete dialogs

  def mkClear(alert: Dialog) = {
    def remove = rm(alert)(process)
    def process = wrap(mkPayForm)(app.TransData.payments.clear)
    val container = getLayoutInflater.inflate(R.layout.frag_top_clear, null)
    val clear = container.findViewById(R.id.clearPayments).asInstanceOf[Button]
    clear setOnClickListener new OnClickListener { def onClick(view: View) = remove }
    container
  }

  def getMemo = app.TransData.payments.size match {
    case hasRoom if hasRoom < memo.length - 1 => memo(hasRoom)
    case noRoom => memo.last format noRoom + 1
  }

  def mkPayForm: SpendManager = (getMemo: LinearLayout) match { case title =>
    val content = getLayoutInflater.inflate(R.layout.frag_input_spend, null, false)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), title, content)
    if (app.TransData.payments.nonEmpty) title.addView(mkClear(alert), 0)

    // Wire up interface
    val dc = new DenomControl(me, content)
    val man = new AmountInputManager(dc)
    val spMan = new SpendManager(man)
    dc.radios check dc.nowMode

    def savePay(tc: TryCoin) = Try {
      val addr = spMan.address.getText.toString
      val pay = PayData(new Address(app.params, addr), tc)
      val isAlready = app.TransData.payments contains pay
      if (!isAlready) app.TransData.payments prepend pay
    } match {
      case Success(addrIsOK) => rm(alert)(new PayPass)
      case Failure(e) => toast(R.string.dialog_addr_wrong)
    }

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener new OnClickListener {
      def onClick(recordDataView: View) = man.result match {
        case Success(coin) if coin isLessThan MIN_NONDUST_OUTPUT => toast(R.string.dialog_sum_dusty)
        case Failure(emptyAmountProvided) => toast(R.string.dialog_sum_empty)
        case addressMayFail => savePay(addressMayFail)
      }
    }

    spMan
  }

  class PayPass {
    // Make a local copy of payments
    val pays = app.TransData.payments map identity
    val totalSum = (Coin.ZERO /: pays)(_ add _.tc.get)
    val inSat = satTemplate format inpInBtc(totalSum)

    // Create all the needed views
    val txt = for (pay <- pays.toArray) yield Html.fromHtml(pay pretty sumOut)
    val (passAsk, secretField) = generatePasswordPromptView(passType, wallet_password)
    val payActs = getLayoutInflater.inflate(R.layout.frag_top_acts, null).asInstanceOf[LinearLayout]
    val divider = getLayoutInflater.inflate(R.layout.frag_divider, null).asInstanceOf[LinearLayout]
    val listCon = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    val alert = mkForm(mkChoiceDialog(confirm, none, dialog_pay, dialog_cancel), null, passAsk)
    listCon setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.actionTip, txt)
    payActs.addView(mkClear(alert), 0)
    passAsk.addView(payActs, 0)
    divider addView listCon
    passAsk addView divider

    // Wire buttons up
    def rePay = rm(alert)(mkPayForm)
    def reScan = rm(alert)(me goQRScan null)
    val addNewAddress = payActs.findViewById(R.id.addNewAddress).asInstanceOf[Button]
    val scanQRPicture = payActs.findViewById(R.id.scanQRPicture).asInstanceOf[Button]
    scanQRPicture setOnClickListener new OnClickListener { def onClick(v: View) = reScan }
    addNewAddress setOnClickListener new OnClickListener { def onClick(v: View) = rePay }
    addNewAddress setText getMemo

    def confirm = {
      add(me getString tx_announce, Informer.DECSEND).ui.run
      <(announceMultiTransaction, react)(none)
      app.TransData.payments.clear
    }

    def announceMultiTransaction = {
      // If no money left & one payee then empty this wallet
      val all = app.kit.currentBalance subtract feePerKb(feeBase) isLessThan totalSum
      val request = if (all & pays.size < 2) SendRequest.emptyWallet(pays.head.adr) else makeReq
      request.aesKey = app.kit.wallet.getKeyCrypter deriveKey secretField.getText.toString
      request.feePerKb = feePerKb(feeBase)

      // Make signed transaction
      app.kit.wallet completeTx request
      // Block until at least 1 peer confirms this transaction
      app.kit.peerGroup.broadcastTransaction(request.tx, 1).broadcast.get
    }

    def makeReq = new Transaction(app.params) match { case txn =>
      for (PayData(address, tc) <- pays) txn.addOutput(tc.get, address)
      SendRequest forTx txn
    }

    def react(exc: Throwable): Unit = exc match {
      case e: CouldNotAdjustDownwards => onError(me getString R.string.err_empty_shrunk)
      case e: TxTooLarge => onError(me getString R.string.err_transaction_too_large)
      case e: NoFunds => onError(me getString R.string.err_low_funds format inSat)
      case e: KeyCrypterException => onError(me getString R.string.err_pass)
      case e: Throwable => onError(me getString R.string.err_general)
    }

    def onError(errMsg: String) = try {
      val info = mkChoiceDialog(new PayPass, none, dialog_ok, dialog_cancel)
      info.setMessage(errMsg).show setCanceledOnTouchOutside false
      app.TransData.payments ++= pays
      del(Informer.DECSEND).run
    } catch none
  }

  def mkRequestForm = {
    val requestText = me getString R.string.action_request_payment
    val content = getLayoutInflater.inflate(R.layout.frag_input_receive, null)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), requestText, content)

    // Wire up request interface
    val dc = new DenomControl(me, content)
    val man = new AmountInputManager(dc)
    dc.radios check dc.nowMode

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener new OnClickListener {
      def onClick(posButtonView: View) = rm(alert) {
        val pay = PayData(app.kit.currentAddress, man.result)
        val titleText = requestText + "<br><br>" + pay.pretty(sumIn)
        val listCon = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
        val adapter = new ArrayAdapter(me, R.layout.frag_center_text, R.id.textItem, requestOpts)
        val dialog = mkForm(me negBld dialog_cancel, Html fromHtml titleText, listCon)

        listCon setOnItemClickListener new AdapterView.OnItemClickListener {
          def onItemClick(par: AdapterView[_], view: View, pos: Int, id: Long) =
            rm(dialog) _ apply choose(pos, pay.getURI)
        }

        app.TransData.value = Option(pay)
        listCon setAdapter adapter
      }
    }
  }

  def mkSetsForm: Unit = {
    val fee = fmt(me feePerKb feeBase)
    val feeTitle = me getString R.string.sets_fee
    val form = getLayoutInflater.inflate(R.layout.frag_settings, null)
    val dialog = mkForm(me negBld dialog_back, null, form)

    val fiatType = form.findViewById(R.id.fiatType).asInstanceOf[SegmentedGroup]
    val rescanChain = form.findViewById(R.id.rescanBlockchain).asInstanceOf[Button]
    val askForPass = form.findViewById(R.id.askForPassword).asInstanceOf[CheckBox]
    val viewMnemonic = form.findViewById(R.id.viewMnemonic).asInstanceOf[Button]
    val changePass = form.findViewById(R.id.changePass).asInstanceOf[Button]
    val setCode = form.findViewById(R.id.setCode).asInstanceOf[Button]
    val setFee = form.findViewById(R.id.setFee).asInstanceOf[Button]
    val about = form.findViewById(R.id.about).asInstanceOf[Button]

    setFee setText Html.fromHtml(s"$feeTitle <font color=#E31300>$fee</font>")
    askForPass setChecked prefs.getBoolean(AbstractKit.PASSWORD_ASK_STARTUP, false)
    askForPass setOnCheckedChangeListener new CompoundButton.OnCheckedChangeListener {
      def onCheckedChanged(button: CompoundButton, isChecked: Boolean) = setPassAsk(isChecked)
    }

    setFee setOnClickListener new OnClickListener {
      override def onClick(view: View) = rm(dialog) {
        val feePerKilobytePicker = new NumberPicker(me)

        feePerKilobytePicker setFormatter new NumberPicker.Formatter {
          def format(chosenFeeFactor: Int) = fmt(me feePerKb chosenFeeFactor)
          feePerKilobytePicker setMaxValue 10
          feePerKilobytePicker setMinValue 1
        }

        def go = prefs.edit.putInt(AbstractKit.FEE_FACTOR, feePerKilobytePicker.getValue).commit
        val feeDialog = mkChoiceDialog(go, mkSetsForm, dialog_ok, dialog_back) setView feePerKilobytePicker
        feeDialog.setCustomTitle(feeTitle).show setInverseBackgroundForced false
        feePerKilobytePicker setDescendantFocusability FOCUS_BLOCK_DESCENDANTS
        feePerKilobytePicker setValue rawFeeFactor
      }
    }

    fiatType check revFiatMap(currentFiatName)
    fiatType setOnCheckedChangeListener new OnCheckedChangeListener {
      def onCheckedChanged(radioGroup: RadioGroup, newCheckedFiatName: Int) = {
        prefs.edit.putString(AbstractKit.CURRENCY, fiatMap apply newCheckedFiatName).commit
      }
    }

    rescanChain setOnClickListener new OnClickListener {
      def wrong = wrap(me toast R.string.password_wrong)(openForm)
      def onClick(view: View) = rm(dialog)(openForm)

      def openForm: Unit = checkPass(wrong) { _ =>
        val alert = mkChoiceDialog(go, mkSetsForm, dialog_ok, dialog_back)
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
      def wrong = wrap(me toast R.string.password_wrong)(openForm)
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
      def wrong = wrap(me toast R.string.password_wrong)(openForm)
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
      def wrong = wrap(me toast R.string.password_wrong)(openForm)
      def onClick(viewMnemonicView: View) = rm(dialog)(openForm)

      def openForm: Unit = passPlus { sec =>
        val crypter = app.kit.wallet.getKeyCrypter
        <(app.kit.wallet.getKeyChainSeed.decrypt(crypter, sec,
          crypter deriveKey sec), _ => wrong)(showMnemonic)
      }

      def showMnemonic(seed: DeterministicSeed): Unit = {
        val builder = new Builder(me).setCustomTitle(me getString R.string.sets_noscreen)
        val mnemonic = TextUtils.join(separator, seed.getMnemonicCode)
        builder.setMessage(mnemonic).show
      }
    }

    about setOnClickListener new OnClickListener {
      def site = Uri parse "http://btcontract.github.io/VisualBitcoinWallet/"
      def onClick(view: View) = me startActivity new Intent(Intent.ACTION_VIEW, site)
    }

    def checkPass(no: => Unit)(next: String => Unit) = passPlus { txt =>
      def process(res: Boolean) = if (res) try next(txt) catch none else no
      <(app.kit.wallet checkPassword txt, _ => no)(process)
    }

    def passPlus(next: String => Unit) = {
      val (passAsk, secret) = generatePasswordPromptView(passType, password_old)
      mkForm(mkChoiceDialog(infoAndNext, mkSetsForm, dialog_next, dialog_back), null, passAsk)

      def infoAndNext = {
        add(app getString pass_checking, Informer.CODECHANGE).ui.run
        timer.schedule(me del Informer.CODECHANGE, 2500)
        next apply secret.getText.toString
      }
    }

    def shortCheck(txtRes: Int, short: Int)(next: String => Unit) = {
      val (passwordAsk, secret) = generatePasswordPromptView(textType, txtRes)
      def check = if (secret.getText.length < 8) toast(short) else next(secret.getText.toString)
      mkForm(mkChoiceDialog(check, mkSetsForm, dialog_ok, dialog_back), null, passwordAsk)
    }
  }
}

// Timer manager
// Activity switcher

abstract class TimerActivity extends Activity { me =>
  val goTo: Class[_] => Unit = me startActivity new Intent(me, _)
  val exitTo: Class[_] => Unit = goto => wrap(finish)(goTo apply goto)
  lazy val prefs = app.getSharedPreferences("prefs", Context.MODE_PRIVATE)
  lazy val maxDialog = metrics.densityDpi * 2.1
  lazy val metrics = new DisplayMetrics
  val timer = new Timer

  lazy val scrWidth = {
    // Screen width in inches
    getWindowManager.getDefaultDisplay getMetrics metrics
    metrics.widthPixels.toDouble / metrics.densityDpi
  }

  // Navigation related methods and timer cancel
  override def onDestroy = wrap(super.onDestroy)(timer.cancel)
  implicit def anyToRunnable(process: => Unit): Runnable = new Runnable { def run = process }
  implicit def uiTask(process: => Runnable): TimerTask = new TimerTask { def run = me runOnUiThread process }
  def setPassAsk = prefs.edit.putBoolean(AbstractKit.PASSWORD_ASK_STARTUP, _: Boolean).commit
  def toast(message: Int) = Toast.makeText(app, message, Toast.LENGTH_LONG).show

  // Fiat conversion utilities
  def inFiat(tc: TryCoin) = currentRate flatMap (rt => for (cn <- tc) yield cn.getValue * rt / 100000000)
  def currentRate = for (rates <- FiatRates.rates) yield rates(currentFiatName)
  def currentFiatName = prefs.getString(AbstractKit.CURRENCY, "Dollar")

  // Run in Future, process results on UI thread
  def <[T](fun: => T, no: Throwable => Unit)(ok: T => Unit) = Future(fun) onComplete {
    case Success(rs) => runOnUiThread(ok apply rs) case Failure(ex) => runOnUiThread(no apply ex)
  }

  // React to request and address options
  def choose(pos: Int, txt: String) = pos match {
    case 0 => me goTo classOf[RequestActivity]
    case 1 => app setBuffer txt
    case 2 => share(txt)
  }

  def share(text: String) = startActivity {
    val sendIntent = new Intent setType "text/plain"
    sendIntent.putExtra(Intent.EXTRA_TEXT, text)
    sendIntent.setAction(Intent.ACTION_SEND)
  }

  // Basis for dialog forms
  implicit def str2View(res: CharSequence): LinearLayout = {
    val view = getLayoutInflater.inflate(R.layout.frag_top_tip, null)
    view.findViewById(R.id.actionTip).asInstanceOf[TextView] setText res
    view.asInstanceOf[LinearLayout]
  }

  def rm(prev: Dialog)(fun: => Unit) = {
    timer.schedule(me anyToRunnable fun, 100)
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

  // Currency converter
  def mkConverterForm = {
    val content = getLayoutInflater.inflate(R.layout.frag_rates, null)
    val fiatTip = content.findViewById(R.id.fiatInputTip).asInstanceOf[TextView]
    val fiatInput = content.findViewById(R.id.fiatInputAmount).asInstanceOf[EditText]
    mkForm(me negBld dialog_cancel, me getString R.string.action_converter, content)

    val dc = new DenomControl(me, content)
    val man = new RateManager(dc)
    dc.radios check dc.nowMode

    val btcListener = new TextChangedWatcher {
      def upd = fiatInput.setText(inFiat(man.result) map baseFiat.format getOrElse emptyString)
      def onTextChanged(s: CharSequence, start: Int, count: Int, after: Int) = if (man.input.hasFocus) upd
    }

    val fiatListener = new TextChangedWatcher {
      def upd = man setSum inBtc.map(Coin valueOf _.toLong) getOrElse man.input.setText(emptyString)
      def inBtc = currentRate map (fiatInput.getText.toString.replace(",", "").toDouble / _ * 100000000)
      def onTextChanged(s: CharSequence, start: Int, count: Int, after: Int) = if (fiatInput.hasFocus) upd
    }

    fiatInput setHint currentFiatName
    fiatTip setText fiatTextMap(currentFiatName)
    fiatInput addTextChangedListener fiatListener
    man.input addTextChangedListener btcListener
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
class AmountInputManager(override val dc: DenomControl) extends RateManager(dc) {
  private[this] val inpBot = dc.view.findViewById(inputBottom).asInstanceOf[TextView]
  def asHuman(sv: String) = fiatSignMap(dc.host.currentFiatName) format sv

  input addTextChangedListener new TextChangedWatcher {
    def base = inpBot setText fiatTextMap(dc.host.currentFiatName)
    def okWay = dc.host.inFiat(result) map baseFiat.format map asHuman map inpBot.setText
    override def onTextChanged(s: CharSequence, x: Int, y: Int, z: Int) = okWay getOrElse base
  }
}

class RateManager(val dc: DenomControl) {
  private[this] val inputMap: Map[Int, Coin => String] = Map(amtInSat -> inpInSat, amtInBtc -> inpInBtc)
  private[this] val hintMap = Map(amtInBtc -> input_hint_btc, amtInSat -> input_hint_sat)
  private[this] val charMap = Map(amtInBtc -> ".0123456789", amtInSat -> ",0123456789")
  val input = dc.view.findViewById(inputAmount).asInstanceOf[EditText]
  val setSum = (_: TryCoin) map inputMap(dc.m) map input.setText
  def result = Try apply norm(dc.m)

  def norm(state: Int) = input.getText.toString.replace(",", "") match {
    case rawClearString if amtInBtc == state => Coin parseCoin rawClearString
    case rawClearString => Coin valueOf rawClearString.toLong
  }

  dc.radios setOnCheckedChangeListener new OnCheckedChangeListener {
    def onCheckedChanged(radioGroup: RadioGroup, checkedButton: Int) = {
      input setKeyListener DigitsKeyListener.getInstance(charMap apply dc.m)
      input.setText(Try apply norm(dc.nowMode) map inputMap(dc.m) getOrElse null)
      input setHint hintMap(dc.m)
      dc.update
    }
  }
}

class DenomControl(val host: TimerActivity, val view: View) {
  val radios = view.findViewById(R.id.inputType).asInstanceOf[SegmentedGroup]
  def update = host.prefs.edit.putBoolean(AbstractKit.BTC_OR_SATOSHI, amtInBtc == m).commit
  def savedAsBtc = host.prefs.getBoolean(AbstractKit.BTC_OR_SATOSHI, true)
  def nowMode = if (savedAsBtc) amtInBtc else amtInSat
  def m = radios.getCheckedRadioButtonId
}

class SpendManager(val man: AmountInputManager) {
  val address = man.dc.view.findViewById(R.id.addressData).asInstanceOf[EditText]
  def setAddressValue(adr: Address) = address setText adr.toString

  def set(uri: BitcoinURI) = {
    this setAddressValue uri.getAddress
    man setSum Try(uri.getAmount)
  }

  def set(data: PayData) = {
    this setAddressValue data.adr
    man setSum data.tc
  }
}

case class PayData(adr: Address, tc: TryCoin) { me =>
  def getURI = BitcoinURI.convertToBitcoinURI(adr, tc getOrElse null, null, null)
  def pretty(way: String) = tc.map(sat => s"${me route way}<br><br>${Utils humanSum sat}") getOrElse route(way)
  def route(way: String) = way format humanAddr(adr)
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