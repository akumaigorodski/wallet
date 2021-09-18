package com.btcontract.wallet

import immortan._
import immortan.utils._
import immortan.sqlite._
import fr.acinq.eclair._
import immortan.crypto.Tools._
import scala.concurrent.duration._
import com.btcontract.wallet.sqlite._
import com.btcontract.wallet.R.string._
import scala.collection.JavaConverters._
import fr.acinq.eclair.blockchain.electrum._

import android.widget.{EditText, Toast}
import java.net.{InetSocketAddress, Socket}
import fr.acinq.bitcoin.{Block, ByteVector32, Satoshi, SatoshiLong}
import fr.acinq.eclair.blockchain.{CurrentBlockCount, EclairWallet}
import android.app.{Application, NotificationChannel, NotificationManager}
import immortan.fsm.{IncomingRevealed, TrampolineAborted, TrampolineRevealed}
import android.content.{ClipData, ClipboardManager, Context, Intent, SharedPreferences}
import fr.acinq.eclair.channel.{CMD_CHECK_FEERATE, NormalCommits, PersistentChannelData}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{TransactionReceived, WalletReady}
import com.btcontract.wallet.utils.{AwaitService, DelayedNotification, LocalBackup, WebsocketBus}
import fr.acinq.eclair.blockchain.electrum.db.{CompleteChainWalletInfo, SigningWallet, WatchingWallet}
import fr.acinq.eclair.blockchain.electrum.ElectrumClientPool.ElectrumServerAddress
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.SSL
import fr.acinq.eclair.wire.CommonCodecs.nodeaddress
import com.btcontract.wallet.BaseActivity.StringOps
import android.view.inputmethod.InputMethodManager
import fr.acinq.eclair.router.Router.RouterConf
import androidx.appcompat.app.AppCompatDelegate
import immortan.utils.Denomination.formatFiat
import fr.acinq.eclair.wire.NodeAddress
import android.text.format.DateFormat
import androidx.multidex.MultiDex
import java.text.SimpleDateFormat
import scala.collection.mutable
import rx.lang.scala.Observable
import java.text.DecimalFormat
import scodec.bits.BitVector
import akka.actor.Props
import android.os.Build
import java.util.Date
import scala.util.Try


object WalletApp {
  var txDataBag: SQLiteTx = _
  var lnUrlBag: SQLiteLNUrl = _
  var chainWalletBag: SQLiteChainWallet = _
  var extDataBag: SQLiteDataExtended = _
  var app: WalletApp = _

  var txDescriptions: Map[ByteVector32, TxDescription] = Map.empty
  var currentChainNode: Option[InetSocketAddress] = None

  final val dbFileNameMisc = "misc.db"
  final val dbFileNameGraph = "graph.db"
  final val dbFileNameEssential = "essential.db"

  val backupSaveWorker: ThrottledWork[Boolean, Any] = new ThrottledWork[Boolean, Any] {
    private def doAttemptStore: Unit = LocalBackup.encryptAndWritePlainBackup(app, dbFileNameEssential, LNParams.chainHash, LNParams.secret.seed)
    def process(useDelay: Boolean, unitAfterDelay: Any): Unit = if (LocalBackup isAllowed app) try doAttemptStore catch none
    def work(useDelay: Boolean): Observable[Any] = if (useDelay) Rx.ioQueue.delay(4.seconds) else Observable.just(null)
  }

  final val USE_AUTH = "useAuth"
  final val FIAT_CODE = "fiatCode"
  final val BTC_DENOM = "btcDenom"
  final val ENSURE_TOR = "ensureTor"
  final val CAP_LN_FEE_TO_CHAIN = "capLNFeeToChain"
  final val LAST_TOTAL_GOSSIP_SYNC = "lastTotalGossipSync"
  final val LAST_NORMAL_GOSSIP_SYNC = "lastNormalGossipSync"
  final val CUSTOM_ELECTRUM_ADDRESS = "customElectrumAddress"
  final val SHOW_RATE_US = "showRateUs"
  final val OPEN_HC = "openHc"

  def useAuth: Boolean = app.prefs.getBoolean(USE_AUTH, false)
  def fiatCode: String = app.prefs.getString(FIAT_CODE, "usd")
  def ensureTor: Boolean = app.prefs.getBoolean(ENSURE_TOR, false)
  def capLNFeeToChain: Boolean = app.prefs.getBoolean(CAP_LN_FEE_TO_CHAIN, false)
  def showRateUs: Boolean = app.prefs.getBoolean(SHOW_RATE_US, true)
  def openHc: Boolean = app.prefs.getBoolean(OPEN_HC, true)

  final val CHECKED_BUTTONS = "checkedButtons"
  def getCheckedButtons(default: Set[String] = Set.empty): mutable.Set[String] = app.prefs.getStringSet(CHECKED_BUTTONS, default.asJava).asScala
  def putCheckedButtons(buttons: Set[String] = Set.empty): Unit = app.prefs.edit.putStringSet(CHECKED_BUTTONS, buttons.asJava).commit

  def denom: Denomination = {
    val denom = app.prefs.getString(BTC_DENOM, SatDenomination.sign)
    if (denom == SatDenomination.sign) SatDenomination else BtcDenomination
  }

  def customElectrumAddress: Try[NodeAddress] = Try {
    val rawAddress = app.prefs.getString(CUSTOM_ELECTRUM_ADDRESS, new String)
    nodeaddress.decode(BitVector fromValidHex rawAddress).require.value
  }

  def isAlive: Boolean = null != txDataBag && null != lnUrlBag && null != chainWalletBag && null != extDataBag && null != app

  def freePossiblyUsedResouces: Unit = {
    // Drop whatever network connections we still have
    WebsocketBus.workers.keys.foreach(WebsocketBus.forget)
    CommsTower.workers.values.map(_.pair).foreach(CommsTower.forget)
    // Clear listeners, destroy actors, finalize state machines
    try LNParams.chainWallets.becomeShutDown catch none
    try LNParams.fiatRates.becomeShutDown catch none
    try LNParams.feeRates.becomeShutDown catch none
    try LNParams.cm.becomeShutDown catch none
    // Make non-alive and non-operational
    LNParams.secret = null
    txDataBag = null
  }

  def restartApplication: Unit = if (isAlive) {
    // This may be called multiple times from different threads
    // execute once if app is alive, otherwise do nothing

    freePossiblyUsedResouces
    require(!LNParams.isOperational, "Still operational")
    val intent = new Intent(app, ClassNames.mainActivityClass)
    app.startActivity(Intent makeRestartActivityTask intent.getComponent)
    System.exit(0)
  }

  def makeAlive: Unit = {
    // Make application minimally operational (so we can check for seed in db)
    val miscInterface = new DBInterfaceSQLiteAndroidMisc(app, dbFileNameMisc)

    miscInterface txWrap {
      txDataBag = new SQLiteTx(miscInterface)
      lnUrlBag = new SQLiteLNUrl(miscInterface)
      chainWalletBag = new SQLiteChainWallet(miscInterface)
      extDataBag = new SQLiteDataExtended(miscInterface)
    }

    // Initialized here in case these are needed early
    LNParams.chainHash = Block.LivenetGenesisBlock.hash
    LNParams.routerConf = RouterConf(initRouteMaxLength = 6)
    LNParams.ourInit = LNParams.createInit
    LNParams.syncParams = new SyncParams
  }

  def makeOperational(secret: WalletSecret): Unit = {
    require(isAlive, "Application is not alive, hence can not become operational")
    val essentialInterface = new DBInterfaceSQLiteAndroidEssential(app, dbFileNameEssential)
    val graphInterface = new DBInterfaceSQLiteAndroidGraph(app, dbFileNameGraph)
    val currentCustomElectrumAddress: Try[NodeAddress] = customElectrumAddress
    LNParams.secret = secret

    val normalBag = new SQLiteNetwork(graphInterface, NormalChannelUpdateTable, NormalChannelAnnouncementTable, NormalExcludedChannelTable)
    val hostedBag = new SQLiteNetwork(graphInterface, HostedChannelUpdateTable, HostedChannelAnnouncementTable, HostedExcludedChannelTable)
    val payBag = new SQLitePayment(extDataBag.db, preimageDb = essentialInterface)

    val chanBag = new SQLiteChannel(essentialInterface, channelTxFeesDb = extDataBag.db) {
      override def put(data: PersistentChannelData): PersistentChannelData = {
        backupSaveWorker.replaceWork(true)
        super.put(data)
      }
    }

    extDataBag.db txWrap {
      LNParams.feeRates = new FeeRates(extDataBag)
      LNParams.fiatRates = new FiatRates(extDataBag)
      LNParams.trampoline = LNParams.defaultTrampolineOn
    }

    val pf = new PathFinder(normalBag, hostedBag) {
      override def getLastTotalResyncStamp: Long = app.prefs.getLong(LAST_TOTAL_GOSSIP_SYNC, 0L)
      override def getLastNormalResyncStamp: Long = app.prefs.getLong(LAST_NORMAL_GOSSIP_SYNC, 0L)
      override def updateLastTotalResyncStamp(stamp: Long): Unit = app.prefs.edit.putLong(LAST_TOTAL_GOSSIP_SYNC, stamp).commit
      override def updateLastNormalResyncStamp(stamp: Long): Unit = app.prefs.edit.putLong(LAST_NORMAL_GOSSIP_SYNC, stamp).commit
      override def getExtraNodes: Set[RemoteNodeInfo] = LNParams.cm.all.values.flatMap(Channel.chanAndCommitsOpt).map(_.commits.remoteInfo).toSet
      override def getPHCExtraNodes: Set[RemoteNodeInfo] = LNParams.cm.allHostedCommits.map(_.remoteInfo).toSet
    }

    ElectrumClientPool.loadFromChainHash = {
      case _ if currentCustomElectrumAddress.isSuccess => ElectrumServerAddress(currentCustomElectrumAddress.get.socketAddress, SSL.LOOSE).asSome.toSet
      case Block.LivenetGenesisBlock.hash => ElectrumClientPool.readServerAddresses(app.getAssets open "servers_mainnet.json", sslEnabled = true)
      case Block.TestnetGenesisBlock.hash => ElectrumClientPool.readServerAddresses(app.getAssets open "servers_testnet.json", sslEnabled = true)
      case _ => throw new RuntimeException
    }

    CheckPoint.loadFromChainHash = {
      case Block.LivenetGenesisBlock.hash => CheckPoint.load(app.getAssets open "checkpoints_mainnet.json")
      case Block.TestnetGenesisBlock.hash => CheckPoint.load(app.getAssets open "checkpoints_testnet.json")
      case _ => throw new RuntimeException
    }

    LNParams.cm = new ChannelMaster(payBag, chanBag, extDataBag, pf) {
      // There will be a disconnect if VPN (Orbot) suddenly stops working, we then clear everything and restart an app
      override def initConnect: Unit = if (ensureTor) app.checkTorProxy(restartApplication)(super.initConnect) else super.initConnect
    }

    // Initialize chain wallet
    import LNParams.{ec, system}
    val params = WalletParameters(extDataBag, chainWalletBag, LNParams.minDustLimit, allowSpendUnconfirmed = true)
    val pool = system.actorOf(Props apply new ElectrumClientPool(LNParams.blockCount, LNParams.chainHash), "connection-pool")
    val sync = system.actorOf(Props apply new ElectrumChainSync(pool, params.headerDb, LNParams.chainHash), "chain-sync")
    val watcher = system.actorOf(Props apply new ElectrumWatcher(LNParams.blockCount, pool), "channel-tx-watcher")
    val catcher = system.actorOf(Props(new WalletEventsCatcher), "events-catcher")

    val walletExt: LNParams.WalletExt =
      (LNParams.WalletExt(wallets = Nil, catcher, sync, pool, watcher, params) /: chainWalletBag.listWallets) {
        case walletExt1 ~ CompleteChainWalletInfo(core: SigningWallet, persistentSigningData, lastBalance, label) =>
          val signingWallet = walletExt1.makeSigningWalletParts(core, lastBalance, label)
          signingWallet.walletRef ! persistentSigningData
          walletExt1 + signingWallet

        case walletExt1 ~ CompleteChainWalletInfo(core: WatchingWallet, persistentWatchingData, lastBalance, label) =>
          val watchingWallet = walletExt1.makeWatchingWalletParts(core, lastBalance, label)
          watchingWallet.walletRef ! persistentWatchingData
          walletExt1 + watchingWallet
      }

    LNParams.chainWallets = if (walletExt.wallets.isEmpty) {
      val params = SigningWallet(EclairWallet.BIP84, isRemovable = false)
      val label = app.getString(R.string.bitcoin_wallet)
      walletExt.withNewSigning(params, label)
    } else walletExt

    LNParams.feeRates.listeners += new FeeRatesListener {
      def onFeeRates(newRatesInfo: FeeRatesInfo): Unit = {
        // We may get fresh feerates after channels become OPEN
        LNParams.cm.all.values.foreach(_ process CMD_CHECK_FEERATE)
        extDataBag.putFeeRatesInfo(newRatesInfo)
      }
    }

    LNParams.fiatRates.listeners += new FiatRatesListener {
      def onFiatRates(newRatesInfo: FiatRatesInfo): Unit =
        extDataBag.putFiatRatesInfo(newRatesInfo)
    }

    // Guaranteed to fire (and update chainWallets) first
    LNParams.chainWallets.catcher ! new WalletEventsListener {
      override def onChainTipKnown(event: CurrentBlockCount): Unit = LNParams.cm.initConnect

      override def onWalletReady(event: WalletReady): Unit = LNParams.updateChainWallet(LNParams.chainWallets withBalanceUpdated event)

      override def onChainMasterSelected(event: InetSocketAddress): Unit = currentChainNode = event.asSome

      override def onChainDisconnected: Unit = currentChainNode = None

      override def onTransactionReceived(event: TransactionReceived): Unit = {
        def addChainTx(received: Satoshi, sent: Satoshi, description: TxDescription, isIncoming: Long): Unit = description match {
          case _: ChanFundingTxDescription => doAddChainTx(received, sent, description, isIncoming, BaseActivity.totalBalance - sent)
          case _ => doAddChainTx(received, sent, description, isIncoming, BaseActivity.totalBalance)
        }

        def doAddChainTx(received: Satoshi, sent: Satoshi, description: TxDescription, isIncoming: Long, totalBalance: MilliSatoshi): Unit = txDataBag.db txWrap {
          txDataBag.addTx(event.tx, event.depth, received, sent, event.feeOpt, event.xPub, description, isIncoming, balanceSnap = totalBalance, LNParams.fiatRates.info.rates)
          txDataBag.addSearchableTransaction(description.queryText(event.tx.txid), event.tx.txid)
        }

        val fee = event.feeOpt.getOrElse(0L.sat)
        val defDescription = TxDescription.define(LNParams.cm.all.values, Nil, event.tx)
        val sentTxDescription = txDescriptions.getOrElse(event.tx.txid, default = defDescription)
        if (event.sent == event.received + fee) addChainTx(received = 0L.sat, sent = fee, sentTxDescription, isIncoming = 0L)
        else if (event.sent > event.received) addChainTx(received = 0L.sat, event.sent - event.received - fee, sentTxDescription, isIncoming = 0L)
        else addChainTx(event.received - event.sent, sent = 0L.sat, TxDescription.define(LNParams.cm.all.values, event.walletAddreses, event.tx), isIncoming = 1L)
      }
    }

    pf.listeners += LNParams.cm.opm
    // Get channels and still active FSMs up and running
    LNParams.cm.all = Channel.load(listeners = Set(LNParams.cm), chanBag)
    // Only schedule periodic resync if Lightning channels are being present
    if (LNParams.cm.all.nonEmpty) pf process PathFinder.CMDStartPeriodicResync
    // This inital notification will create all in/routed/out FSMs
    LNParams.cm.notifyResolvers

    Rx.repeat(Rx.ioQueue.delay(1.second), Rx.incMinute, 2 to Int.MaxValue by 2).foreach { _ =>
      DelayedNotification.cancel(app, DelayedNotification.IN_FLIGHT_HTLC_TAG)
      if (LNParams.cm.inProcessors.nonEmpty) reScheduleInFlight
    }

    Rx.repeat(Rx.ioQueue.delay(2.seconds), Rx.incHour, 1 to Int.MaxValue by 1).foreach { _ =>
      DelayedNotification.cancel(app, DelayedNotification.WATCH_TOWER_TAG)
      if (receivingChannelsExist) reScheduleWatchtower
    }
  }

  def receivingChannelsExist: Boolean = LNParams.cm.allNormal.flatMap(Channel.chanAndCommitsOpt).exists {
    case ChanAndCommits(_, normalCommits: NormalCommits) => normalCommits.remoteNextHtlcId > 0
    case _ => false
  }

  def reScheduleWatchtower: Unit =
    DelayedNotification.schedule(app,
      DelayedNotification.WATCH_TOWER_TAG,
      title = app.getString(delayed_notify_wt_title),
      body = app.getString(delayed_notify_wt_body),
      DelayedNotification.WATCH_TOWER_PERIOD_MSEC)

  def reScheduleInFlight: Unit =
    DelayedNotification.schedule(app,
      DelayedNotification.IN_FLIGHT_HTLC_TAG,
      title = app.getString(delayed_notify_pending_payment_title),
      body = app.getString(delayed_notify_pending_payment_body),
      DelayedNotification.IN_FLIGHT_HTLC_PERIOD_MSEC)

  // Fiat conversion

  def currentRate(rates: Fiat2Btc, code: String): Try[Double] = Try(rates apply code)

  def msatInFiat(rates: Fiat2Btc, code: String)(msat: MilliSatoshi): Try[Double] =
    currentRate(rates, code).map(perBtc => msat.toLong * perBtc / BtcDenomination.factor)

  def msatInFiatHuman(rates: Fiat2Btc, code: String, msat: MilliSatoshi, decimalFormat: DecimalFormat): String = {
    val fiatAmount = msatInFiat(rates, code)(msat).map(f = decimalFormat.format).getOrElse(default = "?")
    val formatted = LNParams.fiatRates.customFiatSymbols.get(code).map(symbol => s"$symbol$fiatAmount")
    formatted.getOrElse(s"$fiatAmount $code")
  }

  val currentMsatInFiatHuman: MilliSatoshi => String = msat =>
    msatInFiatHuman(LNParams.fiatRates.info.rates, fiatCode, msat, formatFiat)
}

object Vibrator {
  private val vibrator = WalletApp.app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
  def vibrate: Unit = if (null != vibrator && vibrator.hasVibrator) vibrator.vibrate(Array(0L, 85, 200), -1)
}

class WalletApp extends Application { me =>
  WalletApp.app = me

  lazy val foregroundServiceIntent = new Intent(me, AwaitService.awaitServiceClass)
  lazy val prefs: SharedPreferences = getSharedPreferences("prefs", Context.MODE_PRIVATE)

  private[this] lazy val metrics = getResources.getDisplayMetrics
  lazy val scrWidth: Double = metrics.widthPixels.toDouble / metrics.densityDpi
  lazy val maxDialog: Double = metrics.densityDpi * 2.3

  import android.provider.Settings.System.{getFloat, FONT_SCALE}
  // Special handling for cases when user has chosen large font and screen size is constrained
  lazy val tooFewSpace: Boolean = getFloat(getContentResolver, FONT_SCALE, 1) > 1 && scrWidth < 2.4

  lazy val dateFormat: SimpleDateFormat = DateFormat.is24HourFormat(me) match {
    case false if tooFewSpace => new SimpleDateFormat("MM/dd/yy")
    case true if tooFewSpace => new SimpleDateFormat("dd/MM/yy")
    case false => new SimpleDateFormat("MMM dd, yyyy")
    case true => new SimpleDateFormat("d MMM yyyy")
  }

  lazy val plur: (Array[String], Long) => String = getString(R.string.lang) match {
    case "eng" | "esp" => (opts: Array[String], num: Long) => if (num == 1) opts(1) else opts(2)
    case "chn" | "jpn" => (phraseOptions: Array[String], _: Long) => phraseOptions(1)
    case "rus" | "ukr" => (phraseOptions: Array[String], num: Long) =>

      val reminder100 = num % 100
      val reminder10 = reminder100 % 10
      if (reminder100 > 10 & reminder100 < 20) phraseOptions(3)
      else if (reminder10 > 1 & reminder10 < 5) phraseOptions(2)
      else if (reminder10 == 1) phraseOptions(1)
      else phraseOptions(3)
  }

  override def attachBaseContext(base: Context): Unit = {
    super.attachBaseContext(base)
    MultiDex.install(me)
  }

  override def onCreate: Unit = runAnd(super.onCreate) {
    // Currently night theme is the only option, should be set by default
    AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_YES)

    if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N_MR1) {
      val manager = this getSystemService classOf[NotificationManager]
      val chan1 = new NotificationChannel(AwaitService.CHANNEL_ID, "Foreground notifications", NotificationManager.IMPORTANCE_DEFAULT)
      val chan2 = new NotificationChannel(DelayedNotification.CHANNEL_ID, "Scheduled notifications", NotificationManager.IMPORTANCE_DEFAULT)
      manager.createNotificationChannel(chan1)
      manager.createNotificationChannel(chan2)
    }

    ChannelMaster.inFinalized.filter(_ => LNParams.cm.inProcessors.isEmpty).foreach { _ =>
      DelayedNotification.cancel(me, DelayedNotification.IN_FLIGHT_HTLC_TAG)
      stopService(foregroundServiceIntent)
    }

    ChannelMaster.stateUpdateStream.filter(_ => LNParams.cm.inProcessors.nonEmpty).foreach { _ =>
      DelayedNotification.cancel(me, DelayedNotification.IN_FLIGHT_HTLC_TAG)
      WalletApp.reScheduleInFlight
    }
  }

  def when(thenDate: Date, simpleFormat: SimpleDateFormat, now: Long = System.currentTimeMillis): String = thenDate.getTime match {
    case ago if now - ago > 12960000 || tooFewSpace || WalletApp.denom == BtcDenomination => simpleFormat.format(thenDate)
    case ago => android.text.format.DateUtils.getRelativeTimeSpanString(ago, now, 0).toString
  }

  def showStickyNotification(titleRes: Int, bodyRes: Int, amount: MilliSatoshi): Unit = {
    val withTitle = foregroundServiceIntent.putExtra(AwaitService.TITLE_TO_DISPLAY, me getString titleRes)
    val bodyText = getString(bodyRes).format(WalletApp.denom.parsedWithSign(amount, Colors.cardIn, Colors.cardZero).html)
    val withBodyAction = withTitle.putExtra(AwaitService.BODY_TO_DISPLAY, bodyText).setAction(AwaitService.ACTION_SHOW)
    androidx.core.content.ContextCompat.startForegroundService(me, withBodyAction)
  }

  def quickToast(code: Int): Unit = quickToast(me getString code)
  def quickToast(msg: CharSequence): Unit = Toast.makeText(me, msg, Toast.LENGTH_LONG).show
  def plurOrZero(opts: Array[String] = Array.empty)(num: Long): String = if (num > 0) plur(opts, num).format(num) else opts(0)
  def clipboardManager: ClipboardManager = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def getBufferUnsafe: String = clipboardManager.getPrimaryClip.getItemAt(0).getText.toString

  def checkTorProxy(onFail: => Unit, socket: Socket = new Socket)(onOk: => Unit): Unit = {
    def connectToProxy: Unit = socket.connect(new InetSocketAddress("localhost", 9050), 4000)
    Rx.ioQueue.map(_ => connectToProxy).toBlocking.subscribe(_ => onOk, _ => onFail, socket.close)
  }

  def inputMethodManager: InputMethodManager = getSystemService(Context.INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
  def showKeys(field: EditText): Unit = try inputMethodManager.showSoftInput(field, InputMethodManager.SHOW_IMPLICIT) catch none
  def hideKeys(field: EditText): Unit = try inputMethodManager.hideSoftInputFromWindow(field.getWindowToken, 0) catch none

  def copy(text: String): Unit = {
    val bufferContent = ClipData.newPlainText("wallet", text)
    clipboardManager.setPrimaryClip(bufferContent)
    quickToast(copied_to_clipboard)
  }
}
