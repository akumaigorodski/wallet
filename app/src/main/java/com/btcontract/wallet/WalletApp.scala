package com.btcontract.wallet

import java.net.InetSocketAddress
import java.text.{DecimalFormat, SimpleDateFormat}
import java.util.Date

import akka.actor.Props
import android.app.Application
import android.content._
import android.text.format.DateFormat
import android.view.inputmethod.InputMethodManager
import android.widget.{EditText, Toast}
import androidx.appcompat.app.AppCompatDelegate
import androidx.multidex.MultiDex
import com.btcontract.wallet.R.string._
import com.btcontract.wallet.sqlite._
import com.softwaremill.quicklens._
import fr.acinq.bitcoin.{Block, ByteVector32, Satoshi, SatoshiLong}
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.EclairWallet
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.SSL
import fr.acinq.eclair.blockchain.electrum.ElectrumClientPool.ElectrumServerAddress
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{TransactionReceived, WalletReady}
import fr.acinq.eclair.blockchain.electrum._
import fr.acinq.eclair.blockchain.electrum.db.{CompleteChainWalletInfo, SigningWallet, WatchingWallet}
import fr.acinq.eclair.wire.CommonCodecs.nodeaddress
import fr.acinq.eclair.wire.NodeAddress
import immortan._
import immortan.crypto.Tools._
import immortan.sqlite._
import immortan.utils._
import scodec.bits.BitVector

import scala.collection.mutable
import scala.util.Try


object WalletApp {
  var chainWalletBag: SQLiteChainWallet = _
  var extDataBag: SQLiteData = _
  var txDataBag: SQLiteTx = _
  var app: WalletApp = _

  val txInfos = mutable.Map.empty[ByteVector32, TxInfo]
  var currentChainNode = Option.empty[InetSocketAddress]

  final val dbFileNameMisc = "misc.db"

  final val FIAT_CODE = "fiatCode"
  final val BTC_DENOM = "btcDenom"
  final val ENSURE_TOR = "ensureTor"
  final val CUSTOM_ELECTRUM = "customElectrum"

  def fiatCode: String = app.prefs.getString(FIAT_CODE, "usd")
  def ensureTor: Boolean = app.prefs.getBoolean(ENSURE_TOR, false)

  def denom: Denomination = {
    val denom = app.prefs.getString(BTC_DENOM, BtcDenomination.sign)
    if (denom == BtcDenomination.sign) BtcDenomination else SatDenomination
  }

  def customElectrumAddress: Try[NodeAddress] = Try {
    val rawAddress = app.prefs.getString(CUSTOM_ELECTRUM, new String)
    nodeaddress.decode(BitVector fromValidHex rawAddress).require.value
  }

  def isAlive: Boolean = null != txDataBag && null != chainWalletBag && null != extDataBag && null != app

  def freePossiblyUsedRuntimeResouces: Unit = {
    // Clear listeners, destroy actors, finalize state machines
    try WalletParams.chainWallets.becomeShutDown catch none
    try WalletParams.fiatRates.becomeShutDown catch none
    try WalletParams.feeRates.becomeShutDown catch none
    // Make non-alive and non-operational
    WalletParams.secret = null
    txDataBag = null
  }

  def restart: Unit = {
    freePossiblyUsedRuntimeResouces
    require(!WalletParams.isOperational, "Still operational")
    val intent = new Intent(app, ClassNames.hubActivityClass)
    val restart = Intent.makeRestartActivityTask(intent.getComponent)
    app.startActivity(restart)
    System.exit(0)
  }

  def makeAlive: Unit = {
    // Make application minimally operational (so we can check for seed in db)
    val miscInterface = new DBInterfaceSQLiteAndroidMisc(app, dbFileNameMisc)

    miscInterface txWrap {
      chainWalletBag = new SQLiteChainWallet(miscInterface)
      extDataBag = new SQLiteData(miscInterface)
      txDataBag = new SQLiteTx(miscInterface)
    }

    // In case these are needed early
    WalletParams.chainHash = Block.TestnetGenesisBlock.hash
    WalletParams.connectionProvider = if (ensureTor) new TorConnectionProvider(app) else new ClearnetConnectionProvider
  }

  def makeOperational(secret: WalletSecret): Unit = {
    require(isAlive, "Application is not alive, hence can not become operational")
    val currentCustomElectrum: Try[NodeAddress] = customElectrumAddress
    WalletParams.secret = secret

    extDataBag.db txWrap {
      WalletParams.feeRates = new FeeRates(extDataBag)
      WalletParams.fiatRates = new FiatRates(extDataBag)
    }

    ElectrumClientPool.loadFromChainHash = {
      case _ if currentCustomElectrum.isSuccess => ElectrumServerAddress(currentCustomElectrum.get.socketAddress, SSL.DECIDE).asSome.toSet
      case Block.LivenetGenesisBlock.hash => ElectrumClientPool.readServerAddresses(app.getAssets open "servers_mainnet.json")
      case Block.TestnetGenesisBlock.hash => ElectrumClientPool.readServerAddresses(app.getAssets open "servers_testnet.json")
      case _ => throw new RuntimeException
    }

    CheckPoint.loadFromChainHash = {
      case Block.LivenetGenesisBlock.hash => CheckPoint.load(app.getAssets open "checkpoints_mainnet.json")
      case Block.TestnetGenesisBlock.hash => CheckPoint.load(app.getAssets open "checkpoints_testnet.json")
      case _ => throw new RuntimeException
    }

    val params = WalletParameters(extDataBag, chainWalletBag, txDataBag, dustLimit = 546L.sat)
    val electrumPool = WalletParams.system.actorOf(Props(classOf[ElectrumClientPool], WalletParams.blockCount, WalletParams.chainHash, WalletParams.ec), "connection-pool")
    val sync = WalletParams.system.actorOf(Props(classOf[ElectrumChainSync], electrumPool, params.headerDb, WalletParams.chainHash), "chain-sync")
    val catcher = WalletParams.system.actorOf(Props(new WalletEventsCatcher), "events-catcher")

    val walletExt: WalletExt =
      (WalletExt(wallets = Map.empty, catcher, sync, electrumPool, params) /: chainWalletBag.listWallets) {
        case ext ~ CompleteChainWalletInfo(core: SigningWallet, persistentSigningWalletData, lastBalance, label, false) =>
          val signingWallet = ext.makeSigningWalletParts(core, core.attachedMaster.getOrElse(secret.keys.master), lastBalance, label)
          val wallets1 = ext.wallets.updated(signingWallet.ewt.xPub.publicKey, signingWallet)
          signingWallet.walletRef ! persistentSigningWalletData
          ext.copy(wallets = wallets1)

        case ext ~ CompleteChainWalletInfo(watchCore: WatchingWallet, persistentWatchingWalletData, lastBalance, label, false) =>
          val watchingWallet = ext.makeWatchingWallet84Parts(core = watchCore, lastBalance, label)
          val wallets1 = ext.wallets.updated(watchingWallet.ewt.xPub.publicKey, watchingWallet)
          watchingWallet.walletRef ! persistentWatchingWalletData
          ext.copy(wallets = wallets1)
      }

    WalletParams.chainWallets = if (walletExt.wallets.isEmpty) {
      val defaultWalletLabel = app.getString(R.string.bitcoin_wallet)
      val core = SigningWallet(walletType = EclairWallet.BIP84, attachedMaster = None, isRemovable = false)
      val wallet = walletExt.makeSigningWalletParts(core, secret.keys.master, Satoshi(0L), defaultWalletLabel)
      walletExt.withFreshWallet(wallet)
    } else walletExt

    WalletParams.feeRates.listeners += new FeeRatesListener {
      def onFeeRates(newRatesInfo: FeeRatesInfo): Unit =
        extDataBag.putFeeRatesInfo(newRatesInfo)
    }

    WalletParams.fiatRates.listeners += new FiatRatesListener {
      def onFiatRates(newRatesInfo: FiatRatesInfo): Unit =
        extDataBag.putFiatRatesInfo(newRatesInfo)
    }

    // Guaranteed to fire (and update chainWallets) first
    WalletParams.chainWallets.catcher ! new WalletEventsListener {
      override def onWalletReady(event: WalletReady): Unit = WalletParams.synchronized {
        // Wallet is already persisted in database so our only job at this point is to update runtime
        WalletParams.chainWallets = WalletParams.chainWallets.modify(_.wallets.index(event.xPub.publicKey).info) using {
          // Coin control is always disabled on start, we update it later with animation to make it noticeable
          _.copy(lastBalance = event.balance, isCoinControlOn = event.excludedOutPoints.nonEmpty)
        }
      }

      override def onChainMasterSelected(event: InetSocketAddress): Unit = currentChainNode = event.asSome

      override def onChainDisconnected: Unit = currentChainNode = None

      override def onTransactionReceived(event: TransactionReceived): Unit = {
        def addChainTx(received: Satoshi, sent: Satoshi, description: TxDescription, isIncoming: Long): Unit = txDataBag.db txWrap {
          txDataBag.addTx(event.tx, event.depth, received, sent, event.fee, event.xPubs, description, isIncoming, WalletParams.fiatRates.info.rates, event.stamp)
          txDataBag.addSearchableTransaction(description.queryText(event.tx.txid), event.tx.txid)
          if (event.depth == 1) Vibrator.vibrate
        }

        val sentTxDesc = txInfos.remove(event.tx.txid).map(_.description) getOrElse PlainTxDescription(Nil)
        if (event.sent == event.received + event.fee) addChainTx(event.received, event.sent, sentTxDesc, isIncoming = 1L)
        else if (event.sent > event.received) addChainTx(event.received, event.sent - event.received - event.fee, sentTxDesc, isIncoming = 0L)
        else addChainTx(event.received - event.sent, event.sent, PlainTxDescription(event.addresses), isIncoming = 1L)
      }
    }

    WalletParams.connectionProvider doWhenReady {
      electrumPool ! ElectrumClientPool.InitConnect

      val feeratePeriodHours = 3
      val rateRetry = Rx.retry(Rx.ioQueue.map(_ => WalletParams.feeRates.reloadData), Rx.incSec, 3 to 18 by 3)
      val rateRepeat = Rx.repeat(rateRetry, Rx.incHour, feeratePeriodHours to Int.MaxValue by feeratePeriodHours)
      val feerateObs = Rx.initDelay(rateRepeat, WalletParams.feeRates.info.stamp, feeratePeriodHours * 3600 * 1000L)
      feerateObs.foreach(WalletParams.feeRates.updateInfo, none)

      val fiatPeriodSecs = 60 * 10
      val fiatRetry = Rx.retry(Rx.ioQueue.map(_ => WalletParams.fiatRates.reloadData), Rx.incSec, 3 to 18 by 3)
      val fiatRepeat = Rx.repeat(fiatRetry, Rx.incSec, fiatPeriodSecs to Int.MaxValue by fiatPeriodSecs)
      val fiatObs = Rx.initDelay(fiatRepeat, WalletParams.fiatRates.info.stamp, fiatPeriodSecs * 1000L)
      fiatObs.foreach(WalletParams.fiatRates.updateInfo, none)
    }
  }

  // Fiat conversion

  def currentRate(rates: Fiat2Btc, code: String): Try[Double] = Try(rates apply code)
  def msatInFiat(rates: Fiat2Btc, code: String)(msat: MilliSatoshi): Try[Double] = currentRate(rates, code).map(perBtc => msat.toLong * perBtc / BtcDenomination.factor)
  val currentMsatInFiatHuman: MilliSatoshi => String = msat => msatInFiatHuman(WalletParams.fiatRates.info.rates, fiatCode, msat, immortan.utils.Denomination.formatFiat)

  def msatInFiatHuman(rates: Fiat2Btc, code: String, msat: MilliSatoshi, decimalFormat: DecimalFormat): String = {
    val fiatAmount: String = msatInFiat(rates, code)(msat).map(decimalFormat.format).getOrElse(default = "?")
    val formatted = WalletParams.fiatRates.customFiatSymbols.get(code).map(symbol => s"$symbol$fiatAmount")
    formatted.getOrElse(s"$fiatAmount $code")
  }
}

class WalletApp extends Application { me =>
  WalletApp.app = me

  private[this] lazy val metrics = getResources.getDisplayMetrics
  lazy val prefs: SharedPreferences = getSharedPreferences("prefs", Context.MODE_PRIVATE)
  lazy val scrWidth: Double = metrics.widthPixels.toDouble / metrics.densityDpi
  lazy val maxDialog: Double = metrics.densityDpi * 2.3

  import android.provider.Settings.System.{FONT_SCALE, getFloat}
  // Special handling for cases when user has chosen large font and screen size is constrained
  lazy val tooFewSpace: Boolean = getFloat(getContentResolver, FONT_SCALE, 1) > 1 && scrWidth < 2.4

  lazy val dateFormat: SimpleDateFormat = DateFormat.is24HourFormat(me) match {
    case false if tooFewSpace => new SimpleDateFormat("MM/dd/yy")
    case true if tooFewSpace => new SimpleDateFormat("dd/MM/yy")
    case false => new SimpleDateFormat("MMM dd, yyyy")
    case true => new SimpleDateFormat("d MMM yyyy")
  }

  override def attachBaseContext(base: Context): Unit = {
    super.attachBaseContext(base)
    MultiDex.install(me)
  }

  override def onCreate: Unit = runAnd(super.onCreate) {
    // Currently night theme is the only option, should be set by default
    AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_YES)
  }

  def when(thenDate: Date, simpleFormat: SimpleDateFormat): String =
    System.currentTimeMillis - thenDate.getTime match {
      case ago if ago > 12960000 => simpleFormat.format(thenDate)
      case ago if ago < android.text.format.DateUtils.MINUTE_IN_MILLIS => "now"
      case ago if ago < android.text.format.DateUtils.HOUR_IN_MILLIS => s"${ago / android.text.format.DateUtils.MINUTE_IN_MILLIS} min ago"
      case ago if ago < android.text.format.DateUtils.DAY_IN_MILLIS => s"${ago / android.text.format.DateUtils.HOUR_IN_MILLIS} hr ago"
    }

  def quickToast(code: Int): Unit = quickToast(me getString code)
  def quickToast(msg: CharSequence): Unit = Toast.makeText(me, msg, Toast.LENGTH_LONG).show
  def clipboardManager: ClipboardManager = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def inputMethodManager: InputMethodManager = getSystemService(Context.INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
  def showKeys(field: EditText): Unit = try inputMethodManager.showSoftInput(field, InputMethodManager.SHOW_IMPLICIT) catch none
  def hideKeys(field: EditText): Unit = try inputMethodManager.hideSoftInputFromWindow(field.getWindowToken, 0) catch none

  def copy(text: String): Unit = {
    val bufferContent = ClipData.newPlainText("wallet", text)
    clipboardManager.setPrimaryClip(bufferContent)
    quickToast(copied_to_clipboard)
  }
}

object Vibrator {
  private val vibrator = WalletApp.app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
  def vibrate: Unit = if (null != vibrator && vibrator.hasVibrator) vibrator.vibrate(Array(0L, 85, 200), -1)
}