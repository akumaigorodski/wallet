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
import com.btcontract.wallet.R.string._
import com.btcontract.wallet.sqlite._
import fr.acinq.bitcoin.{Block, ByteVector32, Satoshi, SatoshiLong}
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.SSL
import fr.acinq.eclair.blockchain.electrum.ElectrumClientPool.ElectrumServerAddress
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{TransactionReceived, chainHash}
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
  var feeRates: FeeRates = _
  var fiatRates: FiatRates = _
  var secret: WalletSecret = _
  var chainWalletBag: SQLiteChainWallet = _
  var extDataBag: SQLiteData = _
  var txDataBag: SQLiteTx = _
  var app: WalletApp = _

  val seenTxInfos = mutable.Map.empty[ByteVector32, TxInfo]
  val pendingTxInfos = mutable.Map.empty[ByteVector32, TxInfo]
  var currentChainNode = Option.empty[InetSocketAddress]

  final val FIAT_CODE = "fiatCode"
  final val BTC_DENOM = "btcDenom"
  final val ENSURE_TOR = "ensureTor"
  final val APP_OPENS_LEFT = "appOpensLeft"
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

  def isOperational: Boolean = null != ElectrumWallet.chainHash && null != secret && null != ElectrumWallet.connectionProvider && null != fiatRates && null != feeRates

  def freePossiblyUsedRuntimeResouces: Unit = {
    try ElectrumWallet.becomeShutDown catch none
    try fiatRates.becomeShutDown catch none
    try feeRates.becomeShutDown catch none
    // Make non-alive and non-operational
    txDataBag = null
    secret = null
  }

  def restart: Unit = {
    freePossiblyUsedRuntimeResouces
    require(!isOperational, "Still operational")
    val intent = new Intent(app, ClassNames.hubActivityClass)
    val restart = Intent.makeRestartActivityTask(intent.getComponent)
    app.startActivity(restart)
    System.exit(0)
  }

  def makeAlive: Unit = {
    ElectrumWallet.chainHash = Block.LivenetGenesisBlock.hash
    val miscInterface = new DBInterfaceSQLiteAndroidMisc(app, "misc.db")

    miscInterface txWrap {
      chainWalletBag = new SQLiteChainWallet(miscInterface)
      extDataBag = new SQLiteData(miscInterface)
      txDataBag = new SQLiteTx(miscInterface)
    }
  }

  def makeOperational(sec: WalletSecret): Unit = {
    require(isAlive, "Halted, application is not alive yet")
    val currentCustomElectrum: Try[NodeAddress] = customElectrumAddress
    ElectrumWallet.params = WalletParameters(extDataBag, chainWalletBag, txDataBag, dustLimit = 546L.sat)
    ElectrumWallet.connectionProvider = if (ensureTor) new TorConnectionProvider(app) else new ClearnetConnectionProvider
    secret = sec

    extDataBag.db txWrap {
      feeRates = new FeeRates(extDataBag)
      fiatRates = new FiatRates(extDataBag)
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

    ElectrumWallet.pool = ElectrumWallet.system.actorOf(Props(classOf[ElectrumClientPool], ElectrumWallet.chainHash, ElectrumWallet.ec), "connection-pool")
    ElectrumWallet.sync = ElectrumWallet.system.actorOf(Props(classOf[ElectrumChainSync], ElectrumWallet.pool, ElectrumWallet.params.headerDb, ElectrumWallet.chainHash), "chain-sync")
    ElectrumWallet.catcher = ElectrumWallet.system.actorOf(Props(new WalletEventsCatcher), "events-catcher")

    chainWalletBag.listWallets collect {
      case CompleteChainWalletInfo(core: SigningWallet, persistentData, lastBalance, label, false) =>
        val ewt = ElectrumWalletType.makeSigningType(core.walletType, core.attachedMaster.getOrElse(secret.keys.master), chainHash)
        val spec = ElectrumWallet.makeSigningWalletParts(core, ewt, lastBalance, label)
        ElectrumWallet.specs.update(ewt.xPub, spec)
        spec.walletRef ! persistentData

      case CompleteChainWalletInfo(core: WatchingWallet, persistentData, lastBalance, label, false) =>
        val spec = ElectrumWallet.makeWatchingWallet84Parts(core, lastBalance, label)
        ElectrumWallet.specs.update(core.xPub, spec)
        spec.walletRef ! persistentData
    }

    if (ElectrumWallet.specs.isEmpty) {
      val core = SigningWallet(ElectrumWallet.BIP84)
      val label = app.getString(R.string.bitcoin_wallet)
      val ewt = ElectrumWalletType.makeSigningType(core.walletType, secret.keys.master, chainHash)
      val spec = ElectrumWallet.makeSigningWalletParts(core, ewt, 0L.sat, label)
      ElectrumWallet.addWallet(spec)
    }

    feeRates.listeners += new FeeRatesListener {
      def onFeeRates(info: FeeRatesInfo): Unit =
        extDataBag.putFeeRatesInfo(info)
    }

    fiatRates.listeners += new FiatRatesListener {
      def onFiatRates(info: FiatRatesInfo): Unit =
        extDataBag.putFiatRatesInfo(info)
    }

    ElectrumWallet.catcher ! new WalletEventsListener {
      override def onChainDisconnected: Unit = currentChainNode = Option.empty[InetSocketAddress]
      override def onChainMasterSelected(event: InetSocketAddress): Unit = currentChainNode = event.asSome

      override def onTransactionReceived(event: TransactionReceived): Unit = {
        def addChainTx(received: Satoshi, sent: Satoshi, fee: Satoshi, description: TxDescription, isIncoming: Long): Unit = txDataBag.db txWrap {
          txDataBag.addTx(event.tx, event.depth, received, sent, fee, event.xPubs, description, isIncoming, fiatRates.info.rates, event.stamp)
          txDataBag.addSearchableTransaction(description.queryText(event.tx.txid), event.tx.txid)
          if (event.depth == 1) Vibrator.vibrate
        }

        pendingTxInfos.remove(event.tx.txid)
        seenTxInfos.get(event.tx.txid) match {
          case Some(seen) => addChainTx(seen.receivedSat, seen.sentSat, seen.feeSat, seen.description, isIncoming = seen.incoming)
          case None if event.received > event.sent => addChainTx(event.received - event.sent, event.sent, Satoshi(0L), PlainTxDescription(event.addresses), isIncoming = 1L)
          case None => addChainTx(event.received, event.sent - event.received, Satoshi(0L), PlainTxDescription(event.addresses), isIncoming = 0L)
        }
      }
    }

    ElectrumWallet.connectionProvider doWhenReady {
      ElectrumWallet.pool ! ElectrumClientPool.InitConnect

      val feeratePeriodHours = 2
      val rateRetry = Rx.retry(Rx.ioQueue.map(_ => feeRates.reloadData), Rx.incSec, 3 to 18 by 3)
      val rateRepeat = Rx.repeat(rateRetry, Rx.incHour, feeratePeriodHours to Int.MaxValue by feeratePeriodHours)
      val feerateObs = Rx.initDelay(rateRepeat, feeRates.info.stamp, feeratePeriodHours * 3600 * 1000L)
      feerateObs.foreach(feeRates.updateInfo, none)

      val fiatPeriodSecs = 60 * 3
      val fiatRetry = Rx.retry(Rx.ioQueue.map(_ => fiatRates.reloadData), Rx.incSec, 3 to 18 by 3)
      val fiatRepeat = Rx.repeat(fiatRetry, Rx.incSec, fiatPeriodSecs to Int.MaxValue by fiatPeriodSecs)
      fiatRepeat.foreach(fiatRates.updateInfo, none)
    }
  }

  // Fiat conversion

  def currentRate(rates: Fiat2Btc, code: String): Try[Double] = Try(rates apply code)
  def msatInFiat(rates: Fiat2Btc, code: String)(msat: MilliSatoshi): Try[Double] = currentRate(rates, code).map(perBtc => msat.toLong * perBtc / BtcDenomination.factor)
  val currentMsatInFiatHuman: MilliSatoshi => String = msat => msatInFiatHuman(fiatRates.info.rates, fiatCode, msat, immortan.utils.Denomination.formatFiat)

  def msatInFiatHuman(rates: Fiat2Btc, code: String, msat: MilliSatoshi, decimalFormat: DecimalFormat): String = {
    val fiatAmount: String = msatInFiat(rates, code)(msat).map(decimalFormat.format).getOrElse(default = "?")
    fiatRates.customFiatSymbols.get(code).map(sign => s"$sign$fiatAmount").getOrElse(s"$fiatAmount $code")
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
  lazy val tooFewSpace: Boolean = getFloat(getContentResolver, FONT_SCALE, 1) > 1.15 || scrWidth < 2.4

  lazy val dateFormat: SimpleDateFormat = DateFormat.is24HourFormat(me) match {
    case false if tooFewSpace => new SimpleDateFormat("MM/dd/yy")
    case true if tooFewSpace => new SimpleDateFormat("dd/MM/yy")
    case false => new SimpleDateFormat("MMM dd, yyyy")
    case true => new SimpleDateFormat("d MMM yyyy")
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