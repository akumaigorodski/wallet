package com.btcontract.wallet

import scala.collection.JavaConverters.asScalaBufferConverter
import com.google.common.util.concurrent.Service.State
import android.graphics.BitmapFactory.decodeResource
import org.bitcoinj.net.discovery.DnsDiscovery
import org.bitcoinj.wallet.KeyChain.KeyPurpose
import org.bitcoinj.core.Wallet.BalanceType
import org.bitcoinj.crypto.KeyCrypterScrypt
import com.google.protobuf.ByteString
import org.bitcoinj.wallet.Protos
import android.app.Application
import android.widget.Toast
import android.os.Vibrator
import java.io.File

import org.bitcoinj.uri.{RequiredFieldValidationException, OptionalFieldValidationException}
import org.bitcoinj.uri.{BitcoinURIParseException, BitcoinURI}
import android.content.{ClipData, ClipboardManager, Context}
import org.jbox2d.dynamics.{BodyType, BodyDef}
import android.graphics.{Typeface, Paint}
import State.{STARTING, RUNNING}
import org.bitcoinj.core._

import com.btcontract.wallet.Utils.Strs
import Context.CLIPBOARD_SERVICE
import Paint.ANTI_ALIAS_FLAG


class WalletApp extends Application {
  lazy val params = org.bitcoinj.params.TestNet3Params.get
  val mls = java.util.concurrent.TimeUnit.MILLISECONDS
  val fontPaint = new Paint(ANTI_ALIAS_FLAG)
  val coinBodyDef = new BodyDef

  // These are provided on startup
  var walletFile, chainFile: File = null
  var kit: WalletKit = null

  lazy val plur = getString(R.string.lang) match {
    case "eng" | "esp" => (opts: Strs, num: Int) => if (num == 1) opts(1) else opts(2)
    case "chn" => (phraseOptions: Strs, num: Int) => phraseOptions(1)
    case "rus" | "ukr" => (phraseOptions: Strs, num: Int) =>

      val reminder100 = num % 100
      val reminder10 = reminder100 % 10
      if (reminder100 > 10 & reminder100 < 20) phraseOptions(3)
      else if (reminder10 > 1 & reminder10 < 5) phraseOptions(2)
      else if (reminder10 == 1) phraseOptions(1)
      else phraseOptions(3)
  }

  def mgr = getSystemService(CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def plurOrZero(options: Strs, num: Int) = if (num > 0) plur(options, num) format num else options(0)
  def isAlive = if (null == kit) false else kit.state match { case STARTING | RUNNING => true case _ => false }

  def setBuffer(data: String) = {
    mgr setPrimaryClip ClipData.newPlainText(Utils.appName, data)
    Toast.makeText(this, R.string.copied_to_buffer, Toast.LENGTH_LONG).show
  }

  override def onCreate = Utils.wrap(super.onCreate) {
    walletFile = new File(getFilesDir, s"${Utils.appName}.wallet")
    chainFile = new File(getFilesDir, s"${Utils.appName}.spvchain")
    fontPaint setTypeface Typeface.create("Droid Sans", Typeface.NORMAL)
    AbstractCoin.bitLogo = decodeResource(getResources, R.drawable.bitwhite2)
    Utils.btcTemplate = getString(R.string.input_alt_sat)
    Utils.satTemplate = getString(R.string.input_alt_btc)
    coinBodyDef setType BodyType.DYNAMIC
    fontPaint setColor 0xBBFFFFFF
  }

  object TransData {
    var value = Option.empty[Any]
    var payments = List.empty[PayData]

    def setValue(raw: String) = value = Option {
      if (raw startsWith "bitcoin:") new BitcoinURI(params, raw)
      else new Address(params, raw)
    }

    def onFail(err: Int => Unit): PartialFunction[Throwable, Unit] = {
      case e: RequiredFieldValidationException => err(R.string.err_required_field)
      case e: OptionalFieldValidationException => err(R.string.err_optional_field)
      case e: WrongNetworkException => err(R.string.err_different_net)
      case e: AddressFormatException => err(R.string.err_address)
      case e: BitcoinURIParseException => err(R.string.err_uri)
      case e: ArithmeticException => err(R.string.err_neg)
      case e: Throwable => err(R.string.err_general)
    }
  }

  abstract class WalletKit extends AbstractKit {
    def autoSaveOff = wallet.shutdownAutosaveAndWait
    def autoSaveOn = wallet.autosaveToFile(walletFile, 500, mls, null)
    def freshOuts = wallet.calculateAllSpendCandidates(false, true).asScala
    def currentAddress = wallet.currentAddress(KeyPurpose.RECEIVE_FUNDS)
    def currentBalance = wallet.getBalance(BalanceType.ESTIMATED).value
    override def shutDown = if (peerGroup.isRunning) peerGroup.stop

    def encryptWallet(pass: CharSequence) = {
      val randSalt8Bytes = ByteString copyFrom KeyCrypterScrypt.randomSalt
      val scrypt = Protos.ScryptParameters.newBuilder setSalt randSalt8Bytes setN 65536
      val cr = new KeyCrypterScrypt(scrypt.build)
      wallet.encrypt(cr, cr deriveKey pass)
    }

    def useCheckPoints(time: Long) = {
      val pts = getAssets open "checkpoints.txt"
      //CheckpointManager.checkpoint(params, pts, store, time)
    }

    def setupAndStartDownload = {
      wallet addEventListener listener
      wallet.allowSpendingUnconfirmedTransactions
      peerGroup addPeerDiscovery new DnsDiscovery(params)
      peerGroup.setUserAgent(Utils.appName, "1.03")
      peerGroup setDownloadTxDependencies false
      peerGroup setPingIntervalMsec 10000
      peerGroup addWallet wallet
      startDownload
      autoSaveOn
    }
  }

  val listener = new AbstractWalletEventListener {
    override def onTransactionConfidenceChanged(w: Wallet, t: Transaction) = if (t.getConfidence.getDepthInBlocks == 1) vibrate(confirmed)
    override def onCoinsReceived(w: Wallet, t: Transaction, pb: Coin, nb: Coin) = if (nb isGreaterThan pb) vibrate(processed)
    override def onCoinsSent(w: Wallet, t: Transaction, pb: Coin, nb: Coin) = vibrate(processed)

    def vibrate(vibrationPattern: Pattern) = {
      val vib = getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[Vibrator]
      if (vib.hasVibrator) vib.vibrate(vibrationPattern, -1)
    }

    val confirmed = Array(0L, 75, 250, 75, 250)
    val processed = Array(0L, 85, 200)
    type Pattern = Array[Long]
  }

  // Coins related

  def txtPaint(size: Long) = {
    val paint = new Paint(fontPaint)
    paint setTextSize txMap(size)
    paint
  }

  def mk(sum: Long, div: Long, paint: Paint, out: TransactionOutput) =
    if (div < 1000000000L) new CircleCoin(paint, sizeMap(div), getText(div, sum), sum, txtPaint(div), out)
    else new BitCoin(paint, sizeMap(div), getText(div, sum), sum, txtPaint(div), out)

  def getText(div: Long, sum: Long) = div match {
    case 1000000000000000L => s"${sum / 100000000000000L}M"
    case 100000000000000L => s"${sum / 100000000000L}K"
    case 10000000000000L => s"${sum / 100000000000L}K"
    case 1000000000000L => s"${sum / 100000000000L}K"
    case 100000000000L => (sum / 100000000L).toString
    case 10000000000L => (sum / 100000000L).toString
    case 1000000000L => (sum / 100000000L).toString
    case 100000000L => s"${sum / 1000000L}M"
    case 10000000L => s"${sum / 1000000L}M"
    case 1000000L => s"${sum / 1000L}K"
    case 100000L => s"${sum / 1000L}K"
    case 10000L => s"${sum / 1000L}K"
    case 1000L => sum.toString
  }

  val sizeMap = Map(1000000000000000L -> 1.35f, 100000000000000L -> 1.3f,
    10000000000000L -> 1.20f, 1000000000000L -> 1.15f, 100000000000L -> 1.1f,
    10000000000L -> 1f, 1000000000L -> 0.95f, 100000000L -> 0.9f, 10000000L -> 0.85f,
    1000000L -> 0.75f, 100000L -> 0.7f, 10000L -> 0.65f, 1000L -> 0.6f)

  lazy val txMap = Map(
    1000000000000000L -> getResources.getDimensionPixelSize(R.dimen.font_12),
    100000000000000L -> getResources.getDimensionPixelSize(R.dimen.font_11),
    10000000000000L -> getResources.getDimensionPixelSize(R.dimen.font_10),
    1000000000000L -> getResources.getDimensionPixelSize(R.dimen.font_9),
    100000000000L -> getResources.getDimensionPixelSize(R.dimen.font_8),
    10000000000L -> getResources.getDimensionPixelSize(R.dimen.font_7),
    1000000000L -> getResources.getDimensionPixelSize(R.dimen.font_6),
    100000000L -> getResources.getDimensionPixelSize(R.dimen.font_5),
    10000000L -> getResources.getDimensionPixelSize(R.dimen.font_4),
    1000000L -> getResources.getDimensionPixelSize(R.dimen.font_3),
    100000L -> getResources.getDimensionPixelSize(R.dimen.font_2),
    10000L -> getResources.getDimensionPixelSize(R.dimen.font_1),
    1000L -> getResources.getDimensionPixelSize(R.dimen.font_0)
  )
}