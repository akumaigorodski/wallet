package com.btcontract.wallet

import Utils._
import R.string._
import org.bitcoinj.core._

import org.bitcoinj.core.listeners.TransactionConfidenceEventListener
import com.btcontract.wallet.lightning.lncloud.OpenHelper
import info.guardianproject.netcipher.proxy.OrbotHelper
import collection.JavaConverters.asScalaBufferConverter
import com.google.common.util.concurrent.Service.State
import org.bitcoinj.net.discovery.DnsDiscovery
import org.bitcoinj.wallet.KeyChain.KeyPurpose
import org.bitcoinj.wallet.Wallet.BalanceType
import com.btcontract.wallet.lightning.Tools
import com.google.protobuf.ByteString
import android.app.Application
import android.widget.Toast
import java.io.File

import org.bitcoinj.wallet.listeners.{WalletChangeEventListener, WalletCoinsSentEventListener, WalletCoinsReceivedEventListener}
import org.bitcoinj.uri.{BitcoinURIParseException, OptionalFieldValidationException}
import org.bitcoinj.uri.{RequiredFieldValidationException, BitcoinURI}
import org.bitcoinj.wallet.{DeterministicSeed, Wallet, Protos}
import android.content.{ClipData, ClipboardManager, Context}
import org.bitcoinj.crypto.{ChildNumber, KeyCrypterScrypt}
import State.{STARTING, RUNNING}

import java.util.concurrent.TimeUnit.MILLISECONDS
import Context.CLIPBOARD_SERVICE


class WalletApp extends Application {
  lazy val prefs = getSharedPreferences("prefs", Context.MODE_PRIVATE)
  lazy val params = org.bitcoinj.params.MainNetParams.get
  var walletFile, chainFile: java.io.File = null
  var kit: WalletKit = null

  lazy val plur = getString(lang) match {
    case "eng" | "esp" => (opts: Array[String], num: Int) => if (num == 1) opts(1) else opts(2)
    case "chn" | "jpn" => (phraseOptions: Array[String], num: Int) => phraseOptions(1)
    case "rus" | "ukr" => (phraseOptions: Array[String], num: Int) =>

      val reminder100 = num % 100
      val reminder10 = reminder100 % 10
      if (reminder100 > 10 & reminder100 < 20) phraseOptions(3)
      else if (reminder10 > 1 & reminder10 < 5) phraseOptions(2)
      else if (reminder10 == 1) phraseOptions(1)
      else phraseOptions(3)
  }

  // Both these methods may throw
  def getTo(base58: String) = Address.fromBase58(params, base58)
  def getTo(out: TransactionOutput) = out.getScriptPubKey.getToAddress(params, true)
  def isAlive = if (null == kit) false else kit.state match { case STARTING | RUNNING => true case _ => false }
  def plurOrZero(opts: Array[String], number: Int) = if (number > 0) plur(opts, number) format number else opts(0)
  def orbotOnline = OrbotHelper isOrbotRunning this

  def setBuffer(bufferTextMessage: String) = {
    val manager = getSystemService(CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
    manager setPrimaryClip ClipData.newPlainText(appName, bufferTextMessage)
    Toast.makeText(this, copied_to_clipboard, Toast.LENGTH_LONG).show
  }

  // Startup actions
  override def onCreate = wrap(super.onCreate) {
    chainFile = new File(getFilesDir, s"$appName.spvchain")
    walletFile = new File(getFilesDir, s"$appName.wallet")
    startupAppReference = this
  }

  object TransData {
    var value = Option.empty[Any]
    val LIGHTNING = "lightning:identity"
    val LNCLOUDSECRET = "lncloud:secret"

    def setValue(text: String) = value = Option {
      if (text startsWith LNCLOUDSECRET) LNCLOUDSECRET :: text.replace(s"$LNCLOUDSECRET:", "") :: Nil
      else if (text startsWith LIGHTNING) LIGHTNING :: text.replace(s"$LIGHTNING:", "") :: Nil
      else if (text startsWith "bitcoin") new BitcoinURI(params, text)
      else getTo(text)
    }

    def onFail(err: Int => Unit): PartialFunction[Throwable, Unit] = {
      case _: RequiredFieldValidationException => err(err_required_field)
      case _: OptionalFieldValidationException => err(err_optional_field)
      case _: WrongNetworkException => err(err_different_net)
      case _: AddressFormatException => err(err_address)
      case _: BitcoinURIParseException => err(err_uri)
      case _: ArithmeticException => err(err_neg)
      case _: Throwable => err(err_general)
    }
  }

  object LNData {
    private var seed: DeterministicSeed = null
    lazy val db = new OpenHelper(app, "lightning.db", 1)
    lazy val idKey = Tools.derive(new ChildNumber(0) :: Nil, 101)(seed)
    def setSeed(newSeed: DeterministicSeed) = seed = newSeed
    def seedAbsent = seed == null

    def newCommitKey = {
      val riseInt = (System.currentTimeMillis / 1000 / 60).toInt
      Tools.derive(new ChildNumber(riseInt) :: Nil, 100)(seed)
    }
  }

  abstract class WalletKit extends AbstractKit {
    def autoSaveOn = wallet.autosaveToFile(walletFile, 500, MILLISECONDS, null)
    def freshOuts = wallet.calculateAllSpendCandidates(false, true).asScala
    def currentBalance = wallet getBalance BalanceType.ESTIMATED_SPENDABLE
    def currentAddress = wallet currentAddress KeyPurpose.RECEIVE_FUNDS
    override def shutDown = if (peerGroup.isRunning) peerGroup.stop

    def encryptWallet(pass: CharSequence) = {
      val randSalt = ByteString copyFrom KeyCrypterScrypt.randomSalt
      val scryptBuilder = Protos.ScryptParameters.newBuilder setSalt randSalt setN 65536
      val cr = new KeyCrypterScrypt(scryptBuilder.build)
      wallet.encrypt(cr, cr deriveKey pass)
    }

    def useCheckPoints(time: Long) = {
      val pts = getAssets open "checkpoints.txt"
      CheckpointManager.checkpoint(params, pts, store, time)
    }

    def setupAndStartDownload = {
      wallet.allowSpendingUnconfirmedTransactions
      wallet addCoinsSentEventListener Vibr.generalTracker
      wallet addCoinsReceivedEventListener Vibr.generalTracker
      wallet addTransactionConfidenceEventListener Vibr.generalTracker
      peerGroup addPeerDiscovery new DnsDiscovery(params)
      peerGroup.setUserAgent(appName, "1.073")
      peerGroup setDownloadTxDependencies 0
      peerGroup setPingIntervalMsec 10000
      peerGroup setMaxConnections 10
      peerGroup addWallet wallet
      startDownload
      autoSaveOn
    }
  }
}

object Vibr {
  def vibrate(pattern: Pattern) = if (null != vib && vib.hasVibrator) vib.vibrate(pattern, -1)
  lazy val vib = app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
  val confirmed = Array(0L, 75, 250, 75, 250)
  val processed = Array(0L, 85, 200)
  type Pattern = Array[Long]

  val generalTracker = new WalletChangeEventListener
    with TransactionConfidenceEventListener with WalletCoinsReceivedEventListener with WalletCoinsSentEventListener {
    def onTransactionConfidenceChanged(w: Wallet, tx: Transaction) = if (tx.getConfidence.getDepthInBlocks == 1) vibrate(confirmed)
    def onCoinsReceived(w: Wallet, t: Transaction, pb: Coin, nb: Coin) = if (nb isGreaterThan pb) vibrate(processed)
    def onCoinsSent(w: Wallet, t: Transaction, pb: Coin, nb: Coin) = vibrate(processed)
    def onWalletChanged(w: Wallet) = none
  }
}