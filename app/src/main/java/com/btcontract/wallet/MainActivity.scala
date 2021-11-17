package com.btcontract.wallet

import java.io.{File, FileInputStream}

import android.content.Intent
import android.os.Bundle
import android.view.View
import android.widget.TextView
import androidx.appcompat.app.AlertDialog
import androidx.core.app.NotificationManagerCompat
import com.btcontract.wallet.BaseActivity.StringOps
import com.btcontract.wallet.R.string._
import com.ornach.nobobutton.NoboButton
import fr.acinq.bitcoin.Satoshi
import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.eclair.blockchain.electrum.db.SigningWallet
import immortan.LNParams
import immortan.crypto.Tools.{none, runAnd}
import immortan.utils.InputParser
import info.guardianproject.netcipher.proxy.OrbotHelper
import org.bitcoinj.wallet._
import org.ndeftools.Message
import org.ndeftools.util.activity.NfcReaderActivity

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}


object ClassNames {
  val chanActivityClass: Class[ChanActivity] = classOf[ChanActivity]
  val statActivityClass: Class[StatActivity] = classOf[StatActivity]
  val qrSplitActivityClass: Class[QRSplitActivity] = classOf[QRSplitActivity]
  val qrChainActivityClass: Class[QRChainActivity] = classOf[QRChainActivity]
  val qrInvoiceActivityClass: Class[QRInvoiceActivity] = classOf[QRInvoiceActivity]
  val coinControlActivityClass: Class[CoinControlActivity] = classOf[CoinControlActivity]

  val settingsActivityClass: Class[SettingsActivity] = classOf[SettingsActivity]
  val remotePeerActivityClass: Class[RemotePeerActivity] = classOf[RemotePeerActivity]
  val mainActivityClass: Class[MainActivity] = classOf[MainActivity]
  val hubActivityClass: Class[HubActivity] = classOf[HubActivity]
}

class MainActivity extends NfcReaderActivity with BaseActivity { me =>
  lazy val legacyWalletFile = new File(getFilesDir, "Bitcoin.wallet")

  def INIT(state: Bundle): Unit = {
    // We may enter an app by tapping a notification
    NotificationManagerCompat.from(me).cancelAll
    initNfc(state)
  }

  lazy val (skipOrbotCheck, takeOrbotAction, mainOrbotMessage, mainOrbotIssues) = {
    // This saves some time by not drawing a view in case if we can proceed to wallet
    
    setContentView(R.layout.activity_main)
    val skipOrbotCheck1 = findViewById(R.id.skipOrbotCheck).asInstanceOf[NoboButton]
    val takeOrbotAction1 = findViewById(R.id.takeOrbotAction).asInstanceOf[NoboButton]
    val mainOrbotMessage1 = findViewById(R.id.mainOrbotMessage).asInstanceOf[TextView]
    val mainOrbotIssues1 = findViewById(R.id.mainOrbotIssues).asInstanceOf[View]
    (skipOrbotCheck1, takeOrbotAction1, mainOrbotMessage1, mainOrbotIssues1)
  }

  // NFC AND SHARE

  // This method is always run when `onResume` event is fired, should be a starting point for all subsequent checks
  def readNdefMessage(msg: Message): Unit = runInFutureProcessOnUI(InputParser recordValue ndefMessageString(msg), proceed)(proceed)

  override def onNoNfcIntentFound: Unit = {
    val processIntent = (getIntent.getFlags & Intent.FLAG_ACTIVITY_LAUNCHED_FROM_HISTORY) == 0
    val dataOpt = Seq(getIntent.getDataString, getIntent getStringExtra Intent.EXTRA_TEXT).find(data => null != data)
    if (processIntent) runInFutureProcessOnUI(dataOpt foreach InputParser.recordValue, proceed)(proceed) else proceed(null)
  }

  def onNfcStateEnabled: Unit = none
  def onNfcStateDisabled: Unit = none
  def onNfcFeatureNotFound: Unit = none
  def onNfcStateChange(ok: Boolean): Unit = none
  def readEmptyNdefMessage: Unit = proceed(null)
  def readNonNdefMessage: Unit = proceed(null)

  def proceed(empty: Any): Unit = WalletApp.isAlive match {
    case false => runAnd(WalletApp.makeAlive)(me proceed null)
    case true if LNParams.isOperational => me exitTo ClassNames.hubActivityClass

    case true =>
      val step3 = if (legacyWalletFile.exists) new EnsureLegacy else new EnsureSeed
      val step2 = if (WalletApp.ensureTor) new EnsureTor(step3) else step3
      val step1 = if (WalletApp.useAuth) new EnsureAuth(step2) else step2
      step1.makeAttempt
  }

  // Tor and auth

  trait Step {
    def makeAttempt: Unit
  }

  class EnsureSeed extends Step {
    def makeAttempt: Unit = WalletApp.extDataBag.tryGetSecret match {
      case Failure(_: android.database.CursorIndexOutOfBoundsException) =>
        // Record is not present at all, this is probaby a fresh wallet
        me exitTo classOf[SetupActivity]

      case Failure(reason) =>
        // Notify user about it
        throw reason

      case Success(secret) =>
        WalletApp.makeOperational(secret)
        me exitTo ClassNames.hubActivityClass
    }
  }

  class EnsureLegacy extends Step {
    private def restoreLegacyWallet = {
      val stream = new FileInputStream(legacyWalletFile)
      val proto: Protos.Wallet = try WalletProtobufSerializer.parseToProto(stream) finally stream.close
      (new WalletProtobufSerializer).readWallet(org.bitcoinj.params.MainNetParams.get, null, proto)
    }

    private def decrypt(wallet: Wallet, pass: String) = Try {
      val scrypt: org.bitcoinj.crypto.KeyCrypter = wallet.getKeyCrypter
      wallet.getKeyChainSeed.decrypt(scrypt, pass, scrypt deriveKey pass)
    }

    def makeAttempt: Unit = {
      val (container, extraInputLayout, extraInput) = singleInputPopup
      val builder = titleBodyAsViewBuilder(title = null, body = container)
      mkCheckForm(proceed, none, builder, dialog_ok, dialog_cancel)
      extraInputLayout.setHint(password)
      showKeys(extraInput)

      def proceed(alert: AlertDialog): Unit = runAnd(alert.dismiss) {
        val core = SigningWallet(walletType = BIP32, isRemovable = true)
        decrypt(restoreLegacyWallet, extraInput.getText.toString) map { seed =>
          SetupActivity.fromMnemonics(seed.getMnemonicCode.asScala.toList, host = me)
          val wallet = LNParams.chainWallets.makeSigningWalletParts(core, Satoshi(0L), core.walletType)
          LNParams.chainWallets = LNParams.chainWallets.withFreshWallet(wallet)
          me exitTo ClassNames.hubActivityClass
          legacyWalletFile.delete
        } getOrElse makeAttempt
      }
    }
  }

  class EnsureAuth(next: Step) extends Step {
    def makeAttempt: Unit = new utils.BiometricAuth(findViewById(R.id.mainLayout), me) {
      def onHardwareUnavailable: Unit = WalletApp.app.quickToast(settings_auth_not_available)
      def onNoHardware: Unit = WalletApp.app.quickToast(settings_auth_no_support)

      def onNoneEnrolled: Unit = {
        // Settings flag is on but user has removed all fingerprints from system
        WalletApp.app.prefs.edit.putBoolean(WalletApp.USE_AUTH, false).commit
        next.makeAttempt
      }

      def onCanAuthenticate: Unit = callAuthDialog
      def onAuthSucceeded: Unit = next.makeAttempt
    }.checkAuth
  }

  class EnsureTor(next: Step) extends Step {
    val orbotHelper: OrbotHelper = OrbotHelper.get(me)
    def makeAttempt: Unit = WalletApp.app.checkTorProxy(showIssue)(next.makeAttempt)

    private def showIssue: Unit = UITask {
      skipOrbotCheck setOnClickListener onButtonTap(proceedAnyway)
      takeOrbotAction setOnClickListener onButtonTap(closeAppOpenOrbot)
      mainOrbotMessage setText getString(orbot_err_unclear).html
      mainOrbotIssues setVisibility View.VISIBLE
    }.run

    def closeAppOpenOrbot: Unit = {
      val pack = OrbotHelper.ORBOT_PACKAGE_NAME
      val intent = getPackageManager getLaunchIntentForPackage pack
      Option(intent).foreach(startActivity)
      finishAffinity
      System exit 0
    }

    def proceedAnyway: Unit = {
      // We must disable Tor check because disconnect later will bring us here again
      WalletApp.app.prefs.edit.putBoolean(WalletApp.ENSURE_TOR, false).commit
      next.makeAttempt
    }
  }
}
