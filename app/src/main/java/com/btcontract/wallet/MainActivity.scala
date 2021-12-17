package com.btcontract.wallet

import java.io.{File, FileInputStream}

import android.content.Intent
import android.os.Bundle
import androidx.appcompat.app.AlertDialog
import androidx.core.app.NotificationManagerCompat
import com.btcontract.wallet.R.string._
import fr.acinq.bitcoin.Satoshi
import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.eclair.blockchain.electrum.db.SigningWallet
import immortan.LNParams
import immortan.crypto.Tools.{none, runAnd}
import immortan.utils.InputParser
import org.bitcoinj.wallet._

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

class MainActivity extends BaseActivity { me =>
  lazy val legacyWalletFile = new File(getFilesDir, "Bitcoin.wallet")

  override def START(state: Bundle): Unit = {
    setContentView(R.layout.frag_linear_layout)
    NotificationManagerCompat.from(me).cancelAll
  }

  override def onResume: Unit = runAnd(super.onResume) {
    val processIntent = (getIntent.getFlags & Intent.FLAG_ACTIVITY_LAUNCHED_FROM_HISTORY) == 0
    val dataOpt = Seq(getIntent.getDataString, getIntent getStringExtra Intent.EXTRA_TEXT).find(data => null != data)
    if (processIntent) runInFutureProcessOnUI(dataOpt.foreach(InputParser.recordValue), proceed)(proceed) else proceed(null)
  }

  def proceed(empty: Any): Unit = WalletApp.isAlive match {
    case false => runAnd(WalletApp.makeAlive)(me proceed null)

    case true if LNParams.isOperational =>
      if (WalletApp.useAuth) new EnsureAuth(ToHub).makeAttempt
      else ToHub.makeAttempt

    case true =>
      val step2 = if (legacyWalletFile.exists) new EnsureLegacy(ToHub) else new EnsureSeed(ToHub)
      if (WalletApp.useAuth) new EnsureAuth(step2).makeAttempt else step2.makeAttempt
  }

  // Tor and auth

  trait Step {
    def makeAttempt: Unit
  }

  object ToHub extends Step {
    def makeAttempt: Unit = {
      // Make sure auth won't be asked for again
      WalletApp.userSentAppToBackground = false
      me exitTo ClassNames.hubActivityClass
    }
  }

  class EnsureAuth(next: Step) extends Step {
    def makeAttempt: Unit = new utils.BiometricAuth(findViewById(R.id.linearLayout), me, _ => finish) {
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

  class EnsureSeed(next: Step) extends Step {
    def makeAttempt: Unit = WalletApp.extDataBag.tryGetSecret match {
      case Failure(_: android.database.CursorIndexOutOfBoundsException) =>
        // Record is not present at all, this is probaby a fresh wallet
        me exitTo classOf[SetupActivity]

      case Failure(reason) =>
        // Notify user about it
        throw reason

      case Success(secret) =>
        WalletApp.makeOperational(secret)
        next.makeAttempt
    }
  }

  class EnsureLegacy(next: Step) extends Step {
    private def decrypt(wallet: Wallet, pass: String) = Try {
      val scrypt: org.bitcoinj.crypto.KeyCrypter = wallet.getKeyCrypter
      wallet.getKeyChainSeed.decrypt(scrypt, pass, scrypt deriveKey pass)
    }

    private def restoreLegacyWallet = {
      val stream = new FileInputStream(legacyWalletFile)
      val proto: Protos.Wallet = try WalletProtobufSerializer.parseToProto(stream) finally stream.close
      (new WalletProtobufSerializer).readWallet(org.bitcoinj.params.MainNetParams.get, null, proto)
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
          runAnd(next.makeAttempt)(legacyWalletFile.delete)
        } getOrElse makeAttempt
      }
    }
  }
}
