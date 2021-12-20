package com.btcontract.wallet

import android.content.Intent
import android.os.Bundle
import androidx.core.app.NotificationManagerCompat
import com.btcontract.wallet.R.string._
import immortan.LNParams
import immortan.crypto.Tools.runAnd
import immortan.utils.InputParser

import scala.util.{Failure, Success}


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
  override def onResume: Unit = runAnd(super.onResume) {
    val processIntent = (getIntent.getFlags & Intent.FLAG_ACTIVITY_LAUNCHED_FROM_HISTORY) == 0
    val dataOpt = Seq(getIntent.getDataString, getIntent getStringExtra Intent.EXTRA_TEXT).find(data => null != data)
    if (processIntent) runInFutureProcessOnUI(dataOpt.foreach(InputParser.recordValue), proceed)(proceed) else proceed(null)
  }

  override def START(state: Bundle): Unit = {
    setContentView(R.layout.frag_linear_layout)
    NotificationManagerCompat.from(me).cancelAll
  }

  def proceed(empty: Any): Unit = WalletApp.isAlive match {
    case false => runAnd(WalletApp.makeAlive)(me proceed null)
    case true if LNParams.isOperational => if (WalletApp.useAuth) new EnsureAuth(ToHub).makeAttempt else ToHub.makeAttempt
    case true => if (WalletApp.useAuth) new EnsureAuth(EnsureSeed).makeAttempt else EnsureSeed.makeAttempt
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

  object EnsureSeed extends Step {
    def makeAttempt: Unit = WalletApp.extDataBag.tryGetSecret match {
      case Failure(_: android.database.CursorIndexOutOfBoundsException) =>
        // Record is not present at all, this is probaby a fresh wallet
        me exitTo classOf[SetupActivity]

      case Failure(reason) =>
        // Notify user about it
        throw reason

      case Success(secret) =>
        WalletApp.makeOperational(secret)
        ToHub.makeAttempt
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
}
