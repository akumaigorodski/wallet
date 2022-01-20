package com.btcontract.wallet

import android.content.Intent
import android.os.Bundle
import androidx.core.app.NotificationManagerCompat
import com.guardanis.applock.activities.{LockCreationActivity, UnlockActivity}
import immortan.LNParams
import immortan.crypto.Tools.runAnd
import immortan.utils.InputParser

import scala.util.{Failure, Success}


object ClassNames {
  val lockCreationClass: Class[LockCreationActivity] = classOf[LockCreationActivity]
  val unlockActivityClass: Class[UnlockActivity] = classOf[UnlockActivity]

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

    case true if LNParams.isOperational =>
      me exitTo ClassNames.hubActivityClass

    case true =>
      WalletApp.extDataBag.tryGetSecret match {
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
}
