package com.btcontract.wallet.utils

import android.content.{Context, Intent}
import com.btcontract.wallet.{ClassNames, R}
import android.app.{NotificationManager, PendingIntent, Service}
import androidx.core.app.NotificationCompat


object AwaitService {
  val awaitServiceClass: Class[AwaitService] = classOf[AwaitService]

  final val TITLE_TO_DISPLAY = "titleToDisplay"
  final val BODY_TO_DISPLAY = "bodyToDisplay"

  final val CHANNEL_ID = "awaitChannelId"
  final val NOTIFICATION_ID = 14

  final val ACTION_CANCEL = "actionCancel"
  final val ACTION_SHOW = "actionShow"
}

class AwaitService extends Service { me =>
  override def onBind(intent: Intent): Null = null

  override def onDestroy: Unit = {
    val srv = getSystemService(Context.NOTIFICATION_SERVICE)
    srv.asInstanceOf[NotificationManager].cancel(AwaitService.NOTIFICATION_ID)
    super.onDestroy
  }

  override def onStartCommand(serviceIntent: Intent, flags: Int, id: Int): Int = {
    processServiceIntent(serviceIntent)
    Service.START_NOT_STICKY
  }

  def processServiceIntent(intent: Intent): Unit =
    if (intent.getAction != AwaitService.ACTION_CANCEL) {
      val awaitedBodyText = intent.getStringExtra(AwaitService.BODY_TO_DISPLAY)
      val awaitedTitleText = intent.getStringExtra(AwaitService.TITLE_TO_DISPLAY)

      val disaplyIntent = PendingIntent.getActivity(me, 0, new Intent(me, ClassNames.mainActivityClass), 0)
      val cancelIntent = PendingIntent.getService(me, 0, new Intent(me, AwaitService.awaitServiceClass).setAction(AwaitService.ACTION_CANCEL), 0)

      val notification =
        new NotificationCompat.Builder(me, AwaitService.CHANNEL_ID).setContentTitle(awaitedTitleText).setContentText(awaitedBodyText)
          .addAction(android.R.drawable.ic_menu_close_clear_cancel, getResources.getString(R.string.dialog_cancel), cancelIntent)
          .setSmallIcon(R.drawable.ic_history_black_24dp).setContentIntent(disaplyIntent).build

      startForeground(AwaitService.NOTIFICATION_ID, notification)
    } else {
      stopForeground(true)
      stopSelf
    }
}
