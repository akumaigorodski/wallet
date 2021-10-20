package com.btcontract.wallet.utils

import androidx.work._
import android.content.{Context, Intent}
import android.app.{Notification, PendingIntent}
import androidx.core.app.{NotificationCompat, NotificationManagerCompat}
import com.btcontract.wallet.ClassNames
import java.util.concurrent.TimeUnit
import fr.acinq.eclair.secureRandom


object DelayedNotification {
  final val WATCH_TOWER_TAG = "watchTower"
  final val IN_FLIGHT_HTLC_TAG = "inFlightHtlc"
  final val CHANNEL_ID = "delayedNotificationChannelId1"

  final val WATCH_TOWER_PERIOD_MSEC = 12 * 24 * 3600 * 1000L // Every 12 days
  final val IN_FLIGHT_HTLC_PERIOD_MSEC = 6 * 3600 * 1000L // Every 6 hours

  def schedule(context: Context, tag: String, title: String, body: String, delayMsecs: Long): Operation = {
    val constraintBuilder = (new Constraints.Builder).setTriggerContentMaxDelay(1, TimeUnit.MILLISECONDS).build
    val dataBuilder = (new Data.Builder).putString("title", title).putString("body", body)
    val targetClass = classOf[NotificationSchedule]

    val builder = new OneTimeWorkRequest.Builder(targetClass)
      .setInitialDelay(delayMsecs, TimeUnit.MILLISECONDS)
      .setConstraints(constraintBuilder)
      .setInputData(dataBuilder.build)
      .addTag(tag)
      .build

    WorkManager.getInstance(context).enqueue(builder)
  }

  def cancel(context: Context, tag: String): Unit = WorkManager.getInstance(context).cancelAllWorkByTag(tag)

  class NotificationSchedule(context: Context, params: WorkerParameters) extends Worker(context, params) {
    private def setNotification(context: Context, notificationTitle: String, notificationBody: String): Unit = {
      val disaplyIntent = PendingIntent.getActivity(context, 0, new Intent(context, ClassNames.mainActivityClass), 0)
      val notificationId = secureRandom.nextInt(1000000)

      val notificationBuilder = new NotificationCompat.Builder(context, CHANNEL_ID)
        .setSmallIcon(com.btcontract.wallet.R.drawable.baseline_feedback_24)
        .setPriority(NotificationCompat.PRIORITY_HIGH)
        .setDefaults(Notification.DEFAULT_ALL)
        .setContentTitle(notificationTitle)
        .setContentText(notificationBody)
        .setContentIntent(disaplyIntent)

      NotificationManagerCompat.from(context).notify(notificationId, notificationBuilder.build)
    }

    override def doWork: ListenableWorker.Result = {
      val title = params.getInputData.getString("title")
      val body = params.getInputData.getString("body")
      setNotification(context, title, body)
      ListenableWorker.Result.success
    }
  }
}
