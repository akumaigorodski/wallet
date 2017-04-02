package com.btcontract.wallet

import java.io.{PrintWriter, StringWriter}
import java.lang.Thread.UncaughtExceptionHandler
import android.content.Intent
import android.app.Activity


class UncaughtHandler(ctxt: Activity)
extends UncaughtExceptionHandler { me =>

  def uncaughtException(thread: Thread, exc: Throwable): Unit = {
    val emerge: Class[EmergencyActivity] = classOf[EmergencyActivity]
    val stackTrace: StringWriter = new StringWriter
    val intent = new Intent(ctxt, emerge)

    exc printStackTrace new PrintWriter(stackTrace)
    intent.putExtra("report", stackTrace.toString)
    ctxt startActivity intent

    android.os.Process killProcess android.os.Process.myPid
    System exit 10
  }
}
