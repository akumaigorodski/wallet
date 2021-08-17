package com.btcontract.wallet

import android.os.Bundle
import android.view.View
import scala.util.Try


class EmergencyActivity extends BaseActivity {
  def INIT(state: Bundle): Unit = setContentView(R.layout.activity_emergency)

  def shareErrorReport(view: View): Unit = {
    val info = Try(getIntent getStringExtra UncaughtHandler.ERROR_REPORT)
    info.foreach(println)
    info.foreach(share)
  }
}
