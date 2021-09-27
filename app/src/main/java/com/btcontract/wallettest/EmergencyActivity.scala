package com.btcontract.wallettest

import android.os.Bundle
import android.view.View
import scala.util.Try


class EmergencyActivity extends BaseActivity {
  def INIT(state: Bundle): Unit = setContentView(R.layout.activity_emergency)

  def shareErrorReport(view: View): Unit = Try(getIntent getStringExtra UncaughtHandler.ERROR_REPORT).foreach(share)
}
