package com.btcontract.wallet

import R.string._
import android.widget._
import Utils.{none, app}
import android.view.View.{VISIBLE, GONE}

import android.view.View


class TCEmailActivity extends TimerActivity with ViewSwitch { me =>
  lazy val tcIdentityEmailInput = findViewById(R.id.tcIdentityEmailInput).asInstanceOf[EditText]
  lazy val tcIdentityEmailNotYet = findViewById(R.id.tcIdentityEmailNotYet).asInstanceOf[TextView]
  lazy val tcIdentityEmailCopy = findViewById(R.id.tcIdentityEmailCopy).asInstanceOf[Button]
  lazy val tcIdentityEmail = findViewById(R.id.tcIdentityEmail).asInstanceOf[TextView]
  lazy val tcIdentityKey = findViewById(R.id.tcIdentityKey).asInstanceOf[TextView]

  lazy val views =
    findViewById(R.id.tcIdentityProgress).asInstanceOf[ImageView] ::
    findViewById(R.id.tcIdentityEmailNew).asInstanceOf[LinearLayout] ::
    findViewById(R.id.tcIdentityEmailConfirm).asInstanceOf[LinearLayout] ::
    findViewById(R.id.tcIdentityInfo).asInstanceOf[LinearLayout] :: Nil

  def setNewEmail(view: View) = none
  def shareIdentity(view: View) = none
  def goNewEmail(view: View) = setVis(GONE, VISIBLE, GONE, GONE)
  def goQRScan(view: View) = me goTo classOf[ScanActivity]
}