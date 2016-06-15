package com.btcontract.wallet

import R.string._
import android.widget._
import Utils.app


class TCEmailActivity extends TimerActivity with ViewSwitch { me =>

  lazy val views =
    findViewById(R.id.tcIdentityProgress).asInstanceOf[ImageView] ::
    findViewById(R.id.tcIdentityEmailNew).asInstanceOf[LinearLayout] ::
    findViewById(R.id.tcIdentityEmailConfirm).asInstanceOf[LinearLayout] ::
    findViewById(R.id.tcIdentityInfo).asInstanceOf[LinearLayout] :: Nil
}