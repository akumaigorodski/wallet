package com.btcontract.wallet

import R.string._
import android.widget.{EditText, Button}
import android.widget.{ImageView, LinearLayout}

import Utils.wrap
import android.os.Bundle
import android.view.View


class LNPassActivity extends TimerActivity with ViewSwitch { me =>
  lazy val passData = findViewById(R.id.passData).asInstanceOf[EditText]
  lazy val checkPass = findViewById(R.id.checkPass).asInstanceOf[Button]
  lazy val views = findViewById(R.id.passProgress).asInstanceOf[ImageView] ::
    findViewById(R.id.passAuthorize).asInstanceOf[LinearLayout] :: Nil

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    setContentView(R.layout.activity_ln_pass)
    checkPass setOnClickListener new View.OnClickListener {
      def wrong = wrap(me toast password_wrong)(showPasswordPromptView)
      def showPasswordPromptView = setVis(View.GONE, View.VISIBLE)
      def check = Mnemonic decrypt passData.getText.toString
      def lnProceed = me exitTo classOf[TxsActivity]

      def onClick(view: View) = hideKeys {
        <(check, _ => wrong)(_ => lnProceed)
        setVis(View.VISIBLE, View.GONE)
      }
    }
  }
}
