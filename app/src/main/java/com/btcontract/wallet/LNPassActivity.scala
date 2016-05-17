package com.btcontract.wallet

import R.string._
import android.widget.{EditText, Button}
import android.widget.{ImageView, LinearLayout}
import com.btcontract.wallet.lightning.LNSeed
import android.os.Bundle
import android.view.View


class LNPassActivity extends TimerActivity with ViewSwitch { me =>
  lazy val passData = findViewById(R.id.passData).asInstanceOf[EditText]
  lazy val checkPassword = findViewById(R.id.checkPass).asInstanceOf[Button]
  lazy val views = findViewById(R.id.passProgress).asInstanceOf[ImageView] ::
    findViewById(R.id.passAuthorize).asInstanceOf[LinearLayout] :: Nil

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    setContentView(R.layout.activity_ln_pass)
    checkPassword setOnClickListener new View.OnClickListener {
      def check = LNSeed.setSeed(Mnemonic decrypt passData.getText.toString)
      def proceed = me exitTo classOf[TxsActivity]

      def onClick(view: View) = hideKeys {
        <(check, _ => wrongPass)(_ => proceed)
        setVis(View.VISIBLE, View.GONE)
      }
    }

    def wrongPass = {
      // Incorrect password notification
      setVis(View.GONE, View.VISIBLE)
      me toast password_wrong
    }
  }
}