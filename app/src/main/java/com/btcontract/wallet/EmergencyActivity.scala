package com.btcontract.wallet

import java.io.FileInputStream

import R.string._
import com.btcontract.wallet.Utils._
import android.view.WindowManager.LayoutParams
import android.app.AlertDialog.Builder
import android.os.Bundle
import android.view.View
import org.bitcoinj.wallet.WalletProtobufSerializer

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Try}


class EmergencyActivity extends AnimatorActivity { me =>
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    add(me getString emerge_subtitle, Informer.PEERS).ui.run
    getActionBar.setTitle(me getString emerge_title)
    setContentView(R.layout.activity_emergency)
    prepareWallet
  }

  def emergeReport(v: View) = Try(getIntent getStringExtra "report") match {
    case Success(report: String) => negBld(dialog_ok).setMessage(report).show
    case _ => negBld(dialog_ok).setMessage(me getString err_general).show
  }

  def emergeMnemonic(v: View) = passPlus(none) { password =>
    <(Mnemonic decrypt password, wrong) { walletSeed =>
      getWindow.setFlags(LayoutParams.FLAG_SECURE, LayoutParams.FLAG_SECURE)
      val bld = new Builder(me) setCustomTitle getString(sets_noscreen)
      bld.setMessage(Mnemonic text walletSeed).show
    }
  }

  def wrong(err: Throwable) = {
    negBld(dialog_ok).setMessage(err.getMessage).show
    me toast password_wrong
  }

  def prepareWallet = Future {
    app.kit = new app.WalletKit {
      val stream = new FileInputStream(app.walletFile)
      val proto = try WalletProtobufSerializer parseToProto stream finally stream.close
      wallet = new WalletProtobufSerializer readWallet (app.params, null, proto)
      store = null
      blockChain = null
      peerGroup = null

      def startUp = none
    }
  }
}
