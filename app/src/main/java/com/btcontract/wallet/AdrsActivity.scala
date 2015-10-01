package com.btcontract.wallet

import org.bitcoinj.crypto.DeterministicKey
import android.app.AlertDialog.Builder
import java.util.Collections
import android.os.Bundle
import Utils.wrap

import android.view.{MenuItem, Menu}
import R.string.err_general


class AdrsActivity extends InfoActivity { me =>
  def onFail(exc: Throwable): Unit = new Builder(me).setMessage(err_general).show
  val taskListener = new WalletListener

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    getActionBar setDisplayHomeAsUpEnabled true

    if (app.isAlive) {
      add(constantListener.mkTxt, Informer.PEERS).ui.run
      new Anim(app.kit.currentBalance, Utils.appName)
      app.kit.wallet addEventListener taskListener
      setContentView(R.layout.activity_list_view)

      <(allAddresses, onFail) { adrs =>

      }

    } else this exitTo classOf[MainActivity]
  }

  // Activity lifecycle listeners management
  override def onOptionsItemSelected(mi: MenuItem) = mi.getItemId match { case act =>
    if (android.R.id.home == act) finish else decideActionToTake(act)
    super.onOptionsItemSelected(mi)
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.transactions_ops, menu)
    super.onCreateOptionsMenu(menu)
  }

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.wallet removeEventListener taskListener
  }

  def allAddresses = {
    val keys = app.kit.wallet.getIssuedReceiveKeys
    Collections.sort(keys, DeterministicKey.CHILDNUM_ORDER)
    keys
  }
}
