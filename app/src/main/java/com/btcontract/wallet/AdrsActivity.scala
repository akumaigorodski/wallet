package com.btcontract.wallet

import android.widget.RadioGroup.OnCheckedChangeListener
import android.widget.{RadioGroup, ListView, TextView}
import org.bitcoinj.core.{Address, Coin}
import org.bitcoinj.crypto.DeterministicKey
import android.app.AlertDialog.Builder
import java.util.Collections
import android.os.Bundle
import Utils.wrap

import android.view.{MenuItem, Menu}
import R.string.err_general


class AdrsActivity extends InfoActivity { me =>
  def onFail(exc: Throwable): Unit = new Builder(me).setMessage(err_general).show
  lazy val head = getLayoutInflater.inflate(R.layout.frag_denom_and_count_head, null)
  lazy val adrsNum = head.findViewById(R.id.txsNumber).asInstanceOf[TextView]
  lazy val list = findViewById(R.id.itemsList).asInstanceOf[ListView]
  lazy val txsOpts = getResources getStringArray R.array.addr_total
  lazy val dc = new DenomControl(prefs, head)
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

      // Setup selector
      dc.radios check dc.nowMode
      dc.radios setOnCheckedChangeListener new OnCheckedChangeListener {
        def onCheckedChanged(r: RadioGroup, n: Int) = wrap(null)(dc.updMode)
      }

      // Wait for addresses and value amount map
      <(addressesAndValueMap, onFail) { case (keys, map) =>

      }

    } else this exitTo classOf[MainActivity]
  }

  // Activity lifecycle listeners management
  override def onOptionsItemSelected(mi: MenuItem) = mi.getItemId match { case act =>
    if (android.R.id.home == act) finish else decideActionToTake(act)
    super.onOptionsItemSelected(mi)
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.address_ops, menu)
    super.onCreateOptionsMenu(menu)
  }

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.wallet removeEventListener taskListener
  }

  def addressesAndValueMap = app.kit.wallet.getIssuedReceiveKeys match { case keys =>
    val adrToSum = collection.mutable.Map.empty[Address, Coin] withDefaultValue Coin.ZERO
    for (out <- app.kit.freshOuts) adrToSum(app.kit toAdr out) add out.getValue
    Collections.sort(keys, DeterministicKey.CHILDNUM_ORDER)
    (keys, adrToSum)
  }
}
