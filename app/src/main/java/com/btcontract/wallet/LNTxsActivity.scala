package com.btcontract.wallet

import Utils.{none, wrap, runAnd}
import android.content.{IntentFilter, Context, Intent, BroadcastReceiver}
import android.view.{MenuItem, Menu}
import android.widget.{SearchView, ListView}

import android.os.Bundle
import android.view.MenuItem.OnActionExpandListener
import android.widget.SearchView.OnQueryTextListener
import info.guardianproject.netcipher.proxy.OrbotHelper


class LNTxsActivity extends InfoActivity with SearchActivity { me =>
  lazy val lnList = findViewById(R.id.lnItemsList).asInstanceOf[ListView]

  private val torStatus = new BroadcastReceiver {
    val tor = new IntentFilter(OrbotHelper.ACTION_STATUS)
    override def onReceive(context: Context, intent: Intent) =
      updateStatus(intent getStringExtra OrbotHelper.EXTRA_STATUS)

    def register = registerReceiver(this, tor)
    def unregister = unregisterReceiver(this)

    def updateStatus(stat: String) = stat match {
      case OrbotHelper.STATUS_STARTING =>
      case OrbotHelper.STATUS_STOPPING =>
      case OrbotHelper.STATUS_ON =>
      case _ => // OFF
    }
  }

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    setContentView(R.layout.activity_ln_txs)
  }

  override def onResume = wrap(super.onResume)(torStatus.register)
  override def onPause = wrap(super.onPause)(torStatus.unregister)

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.ln_transactions_ops, menu)
    me activateMenu menu
    true
  }
}

trait SearchActivity {
  def activateMenu(menu: Menu) = try {
    val item = menu.findItem(R.id.search).getActionView
    val searchView = item.asInstanceOf[SearchView]

    // React to non empty search queries
    searchView setOnQueryTextListener new OnQueryTextListener {
      def onQueryTextChange(textQuery: String) = runAnd(true)(none)
      def onQueryTextSubmit(textQuery: String) = true
    }

    // Restore default view when search is closed if it was changed
    menu getItem 0 setOnActionExpandListener new OnActionExpandListener {
      def onMenuItemActionCollapse(menuItem: MenuItem) = runAnd(true)(none)
      def onMenuItemActionExpand(menuItem: MenuItem) = true
    }

    // Remove the bottom line from search plate, this may throw on some models
    val searchPlateId = searchView.getResources.getIdentifier("android:id/search_plate", null, null)
    searchView.findViewById(searchPlateId).setBackgroundResource(R.drawable.apptheme_search_view_holo)
  } catch none
}