package com.btcontract.wallet

import android.widget.RadioGroup.OnCheckedChangeListener
import android.app.AlertDialog.Builder
import android.os.Bundle
import android.text.Html

import android.widget.{TextView, ListView, BaseAdapter, RadioGroup, AdapterView, ArrayAdapter}
import org.bitcoinj.core.{AbstractWalletEventListener, Wallet, Transaction, Coin, Address}
import Utils.{wrap, denom, Outputs, app, sumIn, humanAddr}
import R.string.{err_general, no_funds, dialog_cancel}
import collection.JavaConversions._
import android.view._


class AdrsActivity extends TimerActivity { me =>
  def onFail(exc: Throwable): Unit = new Builder(me).setMessage(err_general).show
  override def onDestroy = wrap(super.onDestroy)(app.kit.wallet removeEventListener addressListener)
  lazy val head = getLayoutInflater.inflate(R.layout.frag_denom_and_count_head, null)
  lazy val adrsNum = head.findViewById(R.id.txsNumber).asInstanceOf[TextView]
  lazy val list = findViewById(R.id.itemsList).asInstanceOf[ListView]

  // Implicits for denom
  implicit lazy val dc = new DenomControl(me, head)
  implicit lazy val noFunds = getString(no_funds)

  // Human number of addresses, list of dialog options and caches
  lazy val adrOpts = getResources getStringArray R.array.dialog_address
  lazy val adrNum = getResources getStringArray R.array.addr_total
  var bag = List.empty[AdrCache]

  val adapter = new BaseAdapter {
    def getView(pos: Int, oldView: View, parent: ViewGroup) = {
      val view = if (null == oldView) getLayoutInflater.inflate(R.layout.frag_address, null) else oldView
      val hold = if (null == view.getTag) new AdrViewHolder(view) else view.getTag.asInstanceOf[AdrViewHolder]
      hold fillView getItem(pos)
      view
    }

    def getItemId(position: Int) = position
    def getItem(pos: Int) = bag(pos)
    def getCount = bag.size
  }

  val addressListener = new AbstractWalletEventListener {
    override def onCoinsReceived(w: Wallet, tx: Transaction, pb: Coin, nb: Coin) = if (nb isGreaterThan pb) update
    override def onCoinsSent(w: Wallet, tx: Transaction, pb: Coin, nb: Coin) = update

    def update = <(updateData, onFail) { case ok =>
      adrsNum setText app.plurOrZero(adrNum, adapter.getCount)
      adapter.notifyDataSetChanged
    }
  }

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_adrs)
    app.kit.wallet addEventListener addressListener
    // Generate fresh address if there is none yet
    app.kit.currentAddress

    // Setup selector
    dc.radios check dc.nowMode
    dc.radios setOnCheckedChangeListener new OnCheckedChangeListener {
      def onCheckedChanged(r: RadioGroup, n: Int) = wrap(adapter.notifyDataSetChanged)(dc.update)
    }

    // pos - 1 because header is present
    list setOnItemClickListener new AdapterView.OnItemClickListener {
      def onItemClick(par: AdapterView[_], v: View, pos: Int, id: Long) = bag(pos - 1) match { case item =>
        val listCon = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
        val arrAdapter = new ArrayAdapter(me, R.layout.frag_center_text, R.id.textItem, adrOpts)
        val dialog = mkForm(me negBld dialog_cancel, Html fromHtml item.humanAddrWay, listCon)

        listCon setOnItemClickListener new AdapterView.OnItemClickListener {
          def onItemClick(par: AdapterView[_], view: View, pos: Int, id: Long) =
            rm(dialog) _ apply choose(pos, item.address.toString)
        }

        // Wire everything up
        app.TransData.value = Option(item)
        listCon setAdapter arrAdapter
      }
    }

    // Wait for wallet data
    <(updateData, onFail) { case ok =>
      adrsNum setText app.plurOrZero(adrNum, adapter.getCount)
      list.addHeaderView(head, null, true)
      list setAdapter adapter
    }
  }

  // View holder

  class AdrViewHolder(view: View) {
    val addressText = view.findViewById(R.id.address).asInstanceOf[TextView]
    val amountText = view.findViewById(R.id.amount).asInstanceOf[TextView]
    view setTag this

    def fillView(cache: AdrCache) = {
      addressText setText Html.fromHtml(cache.humanAddrWay)
      amountText setText denom(cache.amount)
    }
  }

  def updateData = bag = {
    def summate(outs: Outputs) = (Coin.ZERO /: outs)(_ add _.getValue)
    val withFunds = app.kit.freshOuts groupBy app.kit.toAdr mapValues summate
    val issued = app.kit.wallet.getIssuedReceiveAddresses.map(_ -> Coin.ZERO).toMap
    issued ++ withFunds map AdrCache.tupled
  }.toList
}

case class AdrCache(address: Address, amount: Coin) {
  lazy val humanAddrWay = sumIn format humanAddr(address)
}