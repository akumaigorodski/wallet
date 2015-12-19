package com.btcontract.wallet

import android.widget.RadioGroup.OnCheckedChangeListener
import android.app.AlertDialog.Builder
import android.os.Bundle
import android.text.Html
import util.Success

import android.widget.{TextView, ListView, BaseAdapter, RadioGroup, AdapterView, ArrayAdapter}
import org.bitcoinj.core.{AbstractWalletEventListener, Wallet, Transaction, Coin}
import Utils.{wrap, denom, Outputs, app, sumIn, nullFail}
import R.string.{err_general, no_funds, dialog_cancel}
import R.layout.{frag_address_small, frag_address_big}

import collection.JavaConversions._
import android.view._


class AdrsActivity extends TimerActivity { me =>
  def onFail(exc: Throwable): Unit = new Builder(me).setMessage(err_general).show
  override def onDestroy = wrap(super.onDestroy)(app.kit.wallet removeEventListener addressListener)
  private[this] lazy val head = getLayoutInflater.inflate(R.layout.frag_denom_and_count_head, null)
  private[this] lazy val adrTotal = head.findViewById(R.id.txsNumber).asInstanceOf[TextView]
  private[this] lazy val list = findViewById(R.id.itemsList).asInstanceOf[ListView]

  // Implicits for denom
  private[this] implicit lazy val dc = new DenomControl(me, head)
  private[this] implicit lazy val noFunds = getString(no_funds)

  // Human number of addresses, list of dialog options and caches
  private[this] lazy val itemView = if (scrWidth < 3.5) frag_address_small else frag_address_big
  private[this] lazy val adrOpts = getResources getStringArray R.array.dialog_address
  private[this] lazy val adrNum = getResources getStringArray R.array.addr_total
  private[this] var bag = List.empty[PayData]

  val adapter = new BaseAdapter {
    def getView(pos: Int, oldView: View, parent: ViewGroup) = {
      val view = if (null == oldView) getLayoutInflater.inflate(itemView, null) else oldView
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
      adrTotal setText app.plurOrZero(adrNum, adapter.getCount)
      adapter.notifyDataSetChanged
    }
  }

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_adrs)
    app.kit.wallet addEventListener addressListener
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
        val dialog = mkForm(negBld(dialog_cancel), Html.fromHtml(item route sumIn), listCon)

        listCon setAdapter arrAdapter
        app.TransData.value = Option apply item.copy(tc = nullFail)
        listCon setOnItemClickListener new AdapterView.OnItemClickListener {
          def onItemClick(par: AdapterView[_], view: View, pos: Int, id: Long) =
            rm(dialog) _ apply choose(pos, item.adr.toString)
        }
      }
    }

    // Wait for wallet data
    <(updateData, onFail) { case ok =>
      adrTotal setText app.plurOrZero(adrNum, bag.size)
      list.addHeaderView(head, null, true)
      list setAdapter adapter
    }
  }

  // View holder

  class AdrViewHolder(view: View) {
    val addressText = view.findViewById(R.id.address).asInstanceOf[TextView]
    val amountText = view.findViewById(R.id.amount).asInstanceOf[TextView]
    view setTag this

    def fillView(item: PayData) = {
      addressText setText Html.fromHtml(item route sumIn)
      amountText setText(item.tc map denom getOrElse noFunds)
    }
  }

  def updateData = bag = {
    def summate(outs: Outputs) = Success apply (Coin.ZERO /: outs)(_ add _.getValue)
    val issued = app.kit.wallet.getIssuedReceiveAddresses map (adr => adr -> nullFail)
    val withFunds = app.kit.freshOuts groupBy app.kit.toAdr mapValues summate
    issued.toMap ++ withFunds map PayData.tupled
  }.toList
}