package com.btcontract.wallet

import android.text.Html
import org.bitcoinj.core._

import android.widget.RadioGroup.OnCheckedChangeListener
import scala.collection.JavaConverters.asScalaBufferConverter
import android.widget.{BaseAdapter, RadioGroup, ListView, TextView}
import android.app.AlertDialog.Builder
import android.os.Bundle
import Utils.{ wrap, denom, humanAddr, Outputs }

import android.view.{ViewGroup, View}
import R.string.{txs_sum_in, err_general}

import scala.collection.mutable


class AdrsActivity extends TimerActivity { me =>
  def onFail(exc: Throwable): Unit = new Builder(me).setMessage(err_general).show
  lazy val head = getLayoutInflater.inflate(R.layout.frag_denom_and_count_head, null)
  lazy val adrsNum = head.findViewById(R.id.txsNumber).asInstanceOf[TextView]
  lazy val list = findViewById(R.id.itemsList).asInstanceOf[ListView]
  lazy val adrOpts = getResources getStringArray R.array.addr_total
  implicit lazy val dc = new DenomControl(prefs, head)
  lazy val sumIn = me getString txs_sum_in
  var bag: DataBag = null

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_list_view)

    val adapter = new BaseAdapter {
      def getView(adrPosition: Int, oldView: View, parent: ViewGroup) = {
        val view = if (null == oldView) getLayoutInflater.inflate(R.layout.frag_address, null) else oldView
        val hold = if (null == view.getTag) new AdrViewHolder(view) else view.getTag.asInstanceOf[AdrViewHolder]
        hold fillView getItem(adrPosition)
        view
      }

      def getItem(pos: Int) = bag addresses pos
      def getItemId(position: Int) = position
      def getCount = bag.addresses.size
    }

    // Setup selector
    dc.radios check dc.nowMode
    dc.radios setOnCheckedChangeListener new OnCheckedChangeListener {
      def onCheckedChanged(r: RadioGroup, n: Int) = wrap(adapter.notifyDataSetChanged)(dc.updMode)
    }

    // Wait for wallet data
    <(updateData, onFail) { case dataWasUpdated =>
      adrsNum setText app.plurOrZero(adrOpts, adapter.getCount)
      list.addHeaderView(head, null, true)
      list setAdapter adapter
    }
  }

  // View holder

  class AdrViewHolder(view: View) {
    val address = view.findViewById(R.id.address).asInstanceOf[TextView]
    val amount = view.findViewById(R.id.amount).asInstanceOf[TextView]
    view setTag this

    def fillView(adr: Address) = {
      val human = sumIn format humanAddr(adr)
      amount setText denom(bag valueMap adr)
      address setText Html.fromHtml(human)
    }
  }

  abstract class DataBag {
    val addresses: mutable.Buffer[Address]
    val valueMap: Map[Address, Coin]
    app.kit.currentAddress
  }

  def updateData = bag = new DataBag {
    def coinSum(outs: Outputs) = (Coin.ZERO /: outs)(_ add _.getValue)
    val valueMap = app.kit.freshOuts groupBy app.kit.toAdr mapValues coinSum withDefaultValue Coin.ZERO
    val addresses = app.kit.wallet.getIssuedReceiveAddresses.asScala union valueMap.keys.toList
  }
}