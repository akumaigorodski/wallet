package com.btcontract.wallet

import android.content.Intent
import android.text.Html
import org.bitcoinj.core._

import android.widget.RadioGroup.OnCheckedChangeListener
import scala.collection.JavaConverters.asScalaBufferConverter
import android.widget._
import android.app.AlertDialog.Builder
import android.os.Bundle
import Utils.{ wrap, denom, humanAddr, Outputs }

import android.view.{ViewGroup, View}
import R.string.{txs_sum_in, err_general, no_funds, dialog_cancel}

import collection.JavaConversions._
import scala.collection.mutable
import scala.util.Failure


class AdrsActivity extends TimerActivity { me =>
  def onFail(exc: Throwable): Unit = new Builder(me).setMessage(err_general).show
  lazy val head = getLayoutInflater.inflate(R.layout.frag_denom_and_count_head, null)
  lazy val adrsNum = head.findViewById(R.id.txsNumber).asInstanceOf[TextView]
  lazy val list = findViewById(R.id.itemsList).asInstanceOf[ListView]
  lazy val sumIn = me getString txs_sum_in

  // Implicits for denom
  implicit lazy val dc = new DenomControl(prefs, head)
  implicit lazy val noFunds = me getString no_funds

  // Human number of addresses, list of dialog options and caches
  lazy val adrOpts = getResources getStringArray R.array.dialog_address
  lazy val adrNum = getResources getStringArray R.array.addr_total
  var cache = mutable.Map.empty[Address, AdrCache]
  var bag: DataBag = null

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_list_view)

    val adapter = new BaseAdapter {
      def getView(pos: Int, oldView: View, parent: ViewGroup) = getItem(pos) match { case address =>
        val view = if (null == oldView) getLayoutInflater.inflate(R.layout.frag_address, null) else oldView
        val hold = if (null == view.getTag) new AdrViewHolder(view) else view.getTag.asInstanceOf[AdrViewHolder]
        hold fillView cache.getOrElseUpdate(address, AdrCache apply address)
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

    // pos - 1 because header is present
    list setOnItemClickListener new AdapterView.OnItemClickListener {
      def onItemClick(par: AdapterView[_], v: View, pos: Int, id: Long) =
      {
        val item = me cache adapter.getItem(pos - 1)
        val listCon = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
        val arrAdapter = new ArrayAdapter(me, R.layout.frag_center_text, R.id.textItem, adrOpts)
        val dialog = mkForm(me negBld dialog_cancel, Html fromHtml item.human, listCon)

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
    <(updateData, onFail) { case dataWasUpdated =>
      adrsNum setText app.plurOrZero(adrNum, adapter.getCount)
      list.addHeaderView(head, null, true)
      list setAdapter adapter
    }
  }

  // Cache class

  case class AdrCache(address: Address) {
    val human = sumIn format humanAddr(address)
  }

  // View holder

  class AdrViewHolder(view: View) {
    val addressText = view.findViewById(R.id.address).asInstanceOf[TextView]
    val amount = view.findViewById(R.id.amount).asInstanceOf[TextView]
    view setTag this

    def fillView(cache: AdrCache) = {
      amount setText denom(bag valueMap cache.address)
      addressText setText Html.fromHtml(cache.human)
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
    val addresses = (app.kit.wallet.getIssuedReceiveAddresses ++ valueMap.keys).distinct
  }
}