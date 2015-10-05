package com.btcontract.wallet

import android.widget.RadioGroup.OnCheckedChangeListener
import android.widget.AbsListView.OnScrollListener
import collection.JavaConversions.asScalaBuffer
import org.bitcoinj.core.TransactionConfidence
import android.view.View.OnClickListener
import android.app.AlertDialog.Builder
import android.text.format.DateUtils
import scala.collection.mutable
import android.content.Intent
import scala.util.Success
import android.os.Bundle
import android.text.Html
import android.net.Uri
import java.util.Date

import R.string.{txs_received_to, txs_sent_to, txs_many_received_to, txs_many_sent_to, err_general}
import R.string.{txs_sum_in, txs_sum_out, txs_yes_fee, txs_no_fee, txs_noaddr, dialog_ok}
import Utils.{humanAddr, wrap, denom, Outputs, PayDatas, none}
import android.view.{View, ViewGroup, Menu}

import OnScrollListener.SCROLL_STATE_IDLE
import org.bitcoinj.core._
import android.widget._


class TxsActivity extends InfoActivity { me =>
  def onFail(exc: Throwable): Unit = new Builder(me).setMessage(err_general).show
  lazy val head = getLayoutInflater.inflate(R.layout.frag_denom_and_count_head, null)
  lazy val txsNum = head.findViewById(R.id.txsNumber).asInstanceOf[TextView]
  lazy val list = findViewById(R.id.itemsList).asInstanceOf[ListView]
  implicit lazy val dc = new DenomControl(prefs, head)

  // Confirmation rings and total number of transactions
  lazy val confOpts = getResources getStringArray R.array.txs_normal_conf
  lazy val txsOpts = getResources getStringArray R.array.txs_total

  // Sent/received templates and fee
  lazy val rcvdManyTo = me getString txs_many_received_to
  lazy val sentManyTo = me getString txs_many_sent_to
  lazy val rcvdTo = me getString txs_received_to
  lazy val addrUnknown = me getString txs_noaddr
  lazy val sentTo = me getString txs_sent_to
  lazy val yesFee = me getString txs_yes_fee
  lazy val sumOut = me getString txs_sum_out
  lazy val sumIn = me getString txs_sum_in
  lazy val noFee = me getString txs_no_fee

  // Local state
  var cache = mutable.Map.empty[Sha256Hash, TxCache]
  var adapter: TxsListAdapter = null

  val taskListener = new WalletListener
  val txsListListener = new AbstractWalletEventListener {
    override def onReorganize(w: Wallet) = me runOnUiThread adapter.notifyDataSetChanged
    override def onTransactionConfidenceChanged(w: Wallet, tx: Transaction) = if (tx.getConfidence.getDepthInBlocks < 6) onReorganize(w)
    override def onCoinsReceived(w: Wallet, tx: Transaction, pb: Coin, nb: Coin) = if (nb isGreaterThan pb) me runOnUiThread add(tx)
    override def onCoinsSent(w: Wallet, tx: Transaction, pb: Coin, nb: Coin) = me runOnUiThread add(tx)

    def add(freshTransaction: Transaction) = {
      adapter.transactions.add(0, freshTransaction)
      txsNum setText app.plurOrZero(txsOpts, adapter.getCount)
      adapter.notifyDataSetChanged
    }
  }

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    getActionBar setDisplayHomeAsUpEnabled true

    if (app.isAlive) {
      add(constantListener.mkTxt, Informer.PEERS).ui.run
      new Anim(app.kit.currentBalance, Utils.appName)
      setContentView(R.layout.activity_list_view)

      // Setup adapter here because scrWidth is not available on start
      adapter = if (scrWidth < 2.0) new TxsListAdapter(new TxViewHolder(_), R.layout.frag_transaction_small)
        else if (scrWidth < 3.2) new TxsListAdapter(new TxViewHolderNormal(_), R.layout.frag_transaction_normal)
        else if (scrWidth < 4.4) new TxsListAdapter(new TxViewHolder(_), R.layout.frag_transaction_large)
        else new TxsListAdapter(new TxViewHolderNormal(_), R.layout.frag_transaction_extra)

      // Periodic list update
      list setOnScrollListener new ScrollListener {
        timer.schedule(me anyToRunnable go, 10000, 10000)
        def onScrollStateChanged(v: AbsListView, newState: Int) = state = newState
        def go = if (SCROLL_STATE_IDLE == state) adapter.notifyDataSetChanged
        var state = SCROLL_STATE_IDLE
      }

      // Setup selector
      dc.radios check dc.nowMode
      dc.radios setOnCheckedChangeListener new OnCheckedChangeListener {
        def onCheckedChanged(r: RadioGroup, n: Int) = wrap(adapter.notifyDataSetChanged)(dc.updMode)
      }

      // Wait for transactions list
      <(app.kit.wallet.getTransactionsByTime, onFail) { result =>
        txsNum setText app.plurOrZero(txsOpts, result.size)
        // txsListListener uses adapter which may be null
        app.kit.wallet addEventListener txsListListener
        list.addHeaderView(head, null, true)
        adapter.transactions = result
        list setAdapter adapter

        // pos - 1 because header is present
        list setOnItemClickListener new AdapterView.OnItemClickListener {
          def onItemClick(par: AdapterView[_], v: View, pos: Int, id: Long) = {
            val detailsForm = getLayoutInflater.inflate(R.layout.frag_transaction_details, null)
            val listCon = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
            val outside = detailsForm.findViewById(R.id.viewTxOutside).asInstanceOf[TextView]
            val copy = detailsForm.findViewById(R.id.copyTxHash).asInstanceOf[Button]
            val transaction = adapter getItem pos - 1
            val hash = transaction.getHash
            val entry = cache(hash)

            // Take special pain to inform if transaction is dead
            val totalSum = transaction.getConfidence.getConfidenceType match {
              case TransactionConfidence.ConfidenceType.DEAD => app getString R.string.txs_dead
              case _ => s"${entry.transactAmount}<br><small>${me time transaction.getUpdateTime}</small>"
            }

            outside setOnClickListener new OnClickListener {
              def site = Uri parse s"https://blockexplorer.com/tx/$hash"
              def onClick(v: View) = me startActivity new Intent(Intent.ACTION_VIEW, site)
            }

            copy setOnClickListener new OnClickListener {
              def onClick(v: View) = app setBuffer hash.toString
            }

            // Prepare data and wire everything up
            val color = if (entry.value.isPositive) "#1BA2E0" else "#E31300"
            val txt = for (payment <- entry.pays) yield Html.fromHtml(payment text color)
            listCon setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.actionTip, txt.toArray)
            mkForm(me negBld dialog_ok, Html fromHtml totalSum, listCon)
            listCon addHeaderView detailsForm
          }
        }
      }

      // Wire up listeners
      app.kit.peerGroup addEventListener new CatchUpTracker
      app.kit.peerGroup addEventListener constantListener
      app.kit.wallet addEventListener taskListener
    } else this exitTo classOf[MainActivity]
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.transactions_ops, menu)
    super.onCreateOptionsMenu(menu)
  }

  override def onResume = wrap(super.onResume) {
    prefs.edit.putBoolean(AbstractKit.SACK_OR_TXS, false).commit
  }

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.peerGroup removeEventListener constantListener
    app.kit.wallet removeEventListener txsListListener
    app.kit.wallet removeEventListener taskListener
  }

  // Adapter

  class TxsListAdapter(mk: View => TxViewHolder, id: Int) extends BaseAdapter {
    def getView(transactionPosition: Int, convertView: View, parent: ViewGroup) = {
      val view = if (null == convertView) getLayoutInflater.inflate(id, null) else convertView
      val hold = if (null == view.getTag) mk(view) else view.getTag.asInstanceOf[TxViewHolder]
      hold fillView getItem(transactionPosition)
      view
    }

    var transactions: java.util.List[Transaction] = null
    def getItem(pos: Int) = transactions get pos
    def getItemId(txnPos: Int) = txnPos
    def getCount = transactions.size
  }

  // Tx details converters

  def confMap(num: Int) = num match {
    case 0 => R.drawable.conf0 case 1 => R.drawable.conf1
    case 2 => R.drawable.conf2 case 3 => R.drawable.conf3
    case 4 => R.drawable.conf4 case _ => R.drawable.conf5
  }

  def when(date: Date, now: Long) = date.getTime match { case ago =>
    if (now - ago < 60480000) DateUtils.getRelativeTimeSpanString(ago, now, 0)
    else Html fromHtml time(date)
  }

  def time(date: Date) = String.format("%1$tb %1$te&#x200b;,&#160;%1$tY&#x200b;,&#160;%1$tR", date)
  def feeString(fee: Coin) = if (null == fee || fee.isZero) noFee else yesFee format denom(fee)

  def summary(outs: PayDatas, incoming: Boolean) = outs match {
    case mutable.Buffer(pay, _, _*) if incoming => rcvdManyTo.format(humanAddr(pay.adr), outs.size - 1)
    case mutable.Buffer(pay, _, _*) => sentManyTo.format(humanAddr(pay.adr), outs.size - 1)
    case mutable.Buffer(pay) if incoming => rcvdTo format humanAddr(pay.adr)
    case mutable.Buffer(pay) => sentTo format humanAddr(pay.adr)
    case _ if incoming => rcvdTo format addrUnknown
    case _ => sentTo format addrUnknown
  }

  def getOuts(outs: Outputs, acc: PayDatas, way: Boolean) = {
    for (out <- outs if out.isMine(app.kit.wallet) == way) try {
      acc += PayData(app.kit toAdr out, Success apply out.getValue)
    } catch none
    acc
  }

  def makeCache(txs: Transaction) = txs getValue app.kit.wallet match { case sum =>
    val goodOuts = getOuts(txs.getOutputs, mutable.Buffer.empty, sum.isPositive)
    TxCache(goodOuts, txs.getFee, sum)
  }

  // Transaction cache item and view holders

  case class TxCache(pays: PayDatas, fee: Coin, value: Coin) {
    val htmlSummary = Html fromHtml summary(pays, value.isPositive)
    val sumDirection = if (value.isPositive) sumIn else sumOut
    def transactAmount = sumDirection format denom(value)
  }

  class TxViewHolder(view: View) {
    val transactCircle = view.findViewById(R.id.transactCircle).asInstanceOf[ImageView]
    val transactWhen = view.findViewById(R.id.transactWhen).asInstanceOf[TextView]
    val transactSum = view.findViewById(R.id.transactSum).asInstanceOf[TextView]
    val feeAmount = view.findViewById(R.id.feeAmount).asInstanceOf[TextView]
    val address = view.findViewById(R.id.address).asInstanceOf[TextView]
    view setTag this

    def status(tc: TransactionConfidence) = tc.getConfidenceType match {
      case TransactionConfidence.ConfidenceType.DEAD => transactCircle setImageResource R.drawable.dead
      case anyOtherConfidenceType => transactCircle setImageResource confMap(tc.getDepthInBlocks)
    }

    def fillView(txn: Transaction) = {
      val entry = cache.getOrElseUpdate(txn.getHash, me makeCache txn)
      transactWhen setText when(txn.getUpdateTime, System.currentTimeMillis)
      transactSum setText Html.fromHtml(entry.transactAmount)
      feeAmount setText feeString(entry.fee)
      address setText entry.htmlSummary
      status(txn.getConfidence)
    }
  }

  class TxViewHolderNormal(view: View) extends TxViewHolder(view) {
    val status = view.findViewById(R.id.transactStatus).asInstanceOf[TextView]

    override def status(tc: TransactionConfidence) = {
      val confirmationStatus = tc.getConfidenceType match {
        case TransactionConfidence.ConfidenceType.DEAD => me getString R.string.txs_normal_dead
        case _ => if (tc.getDepthInBlocks > 999) "999+" else app.plurOrZero(confOpts, tc.getDepthInBlocks)
      }

      // Text status and then also a circle
      status setText confirmationStatus
      super.status(tc)
    }
  }
}