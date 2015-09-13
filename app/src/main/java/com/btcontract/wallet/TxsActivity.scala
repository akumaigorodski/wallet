package com.btcontract.wallet

import org.bitcoinj.core.TransactionConfidence
import android.widget.RadioGroup.OnCheckedChangeListener
import android.widget.AbsListView.OnScrollListener
import collection.JavaConversions.asScalaBuffer
import android.view.View.OnClickListener
import android.app.AlertDialog.Builder
import android.text.format.DateUtils
import java.text.SimpleDateFormat
import scala.collection.mutable
import android.os.Bundle
import android.text.Html

import R.string.{txs_received_to, txs_sent_to, err_tx_load}
import R.string.{txs_sum_in, txs_sum_out, dialog_cancel}
import Utils.{humanAddr, wrap, fmt, Outputs, baseSat}
import android.view.{View, ViewGroup, Menu}
import scala.util.{Success, Try}

import OnScrollListener.SCROLL_STATE_IDLE
import DateUtils.FORMAT_ABBREV_ALL
import org.bitcoinj.core._
import android.widget._


class TxsActivity extends InfoActivity { me =>
  def getCache = (cache.getOrElseUpdate _).curried
  def onFail(exc: Throwable): Unit = new Builder(me).setMessage(err_tx_load).show
  lazy val head = getLayoutInflater.inflate(R.layout.frag_transaction_head, null)
  lazy val txsNum = head.findViewById(R.id.txsNumber).asInstanceOf[TextView]
  lazy val list = findViewById(R.id.txslist).asInstanceOf[ListView]
  lazy val txsOpts = getResources getStringArray R.array.txs_total
  lazy val addrUnknown = getString(R.string.txs_noaddr)
  lazy val yesFee = getString(R.string.txs_yes_fee)
  lazy val noFee = getString(R.string.txs_no_fee)
  lazy val dc = new DenomControl(prefs, head)

  val sdf = new SimpleDateFormat("MMM dd yyyy EEE HH:mm:ss")
  val cache = mutable.Map.empty[Sha256Hash, TxCache]
  var adapter: TxsListAdapter = null
  var useBtcDenom = true

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
  override def onCreate(savedInstState: Bundle) =
  {
    super.onCreate(savedInstState)
    getActionBar setDisplayHomeAsUpEnabled true

    if (app.isAlive) {
      add(constantListener.mkTxt, Informer.PEERS).ui.run
      new Anim(app.kit.currentBalance, Utils.appName)
      setContentView(R.layout.activity_txs)

      // Setup adapter
      adapter = if (scrWidth < 2.0) new TxsListAdapter(new TxViewHolder(_), R.layout.frag_transaction_small)
        else if (scrWidth < 2.6) new TxsListAdapter(new TxViewHolderNormal(_), R.layout.frag_transaction_normal)
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
      useBtcDenom = dc.nowIsBtc
      dc.radios check dc.nowMode
      dc.radios setOnCheckedChangeListener new OnCheckedChangeListener {
        def onCheckedChanged(radioGroup: RadioGroup, checkedButton: Int) =
        {
          dc.updMode
          useBtcDenom = dc.nowIsBtc
          adapter.notifyDataSetChanged
        }
      }

      // Wait for transactions list
      <(app.kit.wallet.getTransactionsByTime, onFail) { result =>
        txsNum setText app.plurOrZero(txsOpts, result.size)
        app.kit.wallet addEventListener txsListListener
        list.addHeaderView(head, null, true)
        adapter.transactions = result
        list.setAdapter(adapter)

        // pos - 1 because header is present in list
        list setOnItemClickListener new AdapterView.OnItemClickListener {
          def onItemClick(par: AdapterView[_], v: View, pos: Int, id: Long) = {
            val form = getLayoutInflater.inflate(R.layout.frag_transaction_details, null)
            val details = form.findViewById(R.id.txDetails).asInstanceOf[TextView]
            val copyHash = form.findViewById(R.id.copyHash).asInstanceOf[Button]
            val transaction = adapter getItem pos - 1
            val hash = transaction.getHash
            val entry = cache(hash)

            val sum = transaction.getConfidence.getConfidenceType match {
              case TransactionConfidence.ConfidenceType.DEAD => app getString R.string.txs_dead
              case _ => s"${entry.transactionAmount}<br><small>${sdf format transaction.getUpdateTime}</small>"
            }

            val res = if (entry.value.isPositive) R.string.txs_inc else R.string.txs_out
            val rawDetails = getString(res).format(Utils humanAddr hash.toString, entry.prettyAddress)
            copyHash setOnClickListener new OnClickListener { def onClick(v: View) = app setBuffer hash.toString }
            mkForm(me negBld dialog_cancel, Html fromHtml sum, form)
            details.setText(Html fromHtml rawDetails)
          }
        }
      }

      // Wire up listeners
      app.kit.peerGroup addEventListener new CatchUpTracker
      app.kit.peerGroup addEventListener constantListener
      app.kit.wallet addEventListener taskListener
    } else this exitTo classOf[MainActivity]
  }

  // Activity lifecycle

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

  class TxsListAdapter(makeHolder: View => TxViewHolder, resId: Int) extends BaseAdapter {
    def getItemId(txPosition: Int) = getItem(txPosition).getHash.getBytes match { case bytes =>
      bytes(31).&(0xFFl) | bytes(30).&(0xFFl) << 8 | bytes(29).&(0xFFl) << 16 | bytes(28).&(0xFFl) << 24 |
        bytes(27).&(0xFFl) << 32 | bytes(26).&(0xFFl) << 40 | bytes(25).&(0xFFl) << 48 | bytes(23).&(0xFFl) << 56
    }

    var transactions: java.util.List[Transaction] = null
    def getItem(pos: Int) = transactions get pos
    def getCount = transactions.size

    def getView(pos: Int, convertView: View, parent: ViewGroup) = {
      val view = if (null == convertView) getLayoutInflater.inflate(resId, null) else convertView
      val hold = if (null == view.getTag) makeHolder(view) else view.getTag.asInstanceOf[TxViewHolder]
      hold fillView getItem(pos)
      view
    }
  }

  // Tx details converters

  def confMap(num: Int) = num match {
    case 0 => R.drawable.conf0 case 1 => R.drawable.conf1
    case 2 => R.drawable.conf2 case 3 => R.drawable.conf3
    case 4 => R.drawable.conf4 case _ => R.drawable.conf5
  }

  def when(now: Long, ago: Long) =
    if (now - ago < 345600000) DateUtils.getRelativeTimeSpanString(ago, now, 0)
    else if (now - ago < 15 * 86400 * 1000000L) DateUtils.formatDateTime(app, ago, FORMAT_ABBREV_ALL)
    else app getString R.string.txs_no_time

  def denom(sum: Coin) = if (useBtcDenom) fmt(sum.getValue) else baseSat format sum.getValue
  def feeString(fee: Coin) = if (fee == null || fee.isZero) noFee else yesFee format denom(fee)

  def ga(outs: Outputs, outputType: Boolean): Option[String] = Try {
    val currentOutputIsGoodOne = outs.head.isMine(app.kit.wallet) == outputType
    if (currentOutputIsGoodOne) outs.head.getScriptPubKey.getToAddress(app.params, true)
  } match {
    case Success(adr: Address) => Some(adr.toString)
    case _ if outs.tail.nonEmpty => ga(outs.tail, outputType)
    case _ => None
  }

  // Transaction cache item

  case class TxCache(adr: Option[String], value: Coin) {
    val prettyAddress = adr map humanAddr getOrElse addrUnknown
    val sum = getString { if (value.isPositive) txs_sum_in else txs_sum_out }
    val sentOrGot = getString { if (value.isPositive) txs_received_to else txs_sent_to }
    val htmlAddress = Html.fromHtml(sentOrGot format prettyAddress)
    def transactionAmount = sum format denom(value)
  }

  // View holders

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

    def fillView(tx: Transaction) = {
      val entry = getCache(tx.getHash) {
        val sum = tx.getValue(app.kit.wallet)
        val addrOpt = ga(tx.getOutputs, sum.isPositive)
        TxCache(addrOpt, sum)
      }

      transactWhen setText when(System.currentTimeMillis, tx.getUpdateTime.getTime)
      transactSum setText Html.fromHtml(entry.transactionAmount)
      feeAmount setText feeString(tx.getFee)
      address setText entry.htmlAddress
      status(tx.getConfidence)
    }
  }

  class TxViewHolderNormal(view: View) extends TxViewHolder(view) {
    val status = view.findViewById(R.id.transactStatus).asInstanceOf[TextView]
    val confOpts = getResources getStringArray R.array.txs_normal_conf
    val whenTransactionIsDead = R.string.txs_normal_dead

    override def status(tc: TransactionConfidence) = {
      val confirmationStatus = tc.getConfidenceType match {
        case TransactionConfidence.ConfidenceType.DEAD => app getString whenTransactionIsDead
        case _ => if (tc.getDepthInBlocks > 999) "999+" else app.plurOrZero(confOpts, tc.getDepthInBlocks)
      }

      // Text status and then also a circle
      status setText confirmationStatus
      super.status(tc)
    }
  }
}