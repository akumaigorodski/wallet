package com.btcontract.wallet

import Utils._
import R.string._
import android.widget._
import org.bitcoinj.core._
import org.bitcoinj.wallet.listeners._

import android.view.{View, ViewGroup}
import R.drawable.{dead, await, conf1}
import android.provider.Settings.{System => FontSystem}
import org.bitcoinj.core.listeners.TransactionConfidenceEventListener
import android.text.format.DateUtils.getRelativeTimeSpanString
import android.widget.AbsListView.OnScrollListener
import collection.JavaConversions.asScalaBuffer
import android.view.View.OnClickListener
import android.app.AlertDialog.Builder
import android.text.format.DateFormat
import org.bitcoinj.uri.BitcoinURI
import java.text.SimpleDateFormat
import org.bitcoinj.wallet.Wallet
import android.graphics.Typeface
import scala.collection.mutable
import android.content.Intent
import scala.util.Success
import android.os.Bundle
import android.text.Html
import android.net.Uri

import TransactionConfidence.ConfidenceType.DEAD
import OnScrollListener.SCROLL_STATE_IDLE


trait HumanTimeDisplay { me: TimerActivity =>
  lazy val time = new SimpleDateFormat(timeString).format(_: java.util.Date)
  lazy val bigFont = FontSystem.getFloat(getContentResolver, FontSystem.FONT_SCALE, 1) > 1

  // Should be accessed after activity is initialized
  lazy val timeString = DateFormat is24HourFormat me match {
    case false if scrWidth < 2.2 => "MM/dd/yy' <font color=#999999>'h:mm'<small>'a'</small></font>'"
    case false if scrWidth < 2.7 & bigFont => "MM/dd/yy' <font color=#999999>'h:mm'<small>'a'</small></font>'"
    case false if scrWidth < 2.7 => "MM/dd/yy' <font color=#999999>'h:mm'<small>'a'</small></font>'"
    case false if bigFont => "MMM dd, yyyy' <font color=#999999>'h:mm'<small>'a'</small></font>'"
    case false => "MMMM dd, yyyy' <font color=#999999>'h:mm'<small>'a'</small></font>'"

    case true if scrWidth < 2.2 => "d MMM yyyy' <font color=#999999>'HH:mm'</font>'"
    case true if scrWidth < 2.7 & bigFont => "d MMM yyyy' <font color=#999999>'HH:mm'</font>'"
    case true if scrWidth < 2.7 => "d MMM yyyy' <font color=#999999>'HH:mm'</font>'"
    case true if bigFont => "d MMM yyyy' <font color=#999999>'HH:mm'</font>'"
    case true => "d MMMM yyyy' <font color=#999999>'HH:mm'</font>'"
  }

  // Relative or absolute date
  def when(now: Long, dat: java.util.Date) = dat.getTime match { case ago =>
    if (now - ago < 129600000) getRelativeTimeSpanString(ago, now, 0).toString else time(dat)
  }
}

class TxsActivity extends InfoActivity with HumanTimeDisplay { me =>
  def onFail(e: Throwable): Unit = new Builder(me).setMessage(err_general).show
  lazy val adapter = new TxsListAdapter(R.layout.frag_transaction_line)
  lazy val all = getLayoutInflater.inflate(R.layout.frag_txs_all, null)
  lazy val list = findViewById(R.id.itemsList).asInstanceOf[ListView]
  lazy val txsConfs = getResources getStringArray R.array.txs_confs
  lazy val feeIncoming = getString(txs_fee_incoming)
  lazy val feeDetails = getString(txs_fee_details)
  lazy val feeAbsent = getString(txs_fee_absent)

  private[this] val transactionsTracker = new WalletChangeEventListener with WalletReorganizeEventListener
    with TransactionConfidenceEventListener with WalletCoinsReceivedEventListener with WalletCoinsSentEventListener {
    def onTransactionConfidenceChanged(w: Wallet, tx: Transaction) = if (tx.getConfidence.getDepthInBlocks < 2) onReorganize(w)
    def onCoinsReceived(w: Wallet, tx: Transaction, pb: Coin, nb: Coin) = if (nb isGreaterThan pb) me runOnUiThread tell(tx)
    def onCoinsSent(w: Wallet, tx: Transaction, pb: Coin, nb: Coin) = me runOnUiThread tell(tx)
    def onReorganize(w: Wallet) = me runOnUiThread adapter.notifyDataSetChanged
    def onWalletChanged(w: Wallet) = none
  }

  def tell(freshTransaction: Transaction) = {
    adapter.transactions.add(0, freshTransaction)
    adapter.notifyDataSetChanged
  }

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    val linesNum = if (scrHeight < 4.8) 3 else 5

    if (app.isAlive) {
      add(constListener.mkTxt, Informer.PEERS).ui.run
      new Anim(app.kit.currentBalance, appName)
      setContentView(R.layout.activity_txs)

      list setOnItemClickListener onTap { case position =>
        val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
        val detailsWrapper = getLayoutInflater.inflate(R.layout.frag_transaction_details, null)
        val confNumber = detailsWrapper.findViewById(R.id.confNumber).asInstanceOf[TextView]
        val outside = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]
        val txPlus = new TxPlus(adapter getItem position)
        val hash = txPlus.tx.getHash.toString

        // Compute required variables
        val totalSum = s"${txPlus.humanValue}<br><small>${me time txPlus.tx.getUpdateTime}</small>"
        val site = new Intent(Intent.ACTION_VIEW, Uri parse s"https://blockexplorer.com/tx/$hash")
        val pays = getPays(txPlus.tx.getOutputs, mutable.Buffer.empty, txPlus.value.isPositive)
        val txt = for (payment <- pays) yield Html.fromHtml(payment pretty txPlus.route)

        // Wire everything up
        lst addHeaderView detailsWrapper
        lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.actionTip, txt.toArray)
        lst setOnItemClickListener onTap { position => app setBuffer pays(position - 1).adr.toString }
        outside setOnClickListener new OnClickListener { def onClick(v: View) = me startActivity site }
        mkForm(me negBld dialog_back, Html fromHtml totalSum, lst)
        confNumber setText Html.fromHtml(txPlus.status)
      }

      // Wait for transactions list
      <(app.kit.wallet.getTransactionsByTime, onFail) { result =>
        app.kit.wallet addReorganizeEventListener transactionsTracker
        app.kit.wallet addTransactionConfidenceEventListener transactionsTracker
        app.kit.wallet addCoinsReceivedEventListener transactionsTracker
        app.kit.wallet addCoinsSentEventListener transactionsTracker

        // Show limited txs list
        val range = scala.math.min(linesNum, result.size)
        if (range < result.size) list addFooterView all
        adapter.transactions = result.subList(0, range)
        list setAdapter adapter
      }

      // Periodic list update
      list setOnScrollListener new OnScrollListener {
        timer.schedule(me anyToRunnable go, 10000, 10000)
        def onScroll(v: AbsListView, first: Int, visible: Int, total: Int) = none
        def onScrollStateChanged(v: AbsListView, newState: Int) = state = newState
        def go = if (SCROLL_STATE_IDLE == state) adapter.notifyDataSetChanged
        var state = SCROLL_STATE_IDLE
      }

      // Wire up general listeners
      app.kit.wallet addTransactionConfidenceEventListener tracker
      app.kit.wallet addCoinsReceivedEventListener tracker
      app.kit.wallet addCoinsSentEventListener tracker

      app.kit.peerGroup addBlocksDownloadedEventListener new CatchTracker
      app.kit.peerGroup addDisconnectedEventListener constListener
      app.kit.peerGroup addConnectedEventListener constListener
    } else me exitTo classOf[MainActivity]
  }

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.wallet removeReorganizeEventListener transactionsTracker
    app.kit.wallet removeTransactionConfidenceEventListener transactionsTracker
    app.kit.wallet removeCoinsReceivedEventListener transactionsTracker
    app.kit.wallet removeCoinsSentEventListener transactionsTracker
    app.kit.wallet removeTransactionConfidenceEventListener tracker
    app.kit.wallet removeCoinsReceivedEventListener tracker
    app.kit.wallet removeCoinsSentEventListener tracker

    app.kit.peerGroup removeDisconnectedEventListener constListener
    app.kit.peerGroup removeConnectedEventListener constListener
  }

  override def onResume = {
    app.TransData.value match {
      case Some(addr: Address) => wrap(doPay(null) setAddressValue addr)(app.TransData.value = None)
      case Some(uri: BitcoinURI) => wrap(doPay(null) set uri)(app.TransData.value = None)
      case Some(app.TransData.THUNDERSECRET :: _) => me exitTo classOf[TCEmailActivity]
      case Some(app.TransData.LIGHTNING :: _) => me exitTo classOf[LNTxsActivity]
      case _ => // Incompatible data, just do nothing
    }

    super.onResume
  }

  def showAll(v: View) = {
    list removeFooterView all
    <(app.kit.wallet.getTransactionsByTime, onFail) { result =>
      wrap(adapter.notifyDataSetChanged)(adapter.transactions = result)
    }
  }

  // Adapter for transactions list
  class TxsListAdapter(id: Int) extends BaseAdapter {
    def getView(transactionPosition: Int, convertView: View, parent: ViewGroup) = {
      val view = if (null == convertView) getLayoutInflater.inflate(id, null) else convertView
      val hold = if (null == view.getTag) new TxViewHolder(view) else view.getTag.asInstanceOf[TxViewHolder]
      hold fillView getItem(transactionPosition)
      view
    }

    var transactions: java.util.List[Transaction] = null
    def getItem(position: Int) = transactions get position
    def getItemId(position: Int) = position
    def getCount = transactions.size
  }

  def getPays(outs: mutable.Buffer[TransactionOutput], acc: mutable.Buffer[PayData], way: Boolean) = {
    def add(output: TransactionOutput) = acc += PayData(app getTo output, Success apply output.getValue)
    for (output <- outs if output.isMine(app.kit.wallet) == way) try add(output) catch none
    acc
  }

  class TxPlus(val tx: Transaction) {
    def humanValue = route format btc(value)
    lazy val value = tx getValue app.kit.wallet
    def route = if (value.isPositive) sumIn else sumOut
    def confs = app.plurOrZero(txsConfs, tx.getConfidence.getDepthInBlocks)
    def status = if (value.isPositive) feeIncoming format confs else tx.getFee match {
      case null => feeAbsent format confs case fee => feeDetails.format(Utils btc fee, confs)
    }
  }

  class TxViewHolder(view: View) {
    def fillView(transaction: Transaction) = {
      val isConf = transaction.getConfidence.getDepthInBlocks > 0
      val isDead = transaction.getConfidence.getConfidenceType == DEAD
      val time = Html fromHtml when(System.currentTimeMillis, transaction.getUpdateTime)
      transactCircle setImageResource { if (isDead) dead else if (isConf) conf1 else await }
      transactSum setText Html.fromHtml(new TxPlus(transaction).humanValue)
      transactSum setTypeface Typeface.MONOSPACE
      transactWhen setText time
    }

    val transactCircle = view.findViewById(R.id.transactCircle).asInstanceOf[ImageView]
    val transactWhen = view.findViewById(R.id.transactWhen).asInstanceOf[TextView]
    val transactSum = view.findViewById(R.id.transactSum).asInstanceOf[TextView]
    view setTag this
  }
}