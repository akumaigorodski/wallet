package com.btcontract.wallet

import android.os.Bundle
import android.view.{View, ViewGroup}
import android.widget._
import androidx.cardview.widget.CardView
import androidx.recyclerview.widget.RecyclerView
import androidx.transition.TransitionManager
import com.btcontract.wallet.BaseActivity.StringOps
import com.btcontract.wallet.R.string._
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import fr.acinq.eclair.blockchain.electrum.{ElectrumEclairWallet, Utxo}
import immortan.crypto.Tools._
import immortan.utils.{Haiku, InputParser, WalletEventsCatcher, WalletEventsListener}
import immortan.{LNParams, TxDescription}


class CoinControlActivity extends BaseActivity with ExternalDataChecker { me =>
  lazy private[this] val coinControlContainer = findViewById(R.id.coinControlContainer).asInstanceOf[LinearLayout]
  lazy private[this] val utxoList = findViewById(R.id.utxoList).asInstanceOf[ListView]

  trait UtxoListItem
  case class TransactionLine(txid: String) extends UtxoListItem
  case class UnspentOutputLine(utxo: Utxo) extends UtxoListItem

  private[this] var walletPubKey: PublicKey = _
  private[this] var chooser: ChainWalletCards = _
  private[this] var txLabels: Map[String, TxDescription] = Map.empty
  private[this] var excludedOutPoints: Set[OutPoint] = Set.empty
  private[this] var items: List[UtxoListItem] = List.empty

  private val chainListener = new WalletEventsListener {
    override def onWalletReady(event: WalletReady): Unit = UITask {
      excludedOutPoints = event.excludedOutPoints.toSet
      updateItems(event.unExcludedUtxos)
      chanAdapter.notifyDataSetChanged
      updateWallet
    }.run
  }

  val chanAdapter: BaseAdapter = new BaseAdapter {
    override def getItem(pos: Int): UtxoListItem = items(pos)
    override def getItemId(position: Int): Long = position
    override def getCount: Int = items.size

    def getView(position: Int, savedView: View, parent: ViewGroup): View = {
      val view = if (null == savedView) getLayoutInflater.inflate(R.layout.frag_utxo_line, null) else savedView.asInstanceOf[View]
      val holder = if (null == view.getTag) new UtxoHolder(view) else view.getTag.asInstanceOf[UtxoHolder]

      getItem(position) match {
        case item: TransactionLine => holder.setTxView(item)
        case item: UnspentOutputLine => holder.setUtxoView(item)
      }

      view
    }
  }

  class UtxoHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) {
    val txLabelOrId: TextView = itemView.findViewById(R.id.txLabelOrId).asInstanceOf[TextView]
    val utxoWrap: RelativeLayout = itemView.findViewById(R.id.utxoWrap).asInstanceOf[RelativeLayout]
    val utxoCardContainer: CardView = itemView.findViewById(R.id.utxoCardContainer).asInstanceOf[CardView]

    val utxoIncluded: CheckBox = itemView.findViewById(R.id.utxoIncluded).asInstanceOf[CheckBox]
    val utxoHaikuName: TextView = itemView.findViewById(R.id.utxoHaikuName).asInstanceOf[TextView]
    val utxoAmount: TextView = itemView.findViewById(R.id.utxoAmount).asInstanceOf[TextView]
    itemView.setTag(this)

    def setTxView(item: TransactionLine): Unit = {
      setVisMany(true -> txLabelOrId, false -> utxoWrap)
      val labelOpt = txLabels.get(item.txid).flatMap(_.label)
      txLabelOrId setOnClickListener onButtonTap(WalletApp.app copy item.txid)
      txLabelOrId setText labelOpt.getOrElse(s"TXID ${item.txid.short}".html)
    }

    def setUtxoView(item: UnspentOutputLine): Unit = {
      setVisMany(false -> txLabelOrId, true -> utxoWrap)
      val humanAmount = HubActivity.incoming(item.utxo.item.value.sat.toMilliSatoshi)
      val isExcluded = excludedOutPoints.contains(item.utxo.item.outPoint)
      val utxoName = Haiku.name(item.utxo.key.publickeybytes)

      utxoCardContainer setOnClickListener onButtonTap {
        val excludedOutPoints1 = if (utxoIncluded.isChecked) excludedOutPoints + item.utxo.item.outPoint else excludedOutPoints - item.utxo.item.outPoint
        LNParams.chainWallets.findByPubKey(walletPubKey).foreach(_ provideExcludedOutpoints excludedOutPoints1.toList)
      }

      utxoAmount.setText(humanAmount.html)
      utxoIncluded.setChecked(!isExcluded)
      utxoHaikuName.setText(utxoName)
    }
  }

  def updateItems(unExcludedUtxos: Seq[Utxo] = Nil): Unit = {
    items = unExcludedUtxos.groupBy(_.item.outPoint.txid.toString).flatMap { case (txid, unspents) =>
      val outPointsLine = for (unspentOutput <- unspents) yield UnspentOutputLine(unspentOutput)
      TransactionLine(txid) +: outPointsLine
    }.toList
  }

  override def onDestroy: Unit = {
    val remove = WalletEventsCatcher.Remove(chainListener)
    try LNParams.chainWallets.catcher ! remove catch none
    super.onDestroy
  }

  def INIT(state: Bundle): Unit = {
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_coin_control)
      checkExternalData(noneRunnable)
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }
  }

  def updateWallet: Unit = {
    TransitionManager.beginDelayedTransition(chooser.holder)
    chooser.update(LNParams.chainWallets.findByPubKey(walletPubKey).toList)
    chooser.unPad(LNParams.chainWallets.findByPubKey(walletPubKey).toList)
  }

  def showWalletInfo(wallet: ElectrumEclairWallet): Unit = {
    val title = new TitleView(me getString coin_control)
    title.view.setOnClickListener(me onButtonTap finish)
    title.backArrow.setVisibility(View.VISIBLE)
    coinControlContainer.addView(title.view, 0)

    chooser = new ChainWalletCards(me) {
      override def onLabelTap(wallet: ElectrumEclairWallet): Unit = none
      override def onWalletTap(wallet: ElectrumEclairWallet): Unit = none
      override def onRemoveTap(wallet: ElectrumEclairWallet): Unit = none
      override def onCoinControlTap(wallet: ElectrumEclairWallet): Unit = none
      val holder: LinearLayout = findViewById(R.id.chainCardContainer).asInstanceOf[LinearLayout]
    }

    LNParams.chainWallets.catcher ! chainListener
    txLabels = WalletApp.txDataBag.listAllDescriptions
    walletPubKey = wallet.ewt.xPub.publicKey
    chooser.init(wallet :: Nil)
    updateWallet

    utxoList.setAdapter(chanAdapter)
    utxoList.setDividerHeight(0)
    utxoList.setDivider(null)

    runFutureProcessOnUI(wallet.getData, onFail) { ed =>
      for (walletReady <- ed.data.lastReadyMessage) {
        chainListener.onWalletReady(walletReady)
      }
    }
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case wallet: ElectrumEclairWallet if LNParams.chainWallets.findByPubKey(wallet.ewt.xPub.publicKey).isEmpty => finish
    case wallet: ElectrumEclairWallet => showWalletInfo(wallet)
    case _ => finish
  }
}
