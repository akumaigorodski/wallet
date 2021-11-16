package com.btcontract.wallet

import android.os.Bundle
import android.view.{View, ViewGroup}
import android.widget.TextView
import androidx.appcompat.app.AlertDialog
import androidx.recyclerview.widget.RecyclerView
import com.azoft.carousellayoutmanager._
import com.btcontract.wallet.BaseActivity.StringOps
import com.btcontract.wallet.Colors._
import com.btcontract.wallet.R.string._
import com.ornach.nobobutton.NoboButton
import fr.acinq.bitcoin.Btc
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.EclairWallet.MAX_RECEIVE_ADDRESSES
import fr.acinq.eclair.blockchain.electrum.ElectrumEclairWallet
import immortan.LNParams
import immortan.crypto.Tools._
import immortan.utils.{BitcoinUri, Denomination, InputParser, PaymentRequestExt}

import scala.util.Success


class QRChainActivity extends QRActivity with ExternalDataChecker { me =>
  lazy private[this] val chainQrCaption = findViewById(R.id.chainQrCaption).asInstanceOf[TextView]
  lazy private[this] val chainQrCodes = findViewById(R.id.chainQrCodes).asInstanceOf[RecyclerView]
  lazy private[this] val chainQrMore = findViewById(R.id.chainQrMore).asInstanceOf[NoboButton]

  private[this] var wallet: ElectrumEclairWallet = _
  private[this] var allAddresses: List[BitcoinUri] = Nil
  private[this] var addresses: List[BitcoinUri] = Nil

  val adapter: RecyclerView.Adapter[QRViewHolder] = new RecyclerView.Adapter[QRViewHolder] {
    override def onBindViewHolder(holder: QRViewHolder, pos: Int): Unit = updateView(addresses(pos), holder)
    override def getItemId(itemPosition: Int): Long = itemPosition
    override def getItemCount: Int = addresses.size

    override def onCreateViewHolder(parent: ViewGroup, viewType: Int): QRViewHolder = {
      val qrCodeContainer = getLayoutInflater.inflate(R.layout.frag_qr, parent, false)
      new QRViewHolder(qrCodeContainer)
    }

    private def updateView(bu: BitcoinUri, holder: QRViewHolder): Unit = bu.uri foreach { uri =>
      val humanAmountOpt = for (requestedAmount <- bu.amount) yield WalletApp.denom.parsedWithSign(requestedAmount, cardIn, totalZero)
      val contentToShare = if (bu.amount.isDefined || bu.label.isDefined) PaymentRequestExt.withoutSlashes(InputParser.bitcoin, uri) else bu.address

      val visibleText = (bu.label, humanAmountOpt) match {
        case Some(label) ~ Some(amount) => s"${bu.address.short}<br><br>$label<br><br>$amount"
        case None ~ Some(amount) => s"${bu.address.short}<br><br>$amount"
        case Some(label) ~ None => s"${bu.address.short}<br><br>$label"
        case _ => bu.address.short
      }

      holder.qrLabel setText visibleText.html
      runInFutureProcessOnUI(QRActivity.get(contentToShare, qrSize), onFail) { qrBitmap =>
        def share: Unit = runInFutureProcessOnUI(shareData(qrBitmap, contentToShare), onFail)(none)
        holder.qrCopy setOnClickListener onButtonTap(WalletApp.app copy contentToShare)
        holder.qrCode setOnClickListener onButtonTap(WalletApp.app copy contentToShare)
        holder.qrEdit setOnClickListener onButtonTap(me editAddress bu)
        holder.qrShare setOnClickListener onButtonTap(share)
        holder.qrCode setImageBitmap qrBitmap
      }
    }
  }

  def editAddress(bu: BitcoinUri): Unit = {
    val maxMsat = Btc(21000000L).toSatoshi.toMilliSatoshi
    val canReceiveFiatHuman = WalletApp.currentMsatInFiatHuman(maxMsat)
    val canReceiveHuman = WalletApp.denom.parsedWithSign(maxMsat, cardIn, cardZero)
    val body = getLayoutInflater.inflate(R.layout.frag_input_off_chain, null).asInstanceOf[ViewGroup]
    lazy val manager = new RateManager(body, getString(dialog_add_description).asSome, dialog_visibility_sender, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
    mkCheckForm(proceed, none, titleBodyAsViewBuilder(getString(dialog_receive_btc).asColoredView(me chainWalletBackground wallet), manager.content), dialog_ok, dialog_cancel)
    manager.hintFiatDenom.setText(getString(dialog_up_to).format(canReceiveFiatHuman).html)
    manager.hintDenom.setText(getString(dialog_up_to).format(canReceiveHuman).html)
    bu.amount.foreach(manager.updateText)

    def proceed(alert: AlertDialog): Unit = {
      val uriBuilder = bu.uri.get.buildUpon.clearQuery
      val resultMsat = manager.resultMsat.truncateToSatoshi.toMilliSatoshi
      val uriBuilder1 = if (resultMsat > LNParams.chainWallets.params.dustLimit) uriBuilder.appendQueryParameter("amount", Denomination.msat2BtcBigDecimal(resultMsat).toString) else uriBuilder
      val uriBuilder2 = manager.resultExtraInput match { case Some(resultExtraInput) => uriBuilder1.appendQueryParameter("label", resultExtraInput) case None => uriBuilder1 }

      addresses = addresses map {
        case oldUri if oldUri.address == bu.uri.get.getHost => BitcoinUri(Success(uriBuilder2.build), oldUri.address)
        case oldUri => BitcoinUri(oldUri.uri.map(_.buildUpon.clearQuery.build), oldUri.address)
      }

      adapter.notifyDataSetChanged
      alert.dismiss
    }
  }

  def INIT(state: Bundle): Unit = {
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_qr_chain_addresses)
      checkExternalData(noneRunnable)
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }
  }

  def showCode: Unit = {
    runFutureProcessOnUI(wallet.getReceiveAddresses, onFail) { response =>
      val layoutManager = new CarouselLayoutManager(CarouselLayoutManager.HORIZONTAL, false)
      layoutManager.setPostLayoutListener(new CarouselZoomPostLayoutListener)
      layoutManager.setMaxVisibleItems(MAX_RECEIVE_ADDRESSES)

      // Allow MAX_RECEIVE_ADDRESSES - 6 (first 4 addresses) to be seen to not make it crowded
      allAddresses = response.keys.dropRight(6).map(response.ewt.textAddress).map(BitcoinUri.fromRaw)
      addresses = allAddresses.take(1)

      chainQrMore setOnClickListener onButtonTap {
        // Show all remaining QR images right away
        addresses = allAddresses

        // Animate list changes and remove a button since it gets useless
        adapter.notifyItemRangeInserted(1, allAddresses.size - 1)
        chainQrMore.setVisibility(View.GONE)
      }

      chainQrCodes.addOnScrollListener(new CenterScrollListener)
      chainQrCodes.setLayoutManager(layoutManager)
      chainQrCodes.setHasFixedSize(true)
      chainQrCodes.setAdapter(adapter)
    }

    val text = chainWalletNotice(wallet) map { textRes =>
      getString(dialog_receive_btc) + "<br>" + getString(textRes)
    } getOrElse getString(dialog_receive_btc)
    chainQrCaption.setText(text.html)
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case chainWallet: ElectrumEclairWallet => runAnd(wallet = chainWallet)(showCode)
    case _ => finish
  }
}
