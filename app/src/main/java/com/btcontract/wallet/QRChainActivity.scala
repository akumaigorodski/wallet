package com.btcontract.wallet

import fr.acinq.eclair._
import immortan.crypto.Tools._
import com.btcontract.wallet.Colors._
import com.btcontract.wallet.R.string._
import com.azoft.carousellayoutmanager._

import android.view.{View, ViewGroup}
import immortan.utils.{BitcoinUri, Denomination, InputParser, PaymentRequestExt}
import fr.acinq.eclair.blockchain.EclairWallet.MAX_RECEIVE_ADDRESSES
import com.btcontract.wallet.BaseActivity.StringOps
import androidx.recyclerview.widget.RecyclerView
import androidx.appcompat.app.AlertDialog
import com.ornach.nobobutton.NoboButton
import android.widget.TextView
import fr.acinq.bitcoin.Btc
import scala.util.Success
import immortan.LNParams
import android.os.Bundle


class QRChainActivity extends QRActivity { me =>
  lazy private[this] val chainQrCaption = findViewById(R.id.chainQrCaption).asInstanceOf[TextView]
  lazy private[this] val chainQrCodes = findViewById(R.id.chainQrCodes).asInstanceOf[RecyclerView]
  lazy private[this] val chainQrMore = findViewById(R.id.chainQrMore).asInstanceOf[NoboButton]

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
      val humanAmountOpt = for (amount <- bu.amount) yield WalletApp.denom.parsedWithSign(amount, cardIn, totalZero)
      val withoutSlashes = PaymentRequestExt.withoutSlashes(InputParser.bitcoin, uri)

      val visibleText = (bu.label, humanAmountOpt) match {
        case Some(label) ~ Some(amount) => s"${bu.address.short}<br><br>$label<br><br>$amount"
        case None ~ Some(amount) => s"${bu.address.short}<br><br>$amount"
        case Some(label) ~ None => s"${bu.address.short}<br><br>$label"
        case _ => bu.address.short
      }

      holder.qrLabel setText visibleText.html
      runInFutureProcessOnUI(QRActivity.get(bu.address.toUpperCase, qrSize), onFail) { qrBitmap =>
        def share: Unit = runInFutureProcessOnUI(shareData(qrBitmap, withoutSlashes), onFail)(none)
        holder.qrCopy setOnClickListener onButtonTap(WalletApp.app copy withoutSlashes)
        holder.qrCode setOnClickListener onButtonTap(WalletApp.app copy withoutSlashes)
        holder.qrEdit setOnClickListener onButtonTap(me editAddress bu)
        holder.qrShare setOnClickListener onButtonTap(share)
        holder.qrCode setImageBitmap qrBitmap
      }
    }
  }

  def editAddress(bu: BitcoinUri): Unit = {
    val max: MilliSatoshi = Btc(21000000L).toMilliSatoshi
    val canReceiveFiatHuman: String = WalletApp.currentMsatInFiatHuman(max)
    val canReceiveHuman: String = WalletApp.denom.parsedWithSign(max, cardIn, cardZero)
    val body: ViewGroup = getLayoutInflater.inflate(R.layout.frag_input_off_chain, null).asInstanceOf[ViewGroup]
    lazy val manager: RateManager = new RateManager(body, getString(dialog_add_description).asSome, dialog_visibility_public, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
    mkCheckForm(proceed, none, titleBodyAsViewBuilder(getString(dialog_receive_btc).asColoredView(R.color.cardBitcoinModern), manager.content), dialog_ok, dialog_cancel)
    manager.hintFiatDenom.setText(getString(dialog_up_to).format(canReceiveFiatHuman).html)
    manager.hintDenom.setText(getString(dialog_up_to).format(canReceiveHuman).html)
    bu.amount.foreach(manager.updateText)

    def proceed(alert: AlertDialog): Unit = {
      val uriBuilder = bu.uri.get.buildUpon.clearQuery
      val uriBuilder1 = if (manager.resultMsat > LNParams.chainWallets.params.dustLimit) uriBuilder.appendQueryParameter("amount", Denomination.mast2BtcBigDecimal(manager.resultMsat).toString) else uriBuilder
      val uriBuilder2 = manager.resultExtraInput match { case Some(resultExtraInput) => uriBuilder1.appendQueryParameter("label", resultExtraInput) case None => uriBuilder1 }

      addresses = addresses map {
        case oldUri if oldUri.address == bu.uri.get.getHost => BitcoinUri(Success(uriBuilder2.build), oldUri.address)
        case oldUri => BitcoinUri(oldUri.uri.map(_.buildUpon.clearQuery.build), oldUri.address)
      }

      adapter.notifyDataSetChanged
      alert.dismiss
    }
  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      runFutureProcessOnUI(LNParams.chainWallets.lnWallet.getReceiveAddresses, onFail) { response =>
        val layoutManager = new CarouselLayoutManager(CarouselLayoutManager.HORIZONTAL, false)
        layoutManager.setPostLayoutListener(new CarouselZoomPostLayoutListener)
        layoutManager.setMaxVisibleItems(MAX_RECEIVE_ADDRESSES)

        // Allow MAX_RECEIVE_ADDRESSES - 6 (first 4 addresses) to be seen to not make it crowded
        allAddresses = response.accountToKey.keys.dropRight(6).toList.map(BitcoinUri.fromRaw)
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

      setContentView(R.layout.activity_qr_chain_addresses)
      chainQrCaption.setText(getString(dialog_receive_btc).html)
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }
}
