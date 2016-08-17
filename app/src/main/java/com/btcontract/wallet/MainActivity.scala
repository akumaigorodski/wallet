package com.btcontract.wallet

import R.string._
import android.widget._
import scala.util.{Failure, Success, Try}
import org.bitcoinj.core.{PeerGroup, BlockChain}
import com.btcontract.wallet.helper.{FiatRates, Fee}
import org.bitcoinj.wallet.WalletProtobufSerializer
import android.text.method.LinkMovementMethod
import java.io.FileInputStream
import android.os.Bundle
import android.view.View
import Utils.app


trait ViewSwitch {
  val views: List[View]
  def setVis(ms: Int*) = views zip ms foreach {
    case (view, state) => view setVisibility state
  }
}

class MainActivity extends TimerActivity with ViewSwitch { me =>
  lazy val greet = findViewById(R.id.mainGreetings).asInstanceOf[TextView]
  lazy val views = findViewById(R.id.mainProgress).asInstanceOf[ImageView] ::
    findViewById(R.id.mainChoice).asInstanceOf[LinearLayout] :: Nil

  // Either proceed anyway or quit an application
  def onFail(code: Int): Unit = mkForm(mkChoiceDialog(next, finish,
    dialog_ok, dialog_cancel) setMessage code, null, null)

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    setContentView(R.layout.activity_main)
    greet setMovementMethod LinkMovementMethod.getInstance

    // Proceed if no data present or data is correct, show choice otherwise
    Try(getIntent.getDataString).filter(_ != null).map(app.TransData.setValue) match {
      case Failure(gotNull: NoSuchElementException) | Success(setSuccess: Unit) => next
      case Failure(error) => app.TransData.onFail(onFail)(error)
    }

    // Periodic http
    FiatRates.go
    Fee.go
  }

  def next = app.walletFile.exists match {
    case false => setVis(View.GONE, View.VISIBLE)
    case true if app.isAlive => goWalletTxs
    case true => prepareWallet
  }

  def prepareWallet =
    app.kit = new app.WalletKit {
      // Prepare wallet in a special
      // separate service thread
      startAsync

      def startUp = {
        val stream = new FileInputStream(app.walletFile)
        val proto = try WalletProtobufSerializer parseToProto stream finally stream.close
        wallet = new WalletProtobufSerializer readWallet (app.params, null, proto)
        store = new org.bitcoinj.store.SPVBlockStore(app.params, app.chainFile)
        blockChain = new BlockChain(app.params, wallet, store)
        peerGroup = new PeerGroup(app.params, blockChain)
        setupAndStartDownload
        goWalletTxs
      }
    }

  // State transition helpers
  def goRestoreWallet(view: View) = me exitTo classOf[WalletRestoreActivity]
  def goCreateWallet(view: View) = me exitTo classOf[WalletCreateActivity]
  def goWalletTxs = me exitTo classOf[TxsActivity]
}