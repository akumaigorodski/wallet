package com.btcontract.wallet

import R.string._
import android.widget._
import scala.util.{Success, Try}
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

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    setContentView(R.layout.activity_main)
    greet setMovementMethod LinkMovementMethod.getInstance

    Try(getIntent.getDataString) match {
      case ok@Success(dataNotNull: String) =>
        // If only we actually have some string, try to process it and inform on error
        ok.map(app.TransData.setValue).map(_ => next) recover app.TransData.onFail { code =>
          mkForm(mkChoiceDialog(next, finish, dialog_ok, dialog_cancel) setMessage code, null, null)
        }

      // Usual launch
      case _ => next
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