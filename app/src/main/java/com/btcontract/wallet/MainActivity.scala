package com.btcontract.wallet

import R.string._
import android.widget._
import Utils.{wrap, app}
import scala.util.{Success, Try}
import com.btcontract.wallet.helper.{FiatRates, Fee}
import org.bitcoinj.core.{PeerGroup, BlockChain, Wallet}
import concurrent.ExecutionContext.Implicits.global
import org.bitcoinj.store.WalletProtobufSerializer
import android.text.method.LinkMovementMethod
import android.view.View.OnClickListener
import scala.concurrent.Future
import java.io.FileInputStream
import android.os.Bundle
import android.view.View


trait ViewSwitch {
  val views: List[View]
  def setVis(ms: Int*) = views zip ms foreach {
    case (view, state) => view setVisibility state
  }
}

class MainActivity extends TimerActivity with ViewSwitch { me =>
  def errorWarn(code: Int): Unit = mkForm(mkChoiceDialog(next, finish,
    dialog_ok, dialog_cancel) setMessage code, null, null)

  // Saved settings data
  lazy val askPass = app.prefs.getBoolean(AbstractKit.PASSWORD_ASK_STARTUP, false)
  lazy val destructCode = app.prefs.getString(AbstractKit.DESTRUCT_CODE, null)

  // Interface elements we'll need to interact with
  lazy val greet = findViewById(R.id.mainGreetings).asInstanceOf[TextView]
  lazy val passData = findViewById(R.id.passData).asInstanceOf[EditText]
  lazy val checkPass = findViewById(R.id.checkPass).asInstanceOf[Button]

  lazy val views =
    findViewById(R.id.mainProgress).asInstanceOf[ImageView] ::
    findViewById(R.id.mainPassword).asInstanceOf[LinearLayout] ::
    findViewById(R.id.mainChoice).asInstanceOf[LinearLayout] :: Nil

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    setContentView(R.layout.activity_main)
    greet setMovementMethod LinkMovementMethod.getInstance

    Try(getIntent.getDataString) match {
      case ok@Success(dataNotNull: String) =>
        // Okay, we've got some string, now try to convert it
        val attempt = ok.map(app.TransData.setValue).map(_ => next)
        // So we've indeed got a string, but it is not Bitcoin-related
        attempt.recover(app.TransData onFail errorWarn)

      // Usual launch
      case _ => next
    }

    // Periodic http
    FiatRates.go
    Fee.go
  }

  def next = app.walletFile.exists match {
    case true if app.isAlive => exitToWalletNow
    case true if askPass => showPassword(Future apply prepareWallet)
    case true => showLoadProgress(Future apply prepareWallet)
    case _ => setVis(View.GONE, View.GONE, View.VISIBLE)
  }

  val showPassword: Future[Unit] => Unit = work =>
    checkPass setOnClickListener new OnClickListener {
      def showPrompt = setVis(View.GONE, View.VISIBLE, View.GONE)
      def check = Mnemonic decrypt passData.getText.toString
      def wrong = wrap(me toast password_wrong)(showPrompt)
      showPrompt

      def onClick(view: View) = hideKeys {
        setVis(View.VISIBLE, View.GONE, View.GONE)
        if (passData.getText.toString == destructCode) replaceWallet
        else <(check, _ => wrong)(_ => showLoadProgress apply work)
      }
    }

  val showLoadProgress: Future[Unit] => Unit = work => {
    app.prefs.edit.putBoolean(AbstractKit.PASSWORD_ASK_STARTUP, false).commit
    <<(work, _ => me errorWarn err_general)(_ => app.kit.startAsync)
  }

  def prepareWallet =
    app.kit = new app.WalletKit {
      val stream = new FileInputStream(app.walletFile)
      val proto = try WalletProtobufSerializer parseToProto stream finally stream.close
      wallet = new WalletProtobufSerializer readWallet (app.params, null, proto)
      store = new org.bitcoinj.store.SPVBlockStore(app.params, app.chainFile)
      blockChain = new BlockChain(app.params, wallet, store)
      peerGroup = new PeerGroup(app.params, blockChain)

      def startUp = {
        setupAndStartDownload
        exitToWalletNow
      }
    }

  def replaceWallet =
    app.kit = new app.WalletKit {
      val slot = AbstractKit.DESTRUCT_CODE
      app.prefs.edit.remove(slot).commit
      startAsync

      def startUp = {
        wallet = new Wallet(app.params)
        store = new org.bitcoinj.store.SPVBlockStore(app.params, app.chainFile)
        blockChain = new BlockChain(app.params, wallet, store)
        peerGroup = new PeerGroup(app.params, blockChain)
        app.kit encryptWallet passData.getText.toString
        wallet saveToFile app.walletFile
        setupAndStartDownload
        exitToWalletNow
      }
    }

  // State transition helpers
  def goRestoreWallet(view: View) = me exitTo classOf[WalletRestoreActivity]
  def goCreateWallet(view: View) = me exitTo classOf[WalletCreateActivity]
  def exitToWalletNow = me exitTo classOf[TxsActivity]
}