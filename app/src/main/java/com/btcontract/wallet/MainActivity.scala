package com.btcontract.wallet

import org.bitcoinj.store.WalletProtobufSerializer
import android.text.method.LinkMovementMethod
import android.app.AlertDialog.Builder
import java.io.FileInputStream
import android.os.Bundle
import android.view.View

import org.bitcoinj.core.{PeerGroup, BlockChain, Wallet}
import android.widget.{LinearLayout, EditText, TextView}
import R.string.{dialog_ok, dialog_cancel}
import scala.util.{Success, Try}
import View.{GONE, VISIBLE}


class MainActivity extends TimerActivity { me =>
  lazy val askPass = prefs.getBoolean(AbstractKit.PASSWORD_ASK_STARTUP, false)
  lazy val destructCode = prefs.getString(AbstractKit.DESTRUCT_CODE, null)
  lazy val sack = prefs.getBoolean(AbstractKit.SACK_OR_TXS, true)

  lazy val password = findViewById(R.id.mainPassword).asInstanceOf[LinearLayout]
  lazy val progress = findViewById(R.id.mainProgess).asInstanceOf[LinearLayout]
  lazy val choice = findViewById(R.id.mainChoice).asInstanceOf[LinearLayout]
  lazy val passData = findViewById(R.id.mainPassData).asInstanceOf[EditText]
  lazy val spin = findViewById(R.id.mainSpin).asInstanceOf[TextView]
  var activityIsCurrentlyOperational = true

  // Initialize this activity, method is run once
  override def onCreate(savedInstState: Bundle) =
  {
    super.onCreate(savedInstState)
    setContentView(R.layout.activity_main)

    Try(getIntent.getDataString) match {
      case ok@Success(dataNotNull: String) =>
        ok map app.PaymentInformation.setOutput map (_ => next) recover app.PaymentInformation.onFail {
          showChoiceAlert(next, finish, dialog_ok, dialog_cancel).setMessage(_).show setCanceledOnTouchOutside false
        }
      case _ => next
    }
  }

  def next = if (app.walletFile.exists) {
    timer.scheduleAtFixedRate(new Spinner(spin), 1000, 1000)
    if (app.isAlive) walletOrHistory else warmUp
  } else {
    val mg = findViewById(R.id.mainGreetings).asInstanceOf[TextView]
    mg setMovementMethod LinkMovementMethod.getInstance
    choice setVisibility VISIBLE
  }

  def warmUp = {
    progress setVisibility VISIBLE
    <(prepareWallet, _ => System exit 0)(_ => react)

    def react = if (askPass) {
      password setVisibility VISIBLE
      progress setVisibility GONE
    } else maybeStartKit
  }

  def tryPass(view: View) = hideKeys {
    if (passWordInput == destructCode) replaceWallet
    else <(check, _ => wrong)(if (_) maybeStartKit else wrong)
    progress setVisibility VISIBLE
    password setVisibility GONE

    def wrong = {
      toast(R.string.password_wrong)
      password setVisibility VISIBLE
      progress setVisibility GONE
    }
  }

  def prepareWallet =
    app.kit = new app.WalletKit {
      val stream = new FileInputStream(app.walletFile)
      val proto = try WalletProtobufSerializer.parseToProto(stream) finally stream.close
      wallet = new WalletProtobufSerializer readWallet (app.params, null, proto)
      store = new org.bitcoinj.store.SPVBlockStore(app.params, app.chainFile)
      blockChain = new BlockChain(app.params, wallet, store)
      peerGroup = new PeerGroup(app.params, blockChain)

      def startUp = {
        setupAndStartDownload
        walletOrHistory
      }
    }

  def replaceWallet =
    app.kit = new app.WalletKit {
      // Clear destruction code and make a new wallet
      prefs.edit.remove(AbstractKit.DESTRUCT_CODE).commit
      startAsync

      def startUp = {
        wallet = new Wallet(app.params)
        store = new org.bitcoinj.store.SPVBlockStore(app.params, app.chainFile)
        blockChain = new BlockChain(app.params, wallet, store)
        peerGroup = new PeerGroup(app.params, blockChain)
        app.kit encryptWallet passWordInput
        wallet saveToFile app.walletFile
        setupAndStartDownload
        walletOrHistory
      }
    }

  override def onBackPressed = Utils.wrap(super.onBackPressed) { activityIsCurrentlyOperational = false }
  def walletOrHistory = if (sack) me exitTo classOf[WalletActivity] else me exitTo classOf[TxsActivity]
  def maybeStartKit = if (activityIsCurrentlyOperational) app.kit.startAsync
  def goRestoreWallet(v: View) = me goTo classOf[WalletRestoreActivity]
  def goCreateWallet(v: View) = me goTo classOf[WalletCreateActivity]
  def openConverter(v: View) = me mkConverterForm new Builder(me)
  def check = app.kit.wallet checkPassword passWordInput
  def passWordInput = passData.getText.toString
}