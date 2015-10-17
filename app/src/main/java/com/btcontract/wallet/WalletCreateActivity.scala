package com.btcontract.wallet

import android.text.method.LinkMovementMethod
import org.bitcoinj.store.SPVBlockStore
import android.text.TextUtils
import android.os.Bundle
import android.view.View

import android.widget.{EditText, Button, TextView, LinearLayout}
import org.bitcoinj.core.{PeerGroup, BlockChain, Wallet}
import Utils.{wrap, separator}


class WalletCreateActivity extends TimerActivity { me =>
  lazy val createProgess = findViewById(R.id.createProgess).asInstanceOf[LinearLayout]
  lazy val createInfo = findViewById(R.id.createInfo).asInstanceOf[LinearLayout]
  lazy val createDone = findViewById(R.id.createDone).asInstanceOf[LinearLayout]

  // UI elements
  lazy val mnemonicShow = findViewById(R.id.mnemonicShow).asInstanceOf[Button]
  lazy val mnemonicText = findViewById(R.id.mnemonicText).asInstanceOf[TextView]
  lazy val createPass = findViewById(R.id.createPass).asInstanceOf[EditText]
  lazy val create = findViewById(R.id.createWallet).asInstanceOf[Button]
  lazy val spin = findViewById(R.id.createSpin).asInstanceOf[TextView]

  // Initialize this activity, method is run once
  override def onCreate(savedInstState: Bundle) =
  {
    super.onCreate(savedInstState)
    setContentView(R.layout.activity_create)
    val mnemonicInfo = findViewById(R.id.mnemonicInfo).asInstanceOf[TextView]
    mnemonicInfo setMovementMethod LinkMovementMethod.getInstance

    createPass addTextChangedListener new TextChangedWatcher {
      override def onTextChanged(s: CharSequence, start: Int, count: Int, after: Int) = {
        val txt = if (s.length >= 8) R.string.wallet_create else R.string.password_too_short
        create setEnabled s.length >= 8
        create setText txt
      }
    }
  }

  def doCreateNewWallet =
    app.kit = new app.WalletKit {
      val spinner = new Spinner(spin)
      val pass = createPass.getText.toString
      timer.scheduleAtFixedRate(spinner, 1000, 1000)
      createProgess setVisibility View.VISIBLE
      createInfo setVisibility View.GONE
      startAsync

      def updateView = wallet.getKeyCrypter match { case crypter =>
        val seed = wallet.getKeyChainSeed.decrypt(crypter, pass, crypter deriveKey pass)
        val mnemonic = TextUtils.join(separator, seed.getMnemonicCode).toUpperCase
        createProgess setVisibility View.GONE
        createDone setVisibility View.VISIBLE
        mnemonicText setText mnemonic
      }

      override def startUp = {
        wallet = new Wallet(app.params)
        store = new SPVBlockStore(app.params, app.chainFile)
        useCheckPoints(wallet.getEarliestKeyCreationTime)
        app.kit encryptWallet pass

        // Must be initialized after checkpoints
        blockChain = new BlockChain(app.params, wallet, store)
        peerGroup = new PeerGroup(app.params, blockChain)

        if (app.isAlive) {
          setupAndStartDownload
          wallet saveToFile app.walletFile
          me runOnUiThread updateView
        }
      }
    }

  def showMnemo(view: View) = {
    mnemonicText setVisibility View.VISIBLE
    mnemonicShow setVisibility View.GONE
  }

  def newWallet(view: View) = hideKeys(doCreateNewWallet)
  def openWallet(view: View) = me exitTo classOf[WalletActivity]
  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)
}