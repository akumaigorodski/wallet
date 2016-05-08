package com.btcontract.wallet

import android.text.method.LinkMovementMethod
import org.bitcoinj.store.SPVBlockStore
import android.text.TextUtils
import android.os.Bundle
import android.view.View

import android.widget.{EditText, Button, TextView, LinearLayout}
import org.bitcoinj.wallet.{DeterministicSeed, Wallet}
import org.bitcoinj.core.{PeerGroup, BlockChain}
import Utils.{wrap, app}
import R.string._


object Mnemonic {
  def text(seed: DeterministicSeed) = TextUtils.join("\u0020", seed.getMnemonicCode)
  def decrypt(pass: String) = app.kit.wallet.getKeyCrypter match { case scrypt =>
    app.kit.wallet.getKeyChainSeed.decrypt(scrypt, pass, scrypt deriveKey pass)
  }
}

class WalletCreateActivity extends TimerActivity with ViewSwitch { me =>
  lazy val mnemonicText = findViewById(R.id.mnemonicText).asInstanceOf[TextView]
  lazy val walletReady = findViewById(R.id.walletReady).asInstanceOf[TextView]
  lazy val createWallet = findViewById(R.id.createWallet).asInstanceOf[Button]
  lazy val createPass = findViewById(R.id.createPass).asInstanceOf[EditText]
  lazy val info = findViewById(R.id.mnemonicInfo).asInstanceOf[TextView]
  lazy val spin = findViewById(R.id.createSpin).asInstanceOf[TextView]

  lazy val views =
    findViewById(R.id.createInfo).asInstanceOf[LinearLayout] ::
    findViewById(R.id.createProgress).asInstanceOf[LinearLayout] ::
    findViewById(R.id.createDone).asInstanceOf[LinearLayout] :: Nil

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    setContentView(R.layout.activity_create)
    info setMovementMethod LinkMovementMethod.getInstance

    createPass addTextChangedListener new TextChangedWatcher {
      override def onTextChanged(s: CharSequence, st: Int, n: Int, af: Int) = {
        val buttonMessage = if (s.length >= 6) wallet_create else password_too_short
        createWallet setEnabled s.length >= 6
        createWallet setText buttonMessage
      }
    }
  }

  def makeNewWallet =
    app.kit = new app.WalletKit {
      timer.scheduleAtFixedRate(new Spinner(spin), 0, 1000)
      setVis(View.GONE, View.VISIBLE, View.GONE)
      startAsync

      override def startUp = {
        wallet = new Wallet(app.params)
        val seed = wallet.getKeyChainSeed

        // Encrypt wallet and use checkpoints
        store = new SPVBlockStore(app.params, app.chainFile)
        useCheckPoints(wallet.getEarliestKeyCreationTime)
        app.kit encryptWallet createPass.getText

        // These should be initialized after checkpoints
        blockChain = new BlockChain(app.params, wallet, store)
        peerGroup = new PeerGroup(app.params, blockChain)

        def showDone = {
          mnemonicText setText Mnemonic.text(seed)
          setVis(View.GONE, View.GONE, View.VISIBLE)
        }

        if (app.isAlive) {
          setupAndStartDownload
          wallet saveToFile app.walletFile
          me runOnUiThread showDone
        }
      }
    }

  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)
  def openWallet(view: View) = me exitTo classOf[TxsActivity]
  def newWallet(view: View) = hideKeys(makeNewWallet)

  def revealMnemonic(show: View) = {
    mnemonicText setVisibility View.VISIBLE
    walletReady setText sets_noscreen
    show setVisibility View.GONE
  }
}