package com.btcontract.wallet

import org.bitcoinj.store.SPVBlockStore
import android.os.Bundle
import android.view.View

import android.widget.{EditText, Button, TextView, LinearLayout}
import org.bitcoinj.core.{PeerGroup, BlockChain, Wallet}


class WalletCreateActivity extends TimerActivity { me =>
  lazy val createProgess = findViewById(R.id.createProgess).asInstanceOf[LinearLayout]
  lazy val createInfo = findViewById(R.id.createInfo).asInstanceOf[LinearLayout]
  lazy val createPass = findViewById(R.id.createPass).asInstanceOf[EditText]
  lazy val create = findViewById(R.id.createWallet).asInstanceOf[Button]
  lazy val spin = findViewById(R.id.createSpin).asInstanceOf[TextView]

  // Initialize this activity, method is run once
  override def onCreate(savedInstState: Bundle) =
  {
    super.onCreate(savedInstState)
    setContentView(R.layout.activity_create)

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
      timer.scheduleAtFixedRate(new Spinner(spin), 1000, 1000)
      createProgess setVisibility View.VISIBLE
      createInfo setVisibility View.GONE
      startAsync

      override def startUp = {
        wallet = new Wallet(app.params)
        store = new SPVBlockStore(app.params, app.chainFile)
        useCheckPoints(wallet.getEarliestKeyCreationTime)
        app.kit encryptWallet createPass.getText

        // Must be initialized after checkpoints
        blockChain = new BlockChain(app.params, wallet, store)
        peerGroup = new PeerGroup(app.params, blockChain)

        if (app.isAlive) {
          setupAndStartDownload
          wallet saveToFile app.walletFile
          me exitTo classOf[WalletActivity]
        }
      }
    }

  def newWallet(newButtonView: View) = hideKeys(doCreateNewWallet)
  override def onBackPressed = Utils.wrap(super.onBackPressed)(app.kit.stopAsync)
}