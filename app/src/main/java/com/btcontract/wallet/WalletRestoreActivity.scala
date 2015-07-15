package com.btcontract.wallet

import org.bitcoinj.store.SPVBlockStore
import android.os.Bundle
import android.view.View

import android.widget.{Button, EditText, TextView, LinearLayout}
import org.bitcoinj.wallet.{KeyChainGroup, DeterministicSeed}
import org.bitcoinj.core.{PeerGroup, BlockChain, Wallet}


class WalletRestoreActivity extends TimerActivity {
  lazy val restoreProgess = findViewById(R.id.restoreProgess).asInstanceOf[LinearLayout]
  lazy val restoreInfo = findViewById(R.id.restoreInfo).asInstanceOf[LinearLayout]
  lazy val restoreWallet = findViewById(R.id.restoreWallet).asInstanceOf[Button]
  lazy val restoreCode = findViewById(R.id.restoreCode).asInstanceOf[TextView]
  lazy val password = findViewById(R.id.restorePass).asInstanceOf[EditText]
  lazy val spin = findViewById(R.id.restoreSpin).asInstanceOf[TextView]

  // Initialize this activity, method is run once
  override def onCreate(savedInstState: Bundle) =
  {
    super.onCreate(savedInstState)
    setContentView(R.layout.activity_restore)

    val changeListener = new TextChangedWatcher {
      override def onTextChanged(s: CharSequence, x: Int, y: Int, z: Int) = {
        val mnemonicWordsAreOk = restoreCode.getText.toString.split("\\s+").length > 11
        val passIsOk = password.getText.length >= 8

        restoreWallet.setEnabled(mnemonicWordsAreOk & passIsOk)
        if (!mnemonicWordsAreOk) restoreWallet setText R.string.restore_mnemonic_wrong
        else if (!passIsOk) restoreWallet setText R.string.password_too_short
        else restoreWallet setText R.string.restore_wallet
      }
    }

    password addTextChangedListener changeListener
    restoreCode addTextChangedListener changeListener
  }

  def doRecoverWallet =
    app.kit = new app.WalletKit {
      timer.scheduleAtFixedRate(new Spinner(spin), 1000, 1000)
      restoreProgess setVisibility View.VISIBLE
      restoreInfo setVisibility View.GONE
      startAsync

      def startUp = {
        val may1st2014 = 1398902400L
        val mnemonic = restoreCode.getText.toString
        val ds = new DeterministicSeed(mnemonic, null, "", may1st2014)
        val keyChainGroup = new KeyChainGroup(app.params, ds)
        store = new SPVBlockStore(app.params, app.chainFile)
        wallet = new Wallet(app.params, keyChainGroup)
        app.kit encryptWallet password.getText
        useCheckPoints(may1st2014)

        // Must be initialized after checkpoints
        blockChain = new BlockChain(app.params, wallet, store)
        peerGroup = new PeerGroup(app.params, blockChain)

        if (app.isAlive) {
          setupAndStartDownload
          wallet saveToFile app.walletFile
          exitTo apply classOf[WalletActivity]
        }
      }
    }

  def recWallet(recoverWalletView: View) = hideKeys(doRecoverWallet)
  override def onBackPressed = Utils.wrap(super.onBackPressed)(app.kit.stopAsync)
}