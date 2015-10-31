package com.btcontract.wallet

import org.bitcoinj.store.SPVBlockStore
import android.os.Bundle
import android.view.View
import Utils.wrap

import java.util.{Date, Calendar}
import org.bitcoinj.wallet.{KeyChainGroup, DeterministicSeed}
import org.bitcoinj.core.{PeerGroup, BlockChain, Wallet}
import android.app.{DatePickerDialog, DialogFragment}
import Calendar.{YEAR, MONTH, DATE}
import android.widget._


class WalletRestoreActivity extends TimerActivity { me =>
  lazy val restoreProgess = findViewById(R.id.restoreProgess).asInstanceOf[LinearLayout]
  lazy val restoreInfo = findViewById(R.id.restoreInfo).asInstanceOf[LinearLayout]
  lazy val restoreWallet = findViewById(R.id.restoreWallet).asInstanceOf[Button]
  lazy val restoreCode = findViewById(R.id.restoreCode).asInstanceOf[TextView]
  lazy val restoreWhen = findViewById(R.id.restoreWhen).asInstanceOf[Button]
  lazy val password = findViewById(R.id.restorePass).asInstanceOf[EditText]
  lazy val spin = findViewById(R.id.restoreSpin).asInstanceOf[TextView]
  val may1st2014 = 1398902400L * 1000
  var when = Calendar.getInstance

  class WhenPicker extends DialogFragment with android.app.DatePickerDialog.OnDateSetListener {
    def onDateSet(dp: DatePicker, yr: Int, mn: Int, dt: Int) = wrap(updateWhen) _ apply when.set(yr, mn, dt)
    val dialog = new DatePickerDialog(me, this, when get YEAR, when get MONTH, when get DATE)

    override def onCreateDialog(state: Bundle) = {
      dialog.getDatePicker setMaxDate (new Date).getTime
      dialog
    }
  }

  // Initialize this activity, method is run once
  override def onCreate(savedInstState: Bundle) =
  {
    super.onCreate(savedInstState)
    setContentView(R.layout.activity_restore)
    when setTimeInMillis may1st2014
    updateWhen

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
        val mnemo = restoreCode.getText.toString
        val whenTime = when.getTimeInMillis / 1000
        val ds = new DeterministicSeed(mnemo, null, "", whenTime)
        val keyChainGroup = new KeyChainGroup(app.params, ds)
        store = new SPVBlockStore(app.params, app.chainFile)
        wallet = new Wallet(app.params, keyChainGroup)
        app.kit encryptWallet password.getText
        useCheckPoints(whenTime)

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

  def recWallet(v: View) = hideKeys(doRecoverWallet)
  def setWhen(v: View) = (new WhenPicker).show(getFragmentManager, "whenCreatedTime")
  def updateWhen = restoreWhen setText String.format("%1$tB %1$te, %1$tY", when)
  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)
}