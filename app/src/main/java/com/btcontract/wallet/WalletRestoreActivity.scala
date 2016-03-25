package com.btcontract.wallet

import android.os.Bundle
import java.util.Calendar
import org.bitcoinj.store.SPVBlockStore
import android.widget.DatePicker.OnDateChangedListener
import org.bitcoinj.wallet.{KeyChainGroup, DeterministicSeed}
import org.bitcoinj.core.{PeerGroup, BlockChain, Wallet}
import Utils.{wrap, app, none, runAnd}
import android.view.{ViewGroup, View}
import android.widget._
import R.string._


class WhenPicker(host: TimerActivity, start: Long)
extends DatePicker(host) with OnDateChangedListener { me =>
  def pure = runAnd(me)(try getParent.asInstanceOf[ViewGroup] removeView me catch none)
  def human = java.text.DateFormat getDateInstance java.text.DateFormat.MEDIUM format cal.getTime
  def onDateChanged(view: DatePicker, year: Int, mon: Int, dt: Int) = cal.set(year, mon, dt)
  init(cal get Calendar.YEAR, cal get Calendar.MONTH, cal get Calendar.DATE, me)
  me setCalendarViewShown false

  lazy val cal = {
    val calendar = Calendar.getInstance
    calendar setTimeInMillis start
    calendar
  }
}

class WalletRestoreActivity extends TimerActivity with ViewSwitch { me =>
  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)
  private[this] lazy val datePicker = new WhenPicker(me, 1398902400L * 1000)
  private[this] lazy val restoreWallet = findViewById(R.id.restoreWallet).asInstanceOf[Button]
  private[this] lazy val restoreCode = findViewById(R.id.restoreCode).asInstanceOf[TextView]
  private[this] lazy val restoreWhen = findViewById(R.id.restoreWhen).asInstanceOf[Button]
  private[this] lazy val password = findViewById(R.id.restorePass).asInstanceOf[EditText]
  private[this] lazy val spin = findViewById(R.id.restoreSpin).asInstanceOf[TextView]
  lazy val views = findViewById(R.id.restoreInfo).asInstanceOf[LinearLayout] ::
    findViewById(R.id.restoreProgress).asInstanceOf[LinearLayout] :: Nil

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    setContentView(R.layout.activity_restore)
    restoreWhen setText datePicker.human

    val changeListener = new TextChangedWatcher {
      override def onTextChanged(s: CharSequence, x: Int, y: Int, z: Int) = {
        val mnemonicWordsAreOk = restoreCode.getText.toString.split("\\s+").length > 11
        val passIsOk = password.getText.length >= 8

        restoreWallet.setEnabled(mnemonicWordsAreOk & passIsOk)
        if (!mnemonicWordsAreOk) restoreWallet setText restore_mnemonic_wrong
        else if (!passIsOk) restoreWallet setText password_too_short
        else restoreWallet setText restore_wallet
      }
    }

    password addTextChangedListener changeListener
    restoreCode addTextChangedListener changeListener
  }

  def doRecoverWallet =
    app.kit = new app.WalletKit {
      timer.scheduleAtFixedRate(new Spinner(spin), 0, 1000)
      setVis(View.GONE, View.VISIBLE)
      startAsync

      def startUp = {
        val whenTime = datePicker.cal.getTimeInMillis / 1000
        val mnemonic = restoreCode.getText.toString.toLowerCase.trim
        val seed = new DeterministicSeed(mnemonic, null, "", whenTime)
        val keyChainGroup = new KeyChainGroup(app.params, seed)

        // Recreate encrypted wallet and use checkpoints
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
          exitTo apply classOf[TxsActivity]
        }
      }
    }

  def recWallet(v: View) = hideKeys(doRecoverWallet)
  def setWhen(v: View) = mkForm(mkChoiceDialog(restoreWhen setText
    datePicker.human, none, dialog_ok, dialog_cancel), null, datePicker.pure)
}