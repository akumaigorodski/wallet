package com.btcontract.wallet

import android.os.Bundle
import android.view.View
import android.widget.{ArrayAdapter, LinearLayout}
import androidx.appcompat.app.AlertDialog
import androidx.transition.TransitionManager
import com.btcontract.wallet.R.string._
import fr.acinq.bitcoin.MnemonicCode
import immortan.crypto.Tools.{SEPARATOR, none}
import immortan.{LightningNodeKeys, WalletSecret}
import scodec.bits.ByteVector


object SetupActivity {
  def fromMnemonics(mnemonics: List[String], host: BaseActivity): Unit = {
    val walletSeed = MnemonicCode.toSeed(mnemonics, passphrase = new String)
    val keys = LightningNodeKeys.makeFromSeed(seed = walletSeed.toArray)
    val secret = WalletSecret(keys, mnemonics, walletSeed)
    WalletApp.extDataBag.putSecret(secret)
    WalletApp.makeOperational(secret)
  }
}

class SetupActivity extends BaseActivity { me =>
  private[this] lazy val activitySetupMain = findViewById(R.id.activitySetupMain).asInstanceOf[LinearLayout]

  lazy private[this] val enforceTor = new SettingsHolder(me) {
    override def updateView: Unit = settingsCheck.setChecked(WalletApp.ensureTor)
    settingsTitle.setText(settings_ensure_tor)
    setVis(isVisible = false, settingsInfo)
    disableIfOldAndroid

    view setOnClickListener onButtonTap {
      putBoolAndUpdateView(WalletApp.ENSURE_TOR, !WalletApp.ensureTor)
    }
  }

  override def START(s: Bundle): Unit = {
    setContentView(R.layout.activity_setup)
    activitySetupMain.addView(enforceTor.view, 0)
    enforceTor.updateView
  }

  private[this] lazy val englishWordList = {
    val rawData = getAssets.open("bip39_english_wordlist.txt")
    scala.io.Source.fromInputStream(rawData, "UTF-8").getLines.toArray
  }

  var proceedWithMnemonics: List[String] => Unit = mnemonics => {
    // Make sure this method can be run at most once (to not set runtime data twice) by replacing it with a noop method right away
    runInFutureProcessOnUI(SetupActivity.fromMnemonics(mnemonics, me), onFail)(_ => me exitTo ClassNames.hubActivityClass)
    TransitionManager.beginDelayedTransition(activitySetupMain)
    activitySetupMain.setVisibility(View.GONE)
    proceedWithMnemonics = none
  }

  def createNewWallet(view: View): Unit = {
    val twelveWordsEntropy: ByteVector = fr.acinq.eclair.randomBytes(16)
    val mnemonic = MnemonicCode.toMnemonics(twelveWordsEntropy, englishWordList)
    proceedWithMnemonics(mnemonic)
  }

  def showMnemonicPopup(view: View): Unit = {
    val mnemonicWrap = getLayoutInflater.inflate(R.layout.frag_mnemonic, null).asInstanceOf[LinearLayout]
    val recoveryPhrase = mnemonicWrap.findViewById(R.id.recoveryPhrase).asInstanceOf[com.hootsuite.nachos.NachoTextView]
    recoveryPhrase.addChipTerminator(' ', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    recoveryPhrase.addChipTerminator(',', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    recoveryPhrase.addChipTerminator('\n', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    recoveryPhrase setAdapter new ArrayAdapter(me, android.R.layout.simple_list_item_1, englishWordList)
    recoveryPhrase setDropDownBackgroundResource R.color.button_material_dark

    def getMnemonicList: List[String] = {
      val mnemonic = recoveryPhrase.getText.toString.toLowerCase.trim
      val pureMnemonic = mnemonic.replaceAll("[^a-zA-Z0-9']+", SEPARATOR)
      pureMnemonic.split(SEPARATOR).toList
    }

    def proceed(alert: AlertDialog): Unit = try {
      MnemonicCode.validate(getMnemonicList, englishWordList)
      proceedWithMnemonics(getMnemonicList)
      alert.dismiss
    } catch {
      case exception: Throwable =>
        val msg = getString(R.string.error_wrong_phrase)
        onFail(msg format exception.getMessage)
    }

    val builder = titleBodyAsViewBuilder(getString(R.string.action_recovery_phrase_title).asDefView, mnemonicWrap)
    val alert = mkCheckForm(proceed, none, builder, R.string.dialog_ok, R.string.dialog_cancel)
    updatePopupButton(getPositiveButton(alert), isEnabled = false)

    recoveryPhrase addTextChangedListener onTextChange { _ =>
      updatePopupButton(getPositiveButton(alert), getMnemonicList.size > 11)
    }
  }
}
