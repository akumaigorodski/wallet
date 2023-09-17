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


trait MnemonicActivity { me: BaseActivity =>
  def showMnemonicInput(titleRes: Int)(proceedWithMnemonics: List[String] => Unit): Unit = {
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

    val proceed: AlertDialog => Unit = alert => try {
      MnemonicCode.validate(getMnemonicList, englishWordList)
      if (alert.isShowing) proceedWithMnemonics(getMnemonicList)
      alert.dismiss
    } catch {
      case exception: Throwable =>
        val msg = getString(R.string.error_wrong_phrase)
        onFail(msg format exception.getMessage)
    }

    val builder = titleBodyAsViewBuilder(getString(titleRes).asDefView, mnemonicWrap)
    val alert = mkCheckForm(proceed, none, builder, R.string.dialog_ok, R.string.dialog_cancel)
    updatePopupButton(getPositiveButton(alert), isEnabled = false)

    recoveryPhrase addTextChangedListener onTextChange { _ =>
      updatePopupButton(getPositiveButton(alert), getMnemonicList.size > 11)
    }
  }

  lazy val englishWordList: Array[String] = {
    val rawData = getAssets.open("bip39_english_wordlist.txt")
    scala.io.Source.fromInputStream(rawData, "UTF-8").getLines.toArray
  }
}

class SetupActivity extends BaseActivity with MnemonicActivity { me =>
  lazy val activitySetupMain = findViewById(R.id.activitySetupMain).asInstanceOf[LinearLayout]

  lazy val enforceTor = new SettingsHolder(me) {
    settingsTitle.setText(settings_ensure_tor)
    setVis(isVisible = false, settingsInfo)
    disableIfOldAndroid

    override def updateView: Unit = {
      settingsCheck.setChecked(WalletApp.ensureTor)
    }

    view setOnClickListener onButtonTap {
      putBoolAndUpdateView(WalletApp.ENSURE_TOR, !WalletApp.ensureTor)
    }
  }

  val proceedWithMnemonics: List[String] => Unit = mnemonic => {
    val walletSeed = MnemonicCode.toSeed(mnemonic, passphrase = new String)
    val keys = LightningNodeKeys.makeFromSeed(walletSeed.toArray)
    val secret = WalletSecret(keys, mnemonic, walletSeed)
    WalletApp.extDataBag.putSecret(secret)
    WalletApp.makeOperational(secret)

    TransitionManager.beginDelayedTransition(activitySetupMain)
    activitySetupMain.setVisibility(View.GONE)
    exitTo(ClassNames.hubActivityClass)
  }

  override def START(s: Bundle): Unit = {
    setContentView(R.layout.activity_setup)
    activitySetupMain.addView(enforceTor.view, 0)
    enforceTor.updateView
  }

  def createNewWallet(view: View): Unit = {
    val twelveWordsEntropy = fr.acinq.eclair.randomBytes(length = 16)
    val mnemonic = MnemonicCode.toMnemonics(twelveWordsEntropy, englishWordList)
    proceedWithMnemonics(mnemonic)
  }

  def showMnemonicPopup(view: View): Unit = {
    val title = R.string.action_recovery_phrase_title
    showMnemonicInput(title)(proceedWithMnemonics)
  }
}
