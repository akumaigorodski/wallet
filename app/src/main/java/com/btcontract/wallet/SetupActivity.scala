package com.btcontract.wallet

import android.os.{Build, Bundle}
import android.view.View
import android.widget._
import androidx.appcompat.app.AlertDialog
import androidx.transition.TransitionManager
import com.btcontract.wallet.R.string._
import fr.acinq.bitcoin.MnemonicCode
import fr.acinq.eclair.wire.CommonCodecs.nodeaddress
import fr.acinq.eclair.wire.{Domain, NodeAddress}
import immortan.crypto.Tools.{SEPARATOR, StringList, none, runAnd, \}
import immortan.{LightningNodeKeys, WalletSecret}

import scala.util.Success


abstract class SettingsHolder(host: BaseActivity) {
  val view: RelativeLayout = host.getLayoutInflater.inflate(R.layout.frag_switch, null, false).asInstanceOf[RelativeLayout]
  val settingsCheck: CheckBox = view.findViewById(R.id.settingsCheck).asInstanceOf[CheckBox]
  val settingsTitle: TextView = view.findViewById(R.id.settingsTitle).asInstanceOf[TextView]
  val settingsInfo: TextView = view.findViewById(R.id.settingsInfo).asInstanceOf[TextView]
  def updateView: Unit

  def putBoolAndUpdateView(key: String, value: Boolean): Unit = {
    WalletApp.app.prefs.edit.putBoolean(key, value).commit
    updateView
  }

  def disableIfOldAndroid: Unit = if (Build.VERSION.SDK_INT < Build.VERSION_CODES.N) {
    val message = host.getString(error_old_api_level).format(Build.VERSION.SDK_INT)
    host.setVis(isVisible = true, settingsInfo)
    settingsInfo.setText(message)
    settingsTitle.setAlpha(0.5F)
    view.setEnabled(false)
  }
}

trait MnemonicActivity { me: BaseActivity =>
  val activityContainer: LinearLayout
  val notifyRestart: Int => Unit

  def showMnemonicInput(titleRes: Int)(proceedWithMnemonics: StringList => Unit): Unit = {
    val mnemonicWrap = getLayoutInflater.inflate(R.layout.frag_mnemonic, null).asInstanceOf[LinearLayout]
    val recoveryPhrase = mnemonicWrap.findViewById(R.id.recoveryPhrase).asInstanceOf[com.hootsuite.nachos.NachoTextView]
    recoveryPhrase.addChipTerminator(' ', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    recoveryPhrase.addChipTerminator(',', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    recoveryPhrase.addChipTerminator('\n', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    recoveryPhrase setAdapter new ArrayAdapter(me, android.R.layout.simple_list_item_1, englishWordList)

    def getMnemonicList: StringList = {
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

  lazy val enforceTor = new SettingsHolder(me) {
    override def updateView: Unit = settingsCheck.setChecked(WalletApp.ensureTor)

    settingsTitle.setText(settings_ensure_tor)
    setVis(isVisible = false, settingsInfo)
    disableIfOldAndroid

    view setOnClickListener onButtonTap {
      putBoolAndUpdateView(WalletApp.ENSURE_TOR, !WalletApp.ensureTor)
      notifyRestart(settings_custom_electrum_restart_notice)
    }
  }

  lazy val electrum: SettingsHolder = new SettingsHolder(me) {
    setVis(isVisible = false, settingsCheck)

    override def updateView: Unit = WalletApp.customElectrumAddress match {
      case Success(nodeAddress) => setTexts(settings_custom_electrum_enabled, nodeAddress.toString)
      case _ => setTexts(settings_custom_electrum_disabled, me getString settings_custom_electrum_disabled_tip)
    }

    view setOnClickListener onButtonTap {
      val (container, extraInputLayout, extraInput, _, _) = singleInputPopup
      val builder = titleBodyAsViewBuilder(getString(settings_custom_electrum_disabled).asDefView, container)
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)
      extraInputLayout.setHint(settings_custom_electrum_host_port)
      showKeys(extraInput)

      def proceed: Unit = {
        val input = extraInput.getText.toString.trim
        def saveAddress(address: String) = WalletApp.app.prefs.edit.putString(WalletApp.CUSTOM_ELECTRUM, address)
        if (input.nonEmpty) runInFutureProcessOnUI(saveUnsafeElectrumAddress, onFail)(_ => warnAndUpdateView)
        else runAnd(saveAddress(new String).commit)(warnAndUpdateView)

        def saveUnsafeElectrumAddress: Unit = {
          val hostOrIP \ port = input.splitAt(input lastIndexOf ':')
          val nodeAddress = NodeAddress.fromParts(hostOrIP, port.tail.toInt, Domain)
          saveAddress(nodeaddress.encode(nodeAddress).require.toHex).commit
        }

        def warnAndUpdateView: Unit = {
          notifyRestart(settings_custom_electrum_restart_notice)
          updateView
        }
      }
    }

    def setTexts(titleRes: Int, info: String): Unit = {
      settingsTitle.setText(titleRes)
      settingsInfo.setText(info)
    }
  }
}

class SetupActivity extends BaseActivity with MnemonicActivity { me =>
  lazy val activityContainer = findViewById(R.id.activitySetupMain).asInstanceOf[LinearLayout]
  val notifyRestart: Int => Unit = none

  val proceedWithMnemonics: StringList => Unit = mnemonic => {
    val walletSeed = MnemonicCode.toSeed(mnemonic, passphrase = new String)
    val keys = LightningNodeKeys.fromSeed(walletSeed.toArray)
    val secret = WalletSecret(keys, mnemonic, walletSeed)
    WalletApp.extDataBag.putSecret(secret)
    WalletApp.makeOperational(secret)

    TransitionManager.beginDelayedTransition(activityContainer)
    activityContainer.setVisibility(View.GONE)
    exitTo(ClassNames.hubActivityClass)
  }

  override def START(s: Bundle): Unit = {
    setContentView(R.layout.activity_setup)
    activityContainer.addView(electrum.view, 0)
    activityContainer.addView(enforceTor.view, 0)
    enforceTor.updateView
    electrum.updateView
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
