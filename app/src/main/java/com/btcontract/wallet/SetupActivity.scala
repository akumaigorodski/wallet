package com.btcontract.wallet

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.View
import android.widget.{ArrayAdapter, LinearLayout}
import androidx.appcompat.app.AlertDialog
import androidx.documentfile.provider.DocumentFile
import androidx.transition.TransitionManager
import com.btcontract.wallet.utils.LocalBackup
import com.google.common.io.ByteStreams
import com.ornach.nobobutton.NoboButton
import fr.acinq.bitcoin.MnemonicCode
import immortan.crypto.Tools.{SEPARATOR, none}
import immortan.wire.ExtCodecs
import immortan.{LNParams, LightningNodeKeys, WalletSecret}
import scodec.bits.{BitVector, ByteVector}

import scala.util.{Failure, Success}


object SetupActivity {
  def fromMnemonics(mnemonics: List[String], host: BaseActivity): Unit = {
    val walletSeed = MnemonicCode.toSeed(mnemonics, passphrase = new String)
    val keys = LightningNodeKeys.makeFromSeed(seed = walletSeed.toArray)
    val secret = WalletSecret(keys, mnemonics, walletSeed)

    try {
      // Implant graph into db file from resources
      val snapshotName = LocalBackup.getGraphResourceName(LNParams.chainHash)
      val compressedPlainBytes = ByteStreams.toByteArray(host.getAssets open snapshotName)
      val plainBytes = ExtCodecs.compressedByteVecCodec.decode(BitVector view compressedPlainBytes)
      LocalBackup.copyPlainDataToDbLocation(host, WalletApp.dbFileNameGraph, plainBytes.require.value)
    } catch none

    WalletApp.extDataBag.putSecret(secret)
    WalletApp.makeOperational(secret)
  }
}

class SetupActivity extends BaseActivity { me =>
  override def START(state: Bundle): Unit = setContentView(R.layout.activity_setup)

  private[this] lazy val activitySetupMain = findViewById(R.id.activitySetupMain).asInstanceOf[LinearLayout]
  private[this] lazy val restoreOptionsButton = findViewById(R.id.restoreOptionsButton).asInstanceOf[NoboButton]
  private[this] lazy val restoreOptions = findViewById(R.id.restoreOptions).asInstanceOf[LinearLayout]
  private[this] final val FILE_REQUEST_CODE = 112

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

  override def onActivityResult(requestCode: Int, resultCode: Int, resultData: Intent): Unit =
    if (requestCode == FILE_REQUEST_CODE && resultCode == Activity.RESULT_OK && resultData != null) {
      val cipherBytes = ByteStreams.toByteArray(getContentResolver openInputStream resultData.getData)

      showMnemonicPopup(R.string.action_backup_present_title) { mnemonics =>
        val walletSeed = MnemonicCode.toSeed(mnemonics, passphrase = new String)
        LocalBackup.decryptBackup(ByteVector.view(cipherBytes), walletSeed) match {

          case Success(plainEssentialBytes) =>
            // We were able to decrypt a file, implant it into db location and proceed
            LocalBackup.copyPlainDataToDbLocation(me, WalletApp.dbFileNameEssential, plainEssentialBytes)
            // Delete user-selected backup file while we can here and make an app-owned backup shortly
            DocumentFile.fromSingleUri(me, resultData.getData).delete
            WalletApp.backupSaveWorker.replaceWork(true)
            proceedWithMnemonics(mnemonics)

          case Failure(exception) =>
            val msg = getString(R.string.error_could_not_decrypt)
            onFail(msg format exception.getMessage)
        }
      }
    }

  def createNewWallet(view: View): Unit = {
    val twelveWordsEntropy: ByteVector = fr.acinq.eclair.randomBytes(16)
    val mnemonic = MnemonicCode.toMnemonics(twelveWordsEntropy, englishWordList)
    proceedWithMnemonics(mnemonic)
  }

  def showRestoreOptions(view: View): Unit = {
    TransitionManager.beginDelayedTransition(activitySetupMain)
    restoreOptionsButton.setVisibility(View.GONE)
    restoreOptions.setVisibility(View.VISIBLE)
  }

  def useBackupFile(view: View): Unit = startActivityForResult(new Intent(Intent.ACTION_OPEN_DOCUMENT).setType("*/*"), FILE_REQUEST_CODE)

  def useRecoveryPhrase(view: View): Unit = showMnemonicPopup(R.string.action_recovery_phrase_title)(proceedWithMnemonics)

  def showMnemonicPopup(title: Int)(onMnemonic: List[String] => Unit): Unit = {
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
      onMnemonic(getMnemonicList)
      alert.dismiss
    } catch {
      case exception: Throwable =>
        val msg = getString(R.string.error_wrong_phrase)
        onFail(msg format exception.getMessage)
    }

    val builder = titleBodyAsViewBuilder(getString(title).asDefView, mnemonicWrap)
    val alert = mkCheckForm(proceed, none, builder, R.string.dialog_ok, R.string.dialog_cancel)
    updatePopupButton(getPositiveButton(alert), isEnabled = false)

    recoveryPhrase addTextChangedListener onTextChange { _ =>
      updatePopupButton(getPositiveButton(alert), getMnemonicList.size > 11)
    }
  }
}
