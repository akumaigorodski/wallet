package com.btcontract.wallet

import android.os.Bundle
import android.widget._
import com.btcontract.wallet.BaseActivity.StringOps
import com.btcontract.wallet.Colors._
import com.btcontract.wallet.R.string._
import com.google.android.material.snackbar.Snackbar
import fr.acinq.bitcoin.{MnemonicCode, Satoshi}
import fr.acinq.eclair.MilliSatoshi
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.chainHash
import fr.acinq.eclair.blockchain.electrum.db.SigningWallet
import fr.acinq.eclair.blockchain.electrum.{ElectrumWallet, ElectrumWalletType}
import immortan.LightningNodeKeys
import immortan.crypto.Tools._
import immortan.utils.{BtcDenomination, SatDenomination}

class SettingsActivity extends BaseCheckActivity with MnemonicActivity with ChoiceReceiver { me =>
  lazy val activityContainer = findViewById(R.id.settingsContainer).asInstanceOf[LinearLayout]

  val notifyRestart: Int => Unit = message => {
    def onOk(snack: Snackbar): Unit = runAnd(snack.dismiss)(WalletApp.restart)
    snack(activityContainer, getString(message).html, R.string.dialog_ok, onOk)
  }

  private[this] val units = List(SatDenomination, BtcDenomination)
  private[this] val fiatSymbols = WalletApp.fiatRates.universallySupportedSymbols.toList.sorted
  private[this] val CHOICE_FIAT_DENOM_TAG = "choiceFiatDenominationTag"
  private[this] val CHOICE_DENOM_TAG = "choiceBtcDenominationTag"

  override def onResume: Unit = {
    chainWallets.updateView
    electrum.updateView
    setFiat.updateView
    setBtc.updateView
    super.onResume
  }

  override def onChoiceMade(tag: AnyRef, pos: Int): Unit = tag match {

    case CHOICE_FIAT_DENOM_TAG =>
      val fiatCode \ _ = fiatSymbols(pos)
      WalletApp.app.prefs.edit.putString(WalletApp.FIAT_CODE, fiatCode).commit
      immortan.sqlite.DbStreams.next(immortan.sqlite.DbStreams.txDbStream)
      setFiat.updateView


    case CHOICE_DENOM_TAG =>
      WalletApp.app.prefs.edit.putString(WalletApp.BTC_DENOM, units(pos).sign).commit
      immortan.sqlite.DbStreams.next(immortan.sqlite.DbStreams.txDbStream)
      setBtc.updateView

    case _ =>
  }

  lazy private[this] val chainWallets: SettingsHolder = new SettingsHolder(me) {
    setVisMany(false -> settingsCheck, false -> settingsInfo)
    settingsTitle.setText(settings_chain_wallets)

    view setOnClickListener onButtonTap {
      showMnemonicInput(action_recovery_phrase_title) { mnemonic =>
        val (container, extraInputLayout, extraInput, _, _) = singleInputPopup
        val keys = LightningNodeKeys.fromSeed(MnemonicCode.toSeed(mnemonic, new String).toArray)
        val bld = titleBodyAsViewBuilder(getString(settings_attached_label).asDefView, container)
        mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, bld, dialog_ok, dialog_cancel)
        extraInputLayout.setHint(dialog_set_label)
        showKeys(extraInput)

        def proceed: Unit = runAnd(finish) {
          if (WalletApp.secret.keys.master == keys.master) return
          val core = SigningWallet(ElectrumWallet.BIP84, attachedMaster = keys.master.asSome)
          val ewt = ElectrumWalletType.makeSigningType(core.walletType, master = keys.master, chainHash)
          val spec = ElectrumWallet.makeSigningWalletParts(core, ewt, Satoshi(0L), extraInput.getText.toString.trim)
          HubActivity.instance.walletCards.addChainCard(spec.data.keys.ewt.xPub)
          ElectrumWallet.addWallet(spec)
        }
      }
    }

    override def updateView: Unit = none
  }

  lazy private[this] val setFiat = new SettingsHolder(me) {
    settingsTitle.setText(settings_fiat_currency)
    setVis(isVisible = false, settingsCheck)

    view setOnClickListener onButtonTap {
      val options = fiatSymbols.map { case code \ name => code.toUpperCase + SEPARATOR + name }
      val list = getLayoutInflater.inflate(R.layout.frag_selector_list, null).asInstanceOf[ListView]
      list setAdapter new ArrayAdapter(me, android.R.layout.simple_expandable_list_item_1, options.toArray)
      new sheets.ChoiceBottomSheet(list, CHOICE_FIAT_DENOM_TAG, me).show(getSupportFragmentManager, "unused-tag")
    }

    override def updateView: Unit = settingsInfo.setText(WalletApp.fiatCode.toUpperCase)
  }

  lazy private[this] val setBtc = new SettingsHolder(me) {
    settingsTitle.setText(settings_btc_unit)
    setVis(isVisible = false, settingsCheck)

    view setOnClickListener onButtonTap {
      val list = getLayoutInflater.inflate(R.layout.frag_selector_list, null).asInstanceOf[ListView]
      val listOptions = for (unit <- units) yield unit.parsedWithSignTT(MilliSatoshi(526800020L), cardIn, cardZero).html
      list setAdapter new ArrayAdapter(me, android.R.layout.simple_expandable_list_item_1, listOptions.toArray)
      new sheets.ChoiceBottomSheet(list, CHOICE_DENOM_TAG, me).show(getSupportFragmentManager, "unused-tag")
    }

    override def updateView: Unit = {
      val short = WalletApp.denom.sign.toUpperCase
      val isSatDenom = WalletApp.denom == SatDenomination
      val text = if (isSatDenom) s"Satoshi ($short)" else s"Bitcoin ($short)"
      settingsInfo.setText(text)
    }
  }

  lazy private[this] val viewCode = new SettingsHolder(me) {
    setVisMany(false -> settingsCheck, false -> settingsInfo)
    view setOnClickListener onButtonTap(viewRecoveryCode)
    settingsTitle.setText(settings_view_revocery_phrase)
    override def updateView: Unit = none
  }

  override def PROCEED(state: Bundle): Unit = {
    setContentView(R.layout.activity_settings)

    val title = new TitleView("-= <strong>S</strong>imple <strong>B</strong>itcoin <strong>W</strong>allet =-")
    addFlowChip(title.flow, "Sources", R.drawable.border_gray, _ => me browse "https://github.com/akumaigorodski/wallet")
    addFlowChip(title.flow, "Manual", R.drawable.border_gray, _ => me browse "https://sbw.app/posts/manual")

    activityContainer.addView(title.view)
    activityContainer.addView(viewCode.view)

    activityContainer.addView(chainWallets.view)
    activityContainer.addView(electrum.view)
    activityContainer.addView(setFiat.view)
    activityContainer.addView(setBtc.view)
  }
}
