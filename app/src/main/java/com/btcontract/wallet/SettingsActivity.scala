package com.btcontract.wallet

import android.os.Bundle
import android.widget._
import com.btcontract.wallet.BaseActivity.StringOps
import com.btcontract.wallet.BuildConfig.{VERSION_CODE, VERSION_NAME}
import com.btcontract.wallet.Colors._
import com.btcontract.wallet.R.string._
import com.btcontract.wallet.sheets.{LinearBottomSheet, PairingData}
import com.btcontract.wallet.utils.OnListItemClickListener
import com.google.android.material.snackbar.Snackbar
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPrivateKey
import fr.acinq.bitcoin.{MnemonicCode, Satoshi}
import fr.acinq.eclair.MilliSatoshi
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.chainHash
import fr.acinq.eclair.blockchain.electrum.db.{SigningWallet, WatchingWallet}
import fr.acinq.eclair.blockchain.electrum.{ElectrumWallet, ElectrumWalletType, WalletSpec}
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
    enforceTor.updateView
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
    override def updateView: Unit = none

    view setOnClickListener onButtonTap {
      val sheet = new LinearBottomSheet(R.layout.frag_linear_layout, me)
      val options = new TitleView(getString(settings_chain_wallets).trim)

      val startHardware: String => Unit = { _ =>
        timer.schedule(UITask(callUrScanner), 225)
        sheet.dismiss
      }

      val startMnemonic: String => Unit = { _ =>
        timer.schedule(UITask(callMnemonic), 225)
        sheet.dismiss
      }

      // Allow user to add a hardware wallet or another BIP39-based wallet
      addFlowChip(options.flow, getString(settings_add_hardware_wallet), R.drawable.border_green, startHardware).setTextSize(18f)
      addFlowChip(options.flow, getString(settings_add_recovery_phrase), R.drawable.border_green, startMnemonic).setTextSize(18f)
      sheet.view.setBackgroundResource(com.btcontract.wallet.R.color.chip_default_text_color)
      sheet.view.addView(options.view)

      ElectrumWallet.specs.values.collect {
        case spec if spec.data.keys.ewt.secrets.isDefined =>
          val master = spec.info.core.attachedMaster.getOrElse(WalletApp.secret.keys.master)
          makeSigningWalletTypes(sheet.view, title = spec.info.label, master)
      }

      sheet.show(getSupportFragmentManager, "utag")
    }
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

    val title = new TitleView(s"<strong>S</strong>imple <strong>B</strong>itcoin <strong>W</strong>allet $VERSION_NAME-$VERSION_CODE")
    addFlowChip(title.flow, "Nostr", R.drawable.border_gray, _ => me browse "https://njump.me/npub1chxa2um7gl65ymyaagjrqys39mtzlwnm2drcs6qkqmme7k4edq4qghrjdd")
    addFlowChip(title.flow, "Sources", R.drawable.border_gray, _ => me browse "https://github.com/akumaigorodski/wallet")
    addFlowChip(title.flow, "Manual", R.drawable.border_gray, _ => me browse "https://sbw.app/posts/manual")

    activityContainer.addView(title.view)
    activityContainer.addView(enforceTor.view)
    activityContainer.addView(viewCode.view)

    activityContainer.addView(chainWallets.view)
    activityContainer.addView(electrum.view)
    activityContainer.addView(setFiat.view)
    activityContainer.addView(setBtc.view)
  }

  // Signing wallet options

  def callMnemonic: Unit =
    showMnemonicInput(settings_add_recovery_phrase) { mnemonic =>
      val (container, extraInputLayout, extraInput, _, _) = singleInputPopup
      val bld = titleBodyAsViewBuilder(getString(settings_attached_label).asDefView, container)
      val keys = LightningNodeKeys.fromSeed(MnemonicCode.toSeed(mnemonic, new String).toArray)
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, bld, dialog_ok, dialog_cancel)
      extraInputLayout.setHint(dialog_set_label)
      showKeys(extraInput)

      def proceed: Unit = runAnd(finish) {
        if (WalletApp.secret.keys.master != keys.master) {
          val core = SigningWallet(ElectrumWallet.BIP84, attachedMaster = keys.master.asSome)
          val ewt = ElectrumWalletType.makeSigningType(core.walletType, master = keys.master, chainHash)
          ElectrumWallet addWallet ElectrumWallet.makeSigningWalletParts(core, ewt, Satoshi(0L), extraInput.getText.toString.trim)
          HubActivity.instance.walletCards.resetChainCards
        }
      }
    }

  def callUrScanner: Unit = {
    def onKey(data: PairingData): Unit = {
      val (container, extraInputLayout, extraInput, _, _) = singleInputPopup
      val bld = titleBodyAsViewBuilder(getString(settings_hardware_label).asDefView, container)
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, bld, dialog_ok, dialog_cancel)
      extraInputLayout.setHint(dialog_set_label)
      showKeys(extraInput)

      def proceed: Unit = runAnd(finish) {
        if (ElectrumWallet.specs.get(data.bip84XPub).isEmpty) {
          val core = WatchingWallet(ElectrumWallet.BIP84, masterFingerprint = data.masterFingerprint, data.bip84XPub)
          ElectrumWallet addWallet ElectrumWallet.makeWatchingWallet84Parts(core, Satoshi(0L), extraInput.getText.toString.trim)
          HubActivity.instance.walletCards.resetChainCards
        }
      }
    }

    val sheet = new sheets.URBottomSheet(me, onKey)
    callScanner(sheet)
  }

  def makeSigningWalletTypes(host: LinearLayout, title: String, master: ExtendedPrivateKey): Unit = {
    def find(walletType: String): Option[WalletSpec] = ElectrumWallet.specs.values.find { spec =>
      spec.data.keys.ewt.secrets.exists(_.master == master) && spec.info.core.walletType == walletType
    }

    val ws = for (Tuple2(tag, info \ path) <- wallets) yield s"&#160;<b>$tag</b> <i>$path</i><br>&#160;$info".html
    val list = getLayoutInflater.inflate(R.layout.frag_selector_list, null).asInstanceOf[ListView]
    list setAdapter new ArrayAdapter(me, android.R.layout.select_dialog_multichoice, ws.toArray)

    val listener = new OnListItemClickListener {
      def onItemClicked(itemPosition: Int): Unit = {
        val attachedMaster = if (master == WalletApp.secret.keys.master) None else Some(master)
        val core = SigningWallet(possibleKeys(itemPosition), attachedMaster)

        if (list isItemChecked itemPosition) {
          val ewt = ElectrumWalletType.makeSigningType(core.walletType, master, chainHash)
          ElectrumWallet addWallet ElectrumWallet.makeSigningWalletParts(core, ewt, Satoshi(0L), core.walletType)
        } else find(core.walletType).foreach(spec => ElectrumWallet removeWallet spec.data.keys.ewt.xPub)
        HubActivity.instance.walletCards.resetChainCards
      }
    }

    list.setOnItemClickListener(listener)
    list.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE)
    host.addView(new TitleView(title).view)
    host.addView(list)

    for {
      walletType <- possibleKeys if find(walletType).isDefined
    } list.setItemChecked(possibleKeys.indexOf(walletType), true)
  }

  private val wallets = Map(
    ElectrumWallet.BIP84 -> ("Modern SBW", "m/84'/0'/0'/0/n"),
    ElectrumWallet.BIP32 -> ("Legacy SBW, BRD, Mycelium", "m/0'/0/n"),
    ElectrumWallet.BIP44 -> ("Bitcoin.com, Trust wallet, Exodus", "m/44'/0'/0'/0/n")
  )

  private val possibleKeys: StringList =
    wallets.keys.toList
}
