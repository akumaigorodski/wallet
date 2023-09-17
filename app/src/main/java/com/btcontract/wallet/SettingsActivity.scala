package com.btcontract.wallet

import android.os.{Build, Bundle}
import android.view.View
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
import fr.acinq.eclair.blockchain.EclairWallet
import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.eclair.blockchain.electrum.ElectrumEclairWallet
import fr.acinq.eclair.blockchain.electrum.db.{SigningWallet, WatchingWallet}
import fr.acinq.eclair.wire.CommonCodecs.nodeaddress
import fr.acinq.eclair.wire.{Domain, NodeAddress}
import immortan.{LightningNodeKeys, WalletParams}
import immortan.crypto.Tools._
import immortan.utils.{BtcDenomination, SatDenomination}

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

class SettingsActivity extends BaseCheckActivity with MnemonicActivity with ChoiceReceiver { me =>
  lazy private[this] val settingsContainer = findViewById(R.id.settingsContainer).asInstanceOf[LinearLayout]
  private[this] val fiatSymbols = WalletParams.fiatRates.universallySupportedSymbols.toList.sorted
  private[this] val CHOICE_FIAT_DENOM_TAG = "choiceFiatDenominationTag"
  private[this] val CHOICE_DENOM_TAG = "choiceBtcDenominationTag"
  private[this] val units = List(SatDenomination, BtcDenomination)

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
      val fiatCode ~ _ = fiatSymbols(pos)
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
      addFlowChip(options.flow, getString(settings_add_hardware_wallet), R.drawable.border_yellow, startHardware)
      addFlowChip(options.flow, getString(settings_add_recovery_phrase), R.drawable.border_yellow, startMnemonic)
      sheet.view.setBackgroundResource(R.color.almostBlack)
      sheet.view.addView(options.view)

      // Show options for attached BIP39 wallets
      WalletParams.chainWallets.usableWallets.collect {
        case wallet if BIP84 == wallet.info.core.walletType && wallet.info.core.attachedMaster.isDefined =>
          makeSigningWalletTypes(sheet.view, wallet.info.label, wallet.info.core.attachedMaster.get)
      }

      // And finally show options for built-in BIP39 wallet for which we store a mnemonic phrase
      makeSigningWalletTypes(sheet.view, getString(bitcoin_wallet), WalletParams.secret.keys.master)
      sheet.show(getSupportFragmentManager, "utag")
    }
  }

  lazy private[this] val electrum: SettingsHolder = new SettingsHolder(me) {
    setVis(isVisible = false, settingsCheck)

    override def updateView: Unit = WalletApp.customElectrumAddress match {
      case Success(nodeAddress) => setTexts(settings_custom_electrum_enabled, nodeAddress.toString)
      case _ => setTexts(settings_custom_electrum_disabled, me getString settings_custom_electrum_disabled_tip)
    }

    view setOnClickListener onButtonTap {
      val (container, extraInputLayout, extraInput) = singleInputPopup
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
          val hostOrIP ~ port = input.splitAt(input lastIndexOf ':')
          val nodeAddress = NodeAddress.fromParts(hostOrIP, port.tail.toInt, Domain)
          saveAddress(nodeaddress.encode(nodeAddress).require.toHex).commit
        }

        def warnAndUpdateView: Unit = {
          def onOk(snack: Snackbar): Unit = runAnd(snack.dismiss)(WalletApp.restart)
          val message = getString(settings_custom_electrum_restart_notice).html
          snack(settingsContainer, message, R.string.dialog_ok, onOk)
          updateView
        }
      }
    }

    def setTexts(titleRes: Int, info: String): Unit = {
      settingsTitle.setText(titleRes)
      settingsInfo.setText(info)
    }
  }

  lazy private[this] val setFiat = new SettingsHolder(me) {
    settingsTitle.setText(settings_fiat_currency)
    setVis(isVisible = false, settingsCheck)

    view setOnClickListener onButtonTap {
      val options = fiatSymbols.map { case code ~ name => code.toUpperCase + SEPARATOR + name }
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
      val listOptions = for (unit <- units) yield unit.parsedWithSign(MilliSatoshi(526800020L), cardIn, cardZero).html
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

  lazy private[this] val enforceTor = new SettingsHolder(me) {
    override def updateView: Unit = settingsCheck.setChecked(WalletApp.ensureTor)

    settingsTitle.setText(settings_ensure_tor)
    setVis(isVisible = false, settingsInfo)
    disableIfOldAndroid

    view setOnClickListener onButtonTap {
      putBoolAndUpdateView(WalletApp.ENSURE_TOR, !WalletApp.ensureTor)
      def onOk(snack: Snackbar): Unit = runAnd(snack.dismiss)(WalletApp.restart)
      val message = getString(settings_custom_electrum_restart_notice).html
      snack(settingsContainer, message, R.string.dialog_ok, onOk)
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

    val settingsPageitle = new TitleView(s"v$VERSION_NAME-$VERSION_CODE")
    settingsPageitle.view.setOnClickListener(me onButtonTap finish)
    settingsPageitle.backArrow.setVisibility(View.VISIBLE)

    val links = new TitleView("<strong>S</strong>imple <strong>B</strong>itcoin <strong>W</strong>allet")
    addFlowChip(links.flow, getString(manual), R.drawable.border_green, _ => me browse "https://sbw.app/posts/manual")
    addFlowChip(links.flow, getString(sources), R.drawable.border_green, _ => me browse "https://github.com/akumaigorodski/wallet")
    addFlowChip(links.flow, "Nostr", R.drawable.border_green, _ => me browse "https://njump.me/npub1chxa2um7gl65ymyaagjrqys39mtzlwnm2drcs6qkqmme7k4edq4qghrjdd")

    for (count <- WalletParams.logBag.count if count > 0) {
      def exportLog: Unit = me share WalletParams.logBag.recent.map(_.asString).mkString("\n\n")
      val errorCount = s"${me getString error_log} <font color=$cardZero>$count</font>"
      addFlowChip(links.flow, errorCount, R.drawable.border_yellow, _ => exportLog)
    }

    settingsContainer.addView(settingsPageitle.view)
    settingsContainer.addView(links.view)

    settingsContainer.addView(enforceTor.view)
    settingsContainer.addView(viewCode.view)

    settingsContainer.addView(chainWallets.view)
    settingsContainer.addView(electrum.view)
    settingsContainer.addView(setFiat.view)
    settingsContainer.addView(setBtc.view)
  }

  // Signing wallet options

  def callMnemonic: Unit =
    showMnemonicInput(settings_add_recovery_phrase) { mnemonic =>
      val (container, extraInputLayout, extraInput) = singleInputPopup
      val bld = titleBodyAsViewBuilder(getString(settings_attached_label).asDefView, container)
      val keys = LightningNodeKeys.makeFromSeed(MnemonicCode.toSeed(mnemonic, new String).toArray)
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, bld, dialog_ok, dialog_cancel)
      extraInputLayout.setHint(dialog_set_label)
      showKeys(extraInput)

      def proceed: Unit = runAnd(finish) {
        if (WalletParams.secret.keys.master != keys.master) {
          val label = extraInput.getText.toString.trim.asSome.filter(_.nonEmpty).getOrElse(EclairWallet.BIP84)
          val core = SigningWallet(walletType = BIP84, attachedMaster = Some(keys.master), isRemovable = true)
          val wallet = WalletParams.chainWallets.makeSigningWalletParts(core, keys.master, Satoshi(0L), label)
          HubActivity.instance.walletCards.resetChainCards(WalletParams.chainWallets withFreshWallet wallet)
        }
      }
    }

  def callUrScanner: Unit = {
    def onKey(data: PairingData): Unit = {
      val (container, extraInputLayout, extraInput) = singleInputPopup
      val bld = titleBodyAsViewBuilder(getString(settings_hardware_label).asDefView, container)
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, bld, dialog_ok, dialog_cancel)
      extraInputLayout.setHint(dialog_set_label)
      showKeys(extraInput)

      def proceed: Unit = runAnd(finish) {
        if (WalletParams.chainWallets.findByPubKey(data.bip84XPub.publicKey).isEmpty) {
          val label = extraInput.getText.toString.trim.asSome.filter(_.nonEmpty).getOrElse(EclairWallet.BIP84)
          val core = WatchingWallet(EclairWallet.BIP84, data.masterFingerprint, data.bip84XPub, isRemovable = true)
          val wallet = WalletParams.chainWallets.makeWatchingWallet84Parts(core, lastBalance = Satoshi(0L), label)
          HubActivity.instance.walletCards.resetChainCards(WalletParams.chainWallets withFreshWallet wallet)
        }
      }
    }

    val sheet = new sheets.URBottomSheet(me, onKey)
    callScanner(sheet)
  }

  def makeSigningWalletTypes(host: LinearLayout, title: String, master: ExtendedPrivateKey): Unit = {
    def find(walletType: String): Option[ElectrumEclairWallet] = WalletParams.chainWallets.wallets.find {
      wallet => wallet.ewt.secrets.exists(_.master == master) && wallet.info.core.walletType == walletType
    }

    val ws = for (Tuple2(tag, info ~ path) <- wallets) yield s"<b>$tag</b> <i>$path</i><br>$info".html
    val list = getLayoutInflater.inflate(R.layout.frag_selector_list, null).asInstanceOf[ListView]
    list setAdapter new ArrayAdapter(me, android.R.layout.select_dialog_multichoice, ws.toArray)

    val listener = new OnListItemClickListener {
      def onItemClicked(itemPosition: Int): Unit = {
        val attachedMaster = if (master == WalletParams.secret.keys.master) None else Some(master)
        val core = SigningWallet(possibleKeys(itemPosition), attachedMaster, isRemovable = true)

        if (list isItemChecked itemPosition) {
          val wallet = WalletParams.chainWallets.makeSigningWalletParts(core, master, Satoshi(0L), core.walletType)
          HubActivity.instance.walletCards.resetChainCards(WalletParams.chainWallets withFreshWallet wallet)
        } else {
          val extOpt = find(core.walletType).map(WalletParams.chainWallets.withoutWallet)
          extOpt.foreach(HubActivity.instance.walletCards.resetChainCards)
        }
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
    BIP32 -> ("Legacy SBW, BRD, Mycelium", "m/0'/0/n"),
    BIP44 -> ("Bitcoin.com, Trust wallet, Exodus", "m/44'/0'/0'/0/n"),
    BIP49 -> ("JoinMarket", "m/49'/0'/0'/0/n")
  )

  private val possibleKeys: List[String] =
    wallets.keys.toList
}
