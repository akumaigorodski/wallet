package com.btcontract.wallet

import android.widget._
import immortan.crypto.Tools._
import com.btcontract.wallet.Colors._
import com.btcontract.wallet.R.string._
import fr.acinq.eclair.blockchain.EclairWallet._

import android.view.{View, ViewGroup}
import immortan.{ChannelMaster, LNParams}
import fr.acinq.eclair.wire.{Domain, NodeAddress}
import immortan.utils.{BtcDenomination, SatDenomination}
import com.btcontract.wallet.BuildConfig.{VERSION_CODE, VERSION_NAME}
import com.btcontract.wallet.utils.{LocalBackup, OnListItemClickListener}
import com.btcontract.wallet.sheets.BaseChoiceBottomSheet
import com.google.android.material.textfield.TextInputLayout
import fr.acinq.eclair.blockchain.electrum.db.SigningWallet
import com.btcontract.wallet.BaseActivity.StringOps
import fr.acinq.eclair.wire.CommonCodecs.nodeaddress
import com.google.android.material.snackbar.Snackbar
import androidx.appcompat.app.AlertDialog
import fr.acinq.eclair.MilliSatoshi
import android.content.Intent
import scala.util.Success
import android.os.Bundle


class SettingsActivity extends BaseActivity with HasTypicalChainFee with ChoiceReceiver { me =>
  lazy private[this] val settingsContainer = findViewById(R.id.settingsContainer).asInstanceOf[LinearLayout]
  private[this] val fiatSymbols = LNParams.fiatRates.universallySupportedSymbols.toList.sorted
  private[this] val CHOICE_FIAT_DENOMINATION_TAG = "choiceFiatDenominationTag"
  private[this] val CHOICE_BTC_DENOMINATON_TAG = "choiceBtcDenominationTag"
  private[this] val units = List(SatDenomination, BtcDenomination)

  override def onResume: Unit = {
    storeLocalBackup.updateView
    chainWallets.updateView
    electrum.updateView
    setFiat.updateView
    setBtc.updateView

    useFingerprint.updateView
    enforceTor.updateView
    capLnFees.updateView
    super.onResume
  }

  override def onChoiceMade(tag: AnyRef, pos: Int): Unit = tag match {

    case CHOICE_FIAT_DENOMINATION_TAG =>
      val fiatCode ~ _ = fiatSymbols(pos)
      WalletApp.app.prefs.edit.putString(WalletApp.FIAT_CODE, fiatCode).commit
      ChannelMaster.next(ChannelMaster.stateUpdateStream)
      setFiat.updateView

    case CHOICE_BTC_DENOMINATON_TAG =>
      WalletApp.app.prefs.edit.putString(WalletApp.BTC_DENOM, units(pos).sign).commit
      ChannelMaster.next(ChannelMaster.stateUpdateStream)
      capLnFees.updateView
      setBtc.updateView

    case _ =>
  }

  lazy private[this] val storeLocalBackup = new SettingsHolder {
    setVis(isVisible = false, settingsCheck)

    def updateView: Unit = {
      val backupAllowed = LocalBackup.isAllowed(context = WalletApp.app)
      if (backupAllowed && LNParams.cm.all.nonEmpty) WalletApp.backupSaveWorker.replaceWork("SETTINGS-INIT-SAVE-BACKUP")
      val title = if (backupAllowed) settings_backup_enabled else settings_backup_disabled
      val info = if (backupAllowed) settings_backup_where else settings_backup_how
      settingsTitle.setText(title)
      settingsInfo.setText(info)
    }

    view setOnClickListener onButtonTap {
      val intent = (new Intent).setAction(android.provider.Settings.ACTION_APPLICATION_DETAILS_SETTINGS)
      val intent1 = intent setData android.net.Uri.fromParts("package", getPackageName, null)
      startActivity(intent1)
    }
  }

  lazy private[this] val chainWallets: SettingsHolder = new SettingsHolder {
    settingsTitle.setText(settings_chain_wallets)
    setVis(isVisible = false, settingsCheck)

    override def updateView: Unit = {
      val usedWalletTags = LNParams.chainWallets.wallets.map(_.info.core.walletType)
      val text = usedWalletTags.mkString(s" <font color=$cardZero>/</font> ")
      settingsInfo.setText(text.html)
    }

    private val wallets = Map(
      BIP32 -> ("BRD, legacy wallet", "m/0'/0/n"),
      BIP44 -> ("Bitcoin.com, Mycelium, Exodus...", "m/44'/0'/0'/0/n"),
      BIP49 -> ("JoinMarket, Eclair Mobile, Pine...", "m/49'/0'/0'/0/n"),
      BIP84 -> (getString(settings_chain_modern), "m/84'/0'/0'/0/n")
    )

    val possibleKeys: List[String] = wallets.keys.toList
    val hardcodedPosition: Int = possibleKeys.indexOf(BIP84)

    view setOnClickListener onButtonTap {
      val options = for (Tuple2(tag, info ~ path) <- wallets) yield s"<b>$tag</b> <i>$path</i><br>$info".html
      val adapter = new ArrayAdapter(me, android.R.layout.select_dialog_multichoice, options.toArray) {
        override def isEnabled(itemPosition: Int): Boolean = itemPosition != hardcodedPosition

        override def getView(itemPosition: Int, itemConvertedView: View, itemParentGroup: ViewGroup): View = {
          val bgColor = if (itemPosition == hardcodedPosition) R.color.almostBlack else android.R.color.transparent
          val finalView = super.getView(itemPosition, itemConvertedView, itemParentGroup)
          finalView.setBackgroundResource(bgColor)
          finalView
        }
      }

      val list = selectorList(adapter)
      val listener = new OnListItemClickListener {
        def onItemClicked(itemPosition: Int): Unit = {
          val core = SigningWallet(possibleKeys(itemPosition), isRemovable = true)
          if (list isItemChecked itemPosition) LNParams.chainWallets.withNewSigning(core, core.walletType).asSome.foreach(LNParams.updateChainWallet)
          else LNParams.chainWallets.findByTag(core.walletType).map(LNParams.chainWallets.withoutWallet).foreach(LNParams.updateChainWallet)
          // Call this after chain wallets have been updated to redraw them on hub activity
          HubActivity.chainWalletStream.onNext(LNParams.chainWallets)
          ChannelMaster.next(ChannelMaster.statusUpdateStream)
          updateView
        }
      }

      list.setOnItemClickListener(listener)
      list.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE)
      new BaseChoiceBottomSheet(list).show(getSupportFragmentManager, "unused-wallet-type-tag")
      for (wallet <- LNParams.chainWallets.wallets) list.setItemChecked(possibleKeys.indexOf(wallet.info.core.walletType), true)
    }
  }

  lazy private[this] val electrum: SettingsHolder = new SettingsHolder {
    override def updateView: Unit = WalletApp.customElectrumAddress match {
      case Success(nodeAddress) => setTexts(settings_custom_electrum_enabled, nodeAddress.toString)
      case _ => setTexts(settings_custom_electrum_disabled, me getString settings_custom_electrum_disabled_tip)
    }

    setVis(isVisible = false, settingsCheck)

    view setOnClickListener onButtonTap {
      val title = getString(settings_custom_electrum_disabled).asDefView
      val container = getLayoutInflater.inflate(R.layout.frag_hint_input, null, false)
      val extraInputLayout = container.findViewById(R.id.extraInputLayout).asInstanceOf[TextInputLayout]
      val extraInput = container.findViewById(R.id.extraInput).asInstanceOf[EditText]

      def warnAndUpdateView: Unit = {
        def onOk(snack: Snackbar): Unit = runAnd(snack.dismiss)(WalletApp.restartApplication)
        snack(settingsContainer, getString(settings_custom_electrum_restart_notice).html, R.string.dialog_ok, onOk)
        updateView
      }

      def saveUnsafe: Unit = {
        val rawText = extraInput.getText.toString.trim
        val hostOrIP ~ port = rawText.splitAt(rawText lastIndexOf ':')
        val nodeAddress = NodeAddress.fromParts(host = hostOrIP, port = port.tail.toInt, orElse = Domain)
        WalletApp.app.prefs.edit.putString(WalletApp.CUSTOM_ELECTRUM_ADDRESS, nodeaddress.encode(nodeAddress).require.toHex).commit
      }

      def proceed(alert: AlertDialog): Unit = {
        if (extraInput.getText.toString.trim.nonEmpty) runInFutureProcessOnUI(saveUnsafe, onFail)(_ => warnAndUpdateView)
        else runAnd(WalletApp.app.prefs.edit.putString(WalletApp.CUSTOM_ELECTRUM_ADDRESS, new String).commit)(warnAndUpdateView)
        alert.dismiss
      }

      val builder = titleBodyAsViewBuilder(title, container)
      extraInputLayout.setHint(settings_custom_electrum_host_port)
      mkCheckForm(proceed, none, builder, dialog_ok, dialog_cancel)
    }

    def setTexts(titleRes: Int, info: String): Unit = {
      settingsTitle.setText(titleRes)
      settingsInfo.setText(info)
    }
  }

  lazy private[this] val setFiat = new SettingsHolder {
    override def updateView: Unit = settingsInfo.setText(WalletApp.fiatCode.toUpperCase)

    settingsTitle.setText(settings_fiat_currency)
    setVis(isVisible = false, settingsCheck)

    view setOnClickListener onButtonTap {
      val options = fiatSymbols.map { case code ~ name => code.toUpperCase + SEPARATOR + name }
      val list = me selectorList new ArrayAdapter(me, android.R.layout.simple_expandable_list_item_1, options.toArray)
      new sheets.ChoiceBottomSheet(list, CHOICE_FIAT_DENOMINATION_TAG, me).show(getSupportFragmentManager, "unused-tag")
    }
  }

  lazy private[this] val setBtc = new SettingsHolder {
    override def updateView: Unit = settingsInfo.setText(WalletApp.denom.sign.toUpperCase)

    settingsTitle.setText(settings_btc_unit)
    setVis(isVisible = false, settingsCheck)

    view setOnClickListener onButtonTap {
      val amounts = for (unit <- units) yield unit.parsedWithSign(MilliSatoshi(526800020L), cardIn, cardZero)
      val options = units.zip(amounts).map { case denom ~ amount => s"${denom.sign.toUpperCase} $amount".html }
      val list = me selectorList new ArrayAdapter(me, android.R.layout.simple_expandable_list_item_1, options.toArray)
      new sheets.ChoiceBottomSheet(list, CHOICE_BTC_DENOMINATON_TAG, me).show(getSupportFragmentManager, "unused-tag")
    }
  }

  lazy private[this] val useFingerprint: SettingsHolder = new SettingsHolder { self =>
    private def makeAttempt: Unit = new utils.BiometricAuth(findViewById(R.id.mainLayout), me) {
      def onHardwareUnavailable: Unit = WalletApp.app.quickToast(R.string.settings_auth_not_available)
      def onNoneEnrolled: Unit = WalletApp.app.quickToast(R.string.settings_auth_add_method)
      def onNoHardware: Unit = WalletApp.app.quickToast(R.string.settings_auth_no_support)
      def onAuthSucceeded: Unit = putBoolAndUpdateView(WalletApp.USE_AUTH, value = true)
      def onCanAuthenticate: Unit = callAuthDialog
    }.checkAuth

    def updateView: Unit = settingsCheck.setChecked(WalletApp.useAuth)

    view setOnClickListener onButtonTap {
      if (WalletApp.useAuth) putBoolAndUpdateView(WalletApp.USE_AUTH, value = false) else makeAttempt
    }

    settingsTitle.setText(settings_use_auth)
    setVis(isVisible = false, settingsInfo)
  }

  lazy private[this] val enforceTor = new SettingsHolder {
    settingsTitle.setText(settings_ensure_tor)

    view setOnClickListener onButtonTap {
      putBoolAndUpdateView(WalletApp.ENSURE_TOR, !WalletApp.ensureTor)
    }

    override def updateView: Unit = {
      val status = if (WalletApp.ensureTor) settings_ensure_tor_enabled else settings_ensure_tor_disabled
      settingsCheck.setChecked(WalletApp.ensureTor)
      settingsInfo.setText(status)
    }
  }

  lazy private[this] val capLnFees = new SettingsHolder {
    settingsTitle.setText(settings_ln_fee_cap)

    override def updateView: Unit = {
      val maxPercent = (100 * LNParams.maxOffChainFeeRatio).toLong + "%"
      val typicalFee = WalletApp.denom.parsedWithSign(typicalChainTxFee, cardIn, cardZero)
      val disabledText = getString(settings_ln_fee_cap_disabled).format(maxPercent, typicalFee)
      val enabledText = getString(settings_ln_fee_cap_enabled).format(maxPercent, typicalFee)
      val info = if (WalletApp.capLNFeeToChain) enabledText else disabledText
      settingsCheck.setChecked(WalletApp.capLNFeeToChain)
      settingsInfo.setText(info.html)
    }

    view setOnClickListener onButtonTap {
      putBoolAndUpdateView(WalletApp.CAP_LN_FEE_TO_CHAIN, !WalletApp.capLNFeeToChain)
    }
  }

  lazy private[this] val viewCode = new SettingsHolder {
    setVisMany(false -> settingsCheck, false -> settingsInfo)
    view setOnClickListener onButtonTap(viewRecoveryCode)
    settingsTitle.setText(settings_view_revocery_phrase)
    override def updateView: Unit = none
  }

  lazy private[this] val viewStat = new SettingsHolder {
    setVisMany(false -> settingsCheck, false -> settingsInfo)
    view setOnClickListener onButtonTap(me goTo ClassNames.statActivityClass)
    settingsTitle.setText(settings_stats)
    override def updateView: Unit = none
  }

  def INIT(state: Bundle): Unit = {
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_settings)

      val settingsPageitle = new TitleView(s"v$VERSION_NAME-$VERSION_CODE")
      settingsPageitle.view.setOnClickListener(me onButtonTap finish)
      settingsPageitle.backArrow.setVisibility(View.VISIBLE)

      val links = new TitleView("&#9996;")
      addFlowChip(links.flow, getString(twitter), R.drawable.border_blue, _ => me browse "https://twitter.com/SimpleBtcWallet")
      addFlowChip(links.flow, getString(manual), R.drawable.border_green, _ => me browse "https://lightning-wallet.com/posts/manual")
      addFlowChip(links.flow, getString(sources), R.drawable.border_green, _ => me browse "https://github.com/btcontract/wallet")
      addFlowChip(links.flow, getString(rate), R.drawable.border_green, _ => me bringRateDialog null)

      settingsContainer.addView(settingsPageitle.view)
      settingsContainer.addView(storeLocalBackup.view)
      settingsContainer.addView(chainWallets.view)
      settingsContainer.addView(electrum.view)
      settingsContainer.addView(setFiat.view)
      settingsContainer.addView(setBtc.view)

      settingsContainer.addView(useFingerprint.view)
      settingsContainer.addView(enforceTor.view)
      settingsContainer.addView(capLnFees.view)
      settingsContainer.addView(viewCode.view)
      settingsContainer.addView(viewStat.view)
      settingsContainer.addView(links.view)
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }
  }

  trait SettingsHolder {
    val view: RelativeLayout = getLayoutInflater.inflate(R.layout.frag_switch, null, false).asInstanceOf[RelativeLayout]
    val settingsCheck: CheckBox = view.findViewById(R.id.settingsCheck).asInstanceOf[CheckBox]
    val settingsTitle: TextView = view.findViewById(R.id.settingsTitle).asInstanceOf[TextView]
    val settingsInfo: TextView = view.findViewById(R.id.settingsInfo).asInstanceOf[TextView]
    def updateView: Unit

    def putBoolAndUpdateView(key: String, value: Boolean): Unit = {
      WalletApp.app.prefs.edit.putBoolean(key, value).commit
      updateView
    }
  }
}
