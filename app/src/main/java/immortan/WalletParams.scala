package immortan

import java.util.concurrent.atomic.AtomicLong

import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.util.Timeout
import com.softwaremill.quicklens._
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPrivateKey
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum._
import fr.acinq.eclair.blockchain.electrum.db._
import immortan.crypto.CanBeShutDown
import immortan.crypto.Tools.StringList
import immortan.utils._
import scodec.bits.ByteVector

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.util.Try


object WalletParams {
  var secret: WalletSecret = _
  var chainHash: ByteVector32 = _
  var chainWallets: WalletExt = _
  var connectionProvider: ConnectionProvider = _
  var fiatRates: FiatRates = _
  var feeRates: FeeRates = _

  val blockCount: AtomicLong = new AtomicLong(0L)

  def isOperational: Boolean =
    null != chainHash && null != secret && null != chainWallets &&
      null != connectionProvider && null != fiatRates && null != feeRates

  implicit val timeout: Timeout = Timeout(30.seconds)
  implicit val system: ActorSystem = ActorSystem("immortan-actor-system")
  implicit val ec: ExecutionContextExecutor = scala.concurrent.ExecutionContext.Implicits.global
  def addressToPubKeyScript(address: String): ByteVector = Script write addressToPublicKeyScript(address, chainHash)
}

case class WalletExt(wallets: List[ElectrumEclairWallet], catcher: ActorRef, sync: ActorRef, pool: ActorRef, params: WalletParameters) extends CanBeShutDown { me =>
  lazy val usableWallets: List[ElectrumEclairWallet] = wallets.filter(wallet => wallet.ewt.secrets.nonEmpty || wallet.info.core.masterFingerprint.nonEmpty)
  lazy val spendableWallets: List[ElectrumEclairWallet] = usableWallets.filter(_.info.lastBalance > 0L.sat)

  def findByPubKey(pub: PublicKey): Option[ElectrumEclairWallet] = wallets.find(_.ewt.xPub.publicKey == pub)

  def makeSigningWalletParts(core: SigningWallet, masterPrivKey: ExtendedPrivateKey, lastBalance: Satoshi, label: String): ElectrumEclairWallet = {
    val ewt: ElectrumWalletType = ElectrumWalletType.makeSigningType(tag = core.walletType, master = masterPrivKey, chainHash = WalletParams.chainHash)
    val walletRef = WalletParams.system.actorOf(Props(classOf[ElectrumWallet], pool, sync, params, ewt), core.walletType + "-signing-wallet")
    val infoNoPersistent = CompleteChainWalletInfo(core, data = ByteVector.empty, lastBalance, label, isCoinControlOn = false)
    ElectrumEclairWallet(walletRef, ewt, infoNoPersistent)
  }

  def makeWatchingWallet84Parts(core: WatchingWallet, lastBalance: Satoshi, label: String): ElectrumEclairWallet = {
    val ewt: ElectrumWallet84 = new ElectrumWallet84(secrets = None, xPub = core.xPub, chainHash = WalletParams.chainHash)
    val walletRef = WalletParams.system.actorOf(Props(classOf[ElectrumWallet], pool, sync, params, ewt), core.walletType + "-watching-wallet")
    val infoNoPersistent = CompleteChainWalletInfo(core, data = ByteVector.empty, lastBalance, label, isCoinControlOn = false)
    ElectrumEclairWallet(walletRef, ewt, infoNoPersistent)
  }

  def withFreshWallet(eclairWallet: ElectrumEclairWallet): WalletExt = {
    params.walletDb.addChainWallet(eclairWallet.info, params.emptyPersistentDataBytes, eclairWallet.ewt.xPub.publicKey)
    eclairWallet.walletRef ! params.emptyPersistentDataBytes
    sync ! ElectrumWallet.ChainFor(eclairWallet.walletRef)
    copy(wallets = eclairWallet :: wallets)
  }

  def withoutWallet(wallet: ElectrumEclairWallet): WalletExt = {
    require(wallet.info.core.isRemovable, "Wallet is not removable")
    params.walletDb.remove(wallet.ewt.xPub.publicKey)
    val wallets1 = wallets diff List(wallet)
    wallet.walletRef ! PoisonPill
    copy(wallets = wallets1)
  }

  def withNewLabel(label: String)(wallet1: ElectrumEclairWallet): WalletExt = {
    def sameXPub(wallet: ElectrumEclairWallet): Boolean = wallet.ewt.xPub == wallet1.ewt.xPub
    params.walletDb.updateLabel(label, pub = wallet1.ewt.xPub.publicKey)
    me.modify(_.wallets.eachWhere(sameXPub).info.label).setTo(label)
  }

  override def becomeShutDown: Unit = {
    val actors = List(catcher, sync, pool)
    val allActors = wallets.map(_.walletRef) ++ actors
    allActors.foreach(_ ! PoisonPill)
  }
}

case class WalletSecret(keys: LightningNodeKeys, mnemonic: StringList, seed: ByteVector)

// Interfaces

trait DataBag {
  def putSecret(secret: WalletSecret)
  def tryGetSecret: Try[WalletSecret]

  def putFiatRatesInfo(data: FiatRatesInfo)
  def tryGetFiatRatesInfo: Try[FiatRatesInfo]

  def putFeeRatesInfo(data: FeeRatesInfo)
  def tryGetFeeRatesInfo: Try[FeeRatesInfo]
}
