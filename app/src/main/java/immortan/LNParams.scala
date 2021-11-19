package immortan

import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.util.Timeout
import com.softwaremill.quicklens._
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin._
import fr.acinq.eclair.Features._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum._
import fr.acinq.eclair.blockchain.electrum.db.{CompleteChainWalletInfo, SigningWallet, WatchingWallet}
import fr.acinq.eclair.channel.{ChannelKeys, LocalParams, PersistentChannelData}
import fr.acinq.eclair.router.ChannelUpdateExt
import fr.acinq.eclair.router.Router.{PublicChannel, RouterConf}
import fr.acinq.eclair.transactions.{DirectedHtlc, RemoteFulfill}
import fr.acinq.eclair.wire._
import immortan.SyncMaster.ShortChanIdSet
import immortan.crypto.CanBeShutDown
import immortan.crypto.Noise.KeyPair
import immortan.crypto.Tools._
import immortan.sqlite._
import immortan.utils._
import scodec.bits.{ByteVector, HexStringSyntax}

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.util.Try


object LNParams {
  val blocksPerDay: Int = 144 // On average we can expect this many blocks per day
  val ncFulfillSafetyBlocks: Int = 36 // Force-close and redeem revealed incoming payment on chain if NC peer stalls state update and this many blocks are left until expiration
  val hcFulfillSafetyBlocks: Int = 72 // Offer to publish revealed incoming payment preimage on chain if HC peer stalls state update and this many blocks are left until expiration
  val cltvRejectThreshold: Int = hcFulfillSafetyBlocks + 36 // Reject incoming payment right away if CLTV expiry is closer than this to current chain tip when HTLC arrives
  val incomingFinalCltvExpiry: CltvExpiryDelta = CltvExpiryDelta(hcFulfillSafetyBlocks + 72) // Ask payer to set final CLTV expiry to current chain tip + this many blocks

  val failedChanRecoveryMsec: Double = 300000D // Failed-at-amount channels are fully recovered and their original capacity can be tried again after this much time

  val maxCltvExpiryDelta: CltvExpiryDelta = CltvExpiryDelta(2016) // A relative expiry of the whole route can not exceed this much blocks
  val maxToLocalDelay: CltvExpiryDelta = CltvExpiryDelta(2016) // We ask peer to delay their payment for this long in case of force-close
  val maxFundingSatoshis: Satoshi = Satoshi(10000000000L) // Proposed channels of capacity more than this are not allowed
  val maxReserveToFundingRatio: Double = 0.02 // %
  val maxNegotiationIterations: Int = 20
  val maxChainConnectionsCount: Int = 3
  val maxAcceptedHtlcs: Int = 483
  val maxInChannelHtlcs: Int = 10
  val maxHoldSecs: Long = 600L

  val maxOffChainFeeRatio: Double = 0.01 // We are OK with paying up to this % of LN fee relative to payment amount
  val maxOffChainFeeAboveRatio: MilliSatoshi = MilliSatoshi(20000L) // For small amounts we always accept fee up to this

  val shouldSendUpdateFeerateDiff = 5.0
  val shouldRejectPaymentFeerateDiff = 20.0
  val shouldForceClosePaymentFeerateDiff = 50.0

  val ourRoutingCltvExpiryDelta: CltvExpiryDelta = CltvExpiryDelta(144 * 2) // We will reserve this many blocks for our incoming routed HTLC
  val minRoutingCltvExpiryDelta: CltvExpiryDelta = CltvExpiryDelta(144 * 3) // Ask relayer to set CLTV expiry delta to at least this many blocks

  val minInvoiceExpiryDelta: CltvExpiryDelta = CltvExpiryDelta(18) // If payee does not provide an explicit relative CLTV this is what we use by default
  val minForceClosableIncomingHtlcAmountToFeeRatio = 4 // When incoming HTLC gets (nearly) expired, how much higher than trim threshold should it be for us to force-close
  val minForceClosableOutgoingHtlcAmountToFeeRatio = 5 // When peer sends a suspiciously low feerate, how much higher than trim threshold should our outgoing HTLC be for us to force-close
  val minPayment: MilliSatoshi = MilliSatoshi(1000L) // We can neither send nor receive LN payments which are below this value
  val minChanDustLimit: Satoshi = Satoshi(354L)
  val minDepthBlocks: Int = 3

  // Variables to be assigned at runtime

  var secret: WalletSecret = _
  var chainHash: ByteVector32 = _
  var chainWallets: WalletExt = _
  var logBag: SQLiteLog = _
  var cm: ChannelMaster = _

  var ourInit: Init = _
  var routerConf: RouterConf = _
  var syncParams: SyncParams = _
  var fiatRates: FiatRates = _
  var feeRates: FeeRates = _

  var trampoline: TrampolineOn = TrampolineOn(minPayment, maximumMsat = 1000000000L.msat, feeProportionalMillionths = 1000L, exponent = 0.0, logExponent = 0.0, minRoutingCltvExpiryDelta)

  // Last known chain tip (zero is unknown)
  val blockCount: AtomicLong = new AtomicLong(0L)

  def isOperational: Boolean =
    null != chainHash && null != secret && null != chainWallets && null != syncParams && null != trampoline &&
      null != fiatRates && null != feeRates && null != cm && null != cm.inProcessors && null != cm.sendTo &&
      null != logBag && null != routerConf && null != ourInit

  implicit val timeout: Timeout = Timeout(30.seconds)
  implicit val system: ActorSystem = ActorSystem("immortan-actor-system")
  implicit val ec: ExecutionContextExecutor = scala.concurrent.ExecutionContext.Implicits.global

  def createInit: Init = {
    val networks: InitTlv = InitTlv.Networks(chainHash :: Nil)
    val tlvStream: TlvStream[InitTlv] = TlvStream(networks)

    Init(Features(
      (ChannelRangeQueries, FeatureSupport.Optional),
      (ChannelRangeQueriesExtended, FeatureSupport.Optional),
      (BasicMultiPartPayment, FeatureSupport.Optional),
      (OptionDataLossProtect, FeatureSupport.Optional),
      (VariableLengthOnion, FeatureSupport.Optional),
      (TrampolineRouting, FeatureSupport.Optional),
      (ShutdownAnySegwit, FeatureSupport.Optional),
      (StaticRemoteKey, FeatureSupport.Optional),
      (HostedChannels, FeatureSupport.Optional),
      (PaymentSecret, FeatureSupport.Optional),
      (ChainSwap, FeatureSupport.Optional),
      (Wumbo, FeatureSupport.Optional)
    ), tlvStream)
  }

  // We make sure force-close pays directly to our local wallet always
  def makeChannelParams(isFunder: Boolean, fundingAmount: Satoshi): LocalParams = {
    val walletPubKey = Await.result(chainWallets.lnWallet.getReceiveAddresses, atMost = 40.seconds).keys.head.publicKey
    makeChannelParams(Script.write(Script.pay2wpkh(walletPubKey).toList), walletPubKey, isFunder, fundingAmount)
  }

  // We make sure that funder and fundee key path end differently
  def makeChannelParams(defaultFinalScriptPubkey: ByteVector, walletStaticPaymentBasepoint: PublicKey, isFunder: Boolean, fundingAmount: Satoshi): LocalParams =
    makeChannelParams(defaultFinalScriptPubkey, walletStaticPaymentBasepoint, isFunder, ChannelKeys.newKeyPath(isFunder), fundingAmount)

  // Note: we set local maxHtlcValueInFlightMsat to channel capacity to simplify calculations
  def makeChannelParams(defFinalScriptPubkey: ByteVector, walletStaticPaymentBasepoint: PublicKey, isFunder: Boolean, keyPath: DeterministicWallet.KeyPath, fundingAmount: Satoshi): LocalParams =
    LocalParams(ChannelKeys.fromPath(secret.keys.master, keyPath), minChanDustLimit, UInt64(fundingAmount.toMilliSatoshi.toLong), channelReserve = (fundingAmount * 0.001).max(minChanDustLimit),
      minPayment, maxToLocalDelay, maxInChannelHtlcs, isFunder, defFinalScriptPubkey, walletStaticPaymentBasepoint)

  def currentBlockDay: Long = blockCount.get / blocksPerDay

  def isPeerSupports(theirInit: Init)(feature: Feature): Boolean = Features.canUseFeature(ourInit.features, theirInit.features, feature)

  def loggedActor(childProps: Props, childName: String): ActorRef = system actorOf Props(classOf[LoggingSupervisor], childProps, childName)

  def addressToPubKeyScript(address: String): ByteVector = Script write addressToPublicKeyScript(address, chainHash)

  def isMainnet: Boolean = chainHash == Block.LivenetGenesisBlock.hash
}

case class WalletExt(wallets: List[ElectrumEclairWallet], catcher: ActorRef, sync: ActorRef, pool: ActorRef, watcher: ActorRef, params: WalletParameters) extends CanBeShutDown { me =>

  lazy val lnWallet: ElectrumEclairWallet = wallets.find(_.isBuiltIn).get

  lazy val usableWallets: List[ElectrumEclairWallet] = wallets.filter(wallet => wallet.isBuiltIn || wallet.hasFingerprint)

  def findByPubKey(pub: PublicKey): Option[ElectrumEclairWallet] = wallets.find(_.ewt.xPub.publicKey == pub)

  def makeSigningWalletParts(core: SigningWallet, lastBalance: Satoshi, label: String): ElectrumEclairWallet = {
    val ewt = ElectrumWalletType.makeSigningType(tag = core.walletType, master = LNParams.secret.keys.master, chainHash = LNParams.chainHash)
    val walletRef = LNParams.loggedActor(Props(classOf[ElectrumWallet], pool, sync, params, ewt), core.walletType + "-signing-wallet")
    val infoNoPersistent = CompleteChainWalletInfo(core, data = ByteVector.empty, lastBalance, label, isCoinControlOn = false)
    ElectrumEclairWallet(walletRef, ewt, infoNoPersistent)
  }

  def makeWatchingWallet84Parts(core: WatchingWallet, lastBalance: Satoshi, label: String): ElectrumEclairWallet = {
    val ewt: ElectrumWallet84 = new ElectrumWallet84(secrets = None, xPub = core.xPub, chainHash = LNParams.chainHash)
    val walletRef = LNParams.loggedActor(Props(classOf[ElectrumWallet], pool, sync, params, ewt), core.walletType + "-watching-wallet")
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
    params.walletDb.remove(pub = wallet.ewt.xPub.publicKey)
    params.txDb.removeByPub(xPub = wallet.ewt.xPub)
    val wallets1 = wallets diff List(wallet)
    wallet.walletRef ! PoisonPill
    copy(wallets = wallets1)
  }

  def withNewLabel(label: String)(wallet1: ElectrumEclairWallet): WalletExt = {
    require(!wallet1.isBuiltIn, "Can not re-label a default built in chain wallet")
    def sameXPub(wallet: ElectrumEclairWallet): Boolean = wallet.ewt.xPub == wallet1.ewt.xPub
    params.walletDb.updateLabel(label, pub = wallet1.ewt.xPub.publicKey)
    me.modify(_.wallets.eachWhere(sameXPub).info.label).setTo(label)
  }

  override def becomeShutDown: Unit = {
    val actors = List(catcher, sync, pool, watcher)
    val allActors = wallets.map(_.walletRef) ++ actors
    allActors.foreach(_ ! PoisonPill)
  }
}

class SyncParams {
  val satm: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"02cd1b7bc418fac2dc99f0ba350d60fa6c45fde5ab6017ee14df6425df485fb1dd"), NodeAddress.unresolved(80, host = 134, 209, 228, 207), "SATM")
  val motherbase: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"021e7ea08e31a576b4fd242761d701452a8ac98113eac3074c153db85d2dcc7d27"), NodeAddress.unresolved(9001, host = 5, 9, 83, 143), "Motherbase")
  val bCashIsTrash: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"0298f6074a454a1f5345cb2a7c6f9fce206cd0bf675d177cdbf0ca7508dd28852f"), NodeAddress.unresolved(9735, host = 73, 119, 255, 56), "bCashIsTrash")
  val ergveinNet: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"034a7b1ac1239ff2ac8438ce0a7ade1048514b77d4322f514e96918e6c13944861"), NodeAddress.unresolved(9735, host = 188, 244, 4, 78), "ergvein.net")
  val conductor: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"03c436af41160a355fc1ed230a64f6a64bcbd2ae50f12171d1318f9782602be601"), NodeAddress.unresolved(9735, host = 18, 191, 89, 219), "Conductor")
  val lntxbot1: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"03ee58475055820fbfa52e356a8920f62f8316129c39369dbdde3e5d0198a9e315"), NodeAddress.unresolved(9734, host = 198, 251, 89, 159), "LNTXBOT-E")
  val silentBob: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"02e9046555a9665145b0dbd7f135744598418df7d61d3660659641886ef1274844"), NodeAddress.unresolved(9735, host = 31, 16, 52, 37), "SilentBob")
  val lightning: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"03baa70886d9200af0ffbd3f9e18d96008331c858456b16e3a9b41e735c6208fef"), NodeAddress.unresolved(9735, host = 45, 20, 67, 1), "LIGHTNING")
  val acinq: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"03864ef025fde8fb587d989186ce6a4a186895ee44a926bfc370e2c366597a3f8f"), NodeAddress.unresolved(9735, host = 34, 239, 230, 56), "ACINQ")
  val syncNodes: Set[RemoteNodeInfo] = Set(satm, motherbase, bCashIsTrash, ergveinNet, conductor, silentBob, lightning, acinq)
  val phcSyncNodes: Set[RemoteNodeInfo] = Set(satm, motherbase, lntxbot1)

  val maxPHCCapacity: MilliSatoshi = MilliSatoshi(100000000000000L) // PHC can not be larger than 1000 BTC
  val minPHCCapacity: MilliSatoshi = MilliSatoshi(1000000000L) // PHC can not be smaller than 0.01 BTC
  val minNormalChansForPHC = 5 // How many normal chans a node must have to be eligible for PHCs
  val maxPHCPerNode = 3 // How many PHCs a node can have in total

  val minCapacity: MilliSatoshi = MilliSatoshi(900000000L) // 900k sat
  val maxNodesToSyncFrom = 2 // How many disjoint peers to use for majority sync
  val acceptThreshold = 1 // ShortIds and updates are accepted if confirmed by more than this peers
  val messagesToAsk = 400 // Ask for this many messages from peer before they say this chunk is done
  val chunksToWait = 4 // Wait for at least this much chunk iterations from any peer before recording results
}

class TestNetSyncParams extends SyncParams {
  val sbw: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"03b8534f2d84de39a68d1359f6833fde819b731e188ddf633a666f7bf8c1d7650a"), NodeAddress.unresolved(9735, host = 45, 61, 187, 156), "SBW")
  val endurance: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"03933884aaf1d6b108397e5efe5c86bcf2d8ca8d2f700eda99db9214fc2712b134"), NodeAddress.unresolved(9735, host = 76, 223, 71, 211), "Endurance")
  val localhost: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"038d5cdea665f68e597da00ae0612238bd30a06bdf08d34fa9af783b1f1b3ba9b7"), NodeAddress.unresolved(9735, host = 10, 0, 2, 2), "localhost")
  override val syncNodes: Set[RemoteNodeInfo] = Set(endurance, localhost, sbw)
  override val phcSyncNodes: Set[RemoteNodeInfo] = Set(localhost, sbw)
  override val minCapacity: MilliSatoshi = MilliSatoshi(100000000L)
  override val minNormalChansForPHC = 1
  override val maxNodesToSyncFrom = 1
  override val acceptThreshold = 0
}

// Important: LNParams.secret must be defined
case class RemoteNodeInfo(nodeId: PublicKey, address: NodeAddress, alias: String) {
  lazy val nodeSpecificExtendedKey: DeterministicWallet.ExtendedPrivateKey = LNParams.secret.keys.ourFakeNodeIdKey(nodeId)
  lazy val nodeSpecificPair: KeyPairAndPubKey = KeyPairAndPubKey(KeyPair(nodeSpecificPubKey.value, nodeSpecificPrivKey.value), nodeId)
  lazy val nodeSpecificPrivKey: PrivateKey = nodeSpecificExtendedKey.privateKey
  lazy val nodeSpecificPubKey: PublicKey = nodeSpecificPrivKey.publicKey
  def safeAlias: RemoteNodeInfo = copy(alias = new String)
}

case class WalletSecret(keys: LightningNodeKeys, mnemonic: List[String], seed: ByteVector)
case class UpdateAddHtlcExt(theirAdd: UpdateAddHtlc, remoteInfo: RemoteNodeInfo)
case class SwapInStateExt(state: SwapInState, nodeId: PublicKey)

// Interfaces

trait NetworkBag {
  def addChannelAnnouncement(ca: ChannelAnnouncement, newSqlPQ: PreparedQuery)
  def addChannelUpdateByPosition(cu: ChannelUpdate, newSqlPQ: PreparedQuery, updSqlPQ: PreparedQuery)
  // When adding an excluded channel we disregard an update position: channel as a whole is always excluded
  def addExcludedChannel(shortId: Long, untilStamp: Long, newSqlPQ: PreparedQuery)
  def removeChannelUpdate(shortId: Long, killSqlPQ: PreparedQuery)

  def addChannelUpdateByPosition(cu: ChannelUpdate)
  def removeChannelUpdate(shortId: Long)

  def listChannelAnnouncements: Iterable[ChannelAnnouncement]
  def listChannelUpdates: Iterable[ChannelUpdateExt]
  def listChannelsWithOneUpdate: ShortChanIdSet
  def listExcludedChannels: Set[Long]

  def incrementScore(cu: ChannelUpdateExt)
  def getRoutingData: Map[Long, PublicChannel]
  def removeGhostChannels(ghostIds: ShortChanIdSet, oneSideIds: ShortChanIdSet)
  def processCompleteHostedData(pure: CompleteHostedRoutingData)
  def processPureData(data: PureRoutingData)
}

// Bag of stored payments and successful relays

trait PaymentBag {
  def getPreimage(hash: ByteVector32): Try[ByteVector32]
  def setPreimage(paymentHash: ByteVector32, preimage: ByteVector32)
  def addRelayedPreimageInfo(fullTag: FullPaymentTag, preimage: ByteVector32, relayed: MilliSatoshi, earned: MilliSatoshi)

  def addSearchablePayment(search: String, paymentHash: ByteVector32)
  def searchPayments(rawSearchQuery: String): RichCursor

  def replaceOutgoingPayment(prex: PaymentRequestExt, description: PaymentDescription, action: Option[PaymentAction],
                             finalAmount: MilliSatoshi, balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc,
                             chainFee: MilliSatoshi, seenAt: Long)

  def replaceIncomingPayment(prex: PaymentRequestExt, preimage: ByteVector32, description: PaymentDescription,
                             balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc)

  def getPaymentInfo(paymentHash: ByteVector32): Try[PaymentInfo]
  def removePaymentInfo(paymentHash: ByteVector32)

  def updDescription(description: PaymentDescription, paymentHash: ByteVector32)
  def updOkIncoming(receivedAmount: MilliSatoshi, paymentHash: ByteVector32)
  def updOkOutgoing(fulfill: RemoteFulfill, fee: MilliSatoshi)
  def updAbortedOutgoing(paymentHash: ByteVector32)

  def listRecentRelays(limit: Int): RichCursor
  def listRecentPayments(limit: Int): RichCursor
  def listPendingSecrets: Iterable[ByteVector32]

  def paymentSummary: Try[PaymentSummary]
  def relaySummary: Try[RelaySummary]

  def toRelayedPreimageInfo(rc: RichCursor): RelayedPreimageInfo
  def toPaymentInfo(rc: RichCursor): PaymentInfo
}

trait DataBag {
  def putSecret(secret: WalletSecret)
  def tryGetSecret: Try[WalletSecret]

  def putFiatRatesInfo(data: FiatRatesInfo)
  def tryGetFiatRatesInfo: Try[FiatRatesInfo]

  def putFeeRatesInfo(data: FeeRatesInfo)
  def tryGetFeeRatesInfo: Try[FeeRatesInfo]

  def putReport(paymentHash: ByteVector32, report: String)
  def tryGetReport(paymentHash: ByteVector32): Try[String]

  def putBranding(nodeId: PublicKey, branding: HostedChannelBranding)
  def tryGetBranding(nodeId: PublicKey): Try[HostedChannelBranding]

  def putSwapInState(nodeId: PublicKey, state: SwapInState)
  def tryGetSwapInState(nodeId: PublicKey): Try[SwapInStateExt]
}

object ChannelBag {
  case class Hash160AndCltv(hash160: ByteVector, cltvExpiry: CltvExpiry)
}

trait ChannelBag {
  val db: DBInterface
  def all: Iterable[PersistentChannelData]
  def put(data: PersistentChannelData): PersistentChannelData
  def delete(channelId: ByteVector32)

  def htlcInfos(commitNumer: Long): Iterable[ChannelBag.Hash160AndCltv]
  def putHtlcInfo(sid: Long, commitNumber: Long, paymentHash: ByteVector32, cltvExpiry: CltvExpiry)
  def putHtlcInfos(htlcs: Seq[DirectedHtlc], sid: Long, commitNumber: Long)
  def rmHtlcInfos(sid: Long)

  def channelTxFeesSummary: Try[ChannelTxFeesSummary]
  def addChannelTxFee(feePaid: Satoshi, idenitifer: String, tag: String)
}
