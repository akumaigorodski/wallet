package immortan

import immortan.PathFinder._
import immortan.crypto.Tools._
import scala.collection.JavaConverters._

import immortan.utils.{Rx, Statistics}
import java.util.concurrent.{Executors, TimeUnit}
import immortan.crypto.{CanBeRepliedTo, StateMachine}
import fr.acinq.eclair.router.{ChannelUpdateExt, Router}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import fr.acinq.eclair.router.Router.{Data, PublicChannel, RouteRequest}
import fr.acinq.eclair.router.Graph.GraphStructure.{DirectedGraph, GraphEdge}
import fr.acinq.eclair.{CltvExpiryDelta, MilliSatoshi, ShortChannelId, nodeFee}
import fr.acinq.eclair.router.RouteCalculation.handleRouteRequest
import com.google.common.cache.CacheBuilder
import fr.acinq.eclair.wire.ChannelUpdate
import rx.lang.scala.Subscription
import scala.collection.mutable
import fr.acinq.bitcoin.Crypto


object PathFinder {
  val NotifyRejected = "path-finder-notify-rejected"
  val NotifyOperational = "path-finder-notify-operational"
  val CMDLoadGraph = "cmd-load-graph"

  val WAITING = 0
  val OPERATIONAL = 1

  case class AvgHopParams(cltvExpiryDelta: CltvExpiryDelta, feeProportionalMillionths: Long, feeBaseMsat: MilliSatoshi, sampleSize: Long) {
    def avgHopFee(amount: MilliSatoshi): MilliSatoshi = nodeFee(feeBaseMsat, feeProportionalMillionths, amount)
  }

  case class FindRoute(sender: CanBeRepliedTo, request: RouteRequest)
}

abstract class PathFinder(val normalBag: NetworkBag, val hostedBag: NetworkBag) extends StateMachine[Data] { me =>
  private val extraEdges = CacheBuilder.newBuilder.expireAfterWrite(1, TimeUnit.DAYS).maximumSize(5000).build[ShortChannelId, GraphEdge]
  val extraEdgesMap: mutable.Map[ShortChannelId, GraphEdge] = extraEdges.asMap.asScala
  var lastAvgHopParams: Option[AvgHopParams] = None
  var listeners: Set[CanBeRepliedTo] = Set.empty
  var debugMode: Boolean = false

  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def process(changeMessage: Any): Unit = scala.concurrent.Future(me doProcess changeMessage)

  private val CMDResync = "cmd-resync"
  private val RESYNC_PERIOD: Long = 1000L * 3600 * 24 * 2
  // We don't load routing data on every startup but when user (or system) actually needs it
  become(Data(channels = Map.empty, hostedChannels = Map.empty, DirectedGraph.empty), WAITING)

  val subscription: Subscription = {
    // Init first resync with persistent delay on startup
    val repeat = Rx.repeat(Rx.ioQueue, Rx.incHour, 49 to Int.MaxValue by 49)
    // Resync every RESYNC_PERIOD days + 1 hour to trigger a full resync, not just PHC resync
    val delay = Rx.initDelay(repeat, getLastTotalResyncStamp, RESYNC_PERIOD, preStartMsec = 100)
    delay.subscribe(_ => me process CMDResync)
  }

  def getLastTotalResyncStamp: Long
  def getLastNormalResyncStamp: Long

  def updateLastTotalResyncStamp(stamp: Long): Unit
  def updateLastNormalResyncStamp(stamp: Long): Unit

  def getPHCExtraNodes: Set[RemoteNodeInfo]
  def getExtraNodes: Set[RemoteNodeInfo]

  def doProcess(change: Any): Unit = (change, state) match {
    // Graph is loaded but it is empty: likely this is first launch or synchronizing
    case (fr: FindRoute, OPERATIONAL) if data.channels.isEmpty => fr.sender process NotifyRejected

    case (fr: FindRoute, OPERATIONAL) =>
      // Search through single pre-selected local channel
      val augmentedGraph = data.graph replaceEdge fr.request.localEdge
      fr.sender process handleRouteRequest(augmentedGraph, fr.request)

    case (fr: FindRoute, WAITING) if debugMode =>
      // Do not proceed, just inform the sender
      fr.sender process NotifyRejected

    case (fr: FindRoute, WAITING) =>
      // We need a loaded routing data to search for path properly
      // load that data while notifying sender if it's absent
      fr.sender process NotifyRejected
      me process CMDLoadGraph

    case (CMDResync, WAITING) =>
      // We need a loaded routing data to sync properly
      // load that data before proceeding if it's absent
      me process CMDLoadGraph
      me process CMDResync

    case (CMDLoadGraph, WAITING) =>
      val normalShortIdToPubChan = normalBag.getRoutingData
      val hostedShortIdToPubChan = hostedBag.getRoutingData
      val searchGraph1 = DirectedGraph.makeGraph(normalShortIdToPubChan ++ hostedShortIdToPubChan).addEdges(extraEdgesMap.values)
      become(Data(normalShortIdToPubChan, hostedShortIdToPubChan, searchGraph1), OPERATIONAL)

      if (data.channels.nonEmpty) {
        lastAvgHopParams = getAvgHopParams.asSome
        listeners.foreach(_ process NotifyOperational)
      }

    case (CMDResync, OPERATIONAL) if System.currentTimeMillis - getLastNormalResyncStamp > RESYNC_PERIOD =>
      // Last normal sync has happened too long ago, start with normal sync, then proceed with PHC sync
      val setupData = SyncMasterShortIdData(LNParams.syncParams.syncNodes, getExtraNodes, Set.empty)

      new SyncMaster(normalBag.listExcludedChannels, data) { self =>
        def onChunkSyncComplete(pure: PureRoutingData): Unit = me process pure
        def onTotalSyncComplete: Unit = me process self
      } process setupData

    case (CMDResync, OPERATIONAL) if System.currentTimeMillis - getLastTotalResyncStamp > RESYNC_PERIOD =>
      // Normal resync has happened recently, but PHC resync is outdated (PHC failed last time due to running out of attempts)
      // in this case we skip normal sync and start directly with PHC sync to save time and increase PHC sync success chances
      attemptPHCSync

    case (pure: CompleteHostedRoutingData, OPERATIONAL) =>
      // First, completely replace PHC data with obtained one
      hostedBag.processCompleteHostedData(pure)

      // Then reconstruct graph with new PHC data
      val hostedShortIdToPubChan = hostedBag.getRoutingData
      val searchGraph = DirectedGraph.makeGraph(data.channels ++ hostedShortIdToPubChan).addEdges(extraEdgesMap.values)
      become(Data(data.channels, hostedShortIdToPubChan, searchGraph), OPERATIONAL)
      updateLastTotalResyncStamp(System.currentTimeMillis)

    case (pure: PureRoutingData, OPERATIONAL) =>
      // Run in this thread to not overload SyncMaster
      normalBag.processPureData(pure)

    case (sync: SyncMaster, OPERATIONAL) =>
      // Get rid of channels that peers know nothing about
      val normalShortIdToPubChan = normalBag.getRoutingData
      val oneSideShortIds = normalBag.listChannelsWithOneUpdate
      val ghostIds = normalShortIdToPubChan.keySet.diff(sync.provenShortIds)
      val normalShortIdToPubChan1 = normalShortIdToPubChan -- ghostIds -- oneSideShortIds
      val searchGraph = DirectedGraph.makeGraph(normalShortIdToPubChan1 ++ data.hostedChannels).addEdges(extraEdgesMap.values)
      become(Data(normalShortIdToPubChan1, data.hostedChannels, searchGraph), OPERATIONAL)
      // Update normal checkpoint, if PHC sync fails this time we'll jump to it next time
      updateLastNormalResyncStamp(System.currentTimeMillis)

      // Perform database cleaning in a different thread since it's slow and we are operational
      Rx.ioQueue.foreach(_ => normalBag.removeGhostChannels(ghostIds, oneSideShortIds), none)

      // Notify ASAP, then start PHC sync
      lastAvgHopParams = getAvgHopParams.asSome
      listeners.foreach(_ process NotifyOperational)
      attemptPHCSync

    // We always accept and store disabled channels:
    // - to reduce subsequent sync traffic if channel remains disabled
    // - to account for the case when channel suddenly becomes enabled but we don't know
    // - if channel stays disabled for a long time it will be pruned by peers and then by us

    case (cu: ChannelUpdate, OPERATIONAL) if data.channels.contains(cu.shortChannelId) =>
      val data1 = resolve(data.channels(cu.shortChannelId), cu, normalBag)
      become(data1, OPERATIONAL)

    case (cu: ChannelUpdate, OPERATIONAL) if data.hostedChannels.contains(cu.shortChannelId) =>
      val data1 = resolve(data.hostedChannels(cu.shortChannelId), cu, hostedBag)
      become(data1, OPERATIONAL)

    case (cu: ChannelUpdate, OPERATIONAL) =>
      // Last chance: if it's not a known public update then maybe it's a private one
      Option(extraEdges getIfPresent cu.shortChannelId).foreach { extEdge =>
        val edge1 = extEdge.copy(updExt = extEdge.updExt withNewUpdate cu)
        val data1 = resolveKnownDesc(storeOpt = None, edge1)
        become(data1, OPERATIONAL)
      }

    case (edge: GraphEdge, WAITING | OPERATIONAL) if !data.channels.contains(edge.desc.shortChannelId) =>
      // We add assisted routes to graph as if they are normal channels, also rememeber them to refill later if graph gets reloaded
      // these edges will be private most of the time, but they also may be public yet not visible to us for some reason
      val data1 = data.copy(graph = data.graph replaceEdge edge)
      extraEdges.put(edge.updExt.update.shortChannelId, edge)
      become(data1, state)

    case _ =>
  }

  def resolve(pubChan: PublicChannel, upd1: ChannelUpdate, store: NetworkBag): Data = {
    // Resoving normal/hosted public channel updates we get while trying to route payments
    val desc = Router.getDesc(upd1, pubChan.ann)

    pubChan.getChannelUpdateSameSideAs(upd1) match {
      case Some(oldExt) if oldExt.update.timestamp < upd1.timestamp =>
        // We have an old updateExt and obtained one is newer, this is fine
        val edge = GraphEdge(desc, oldExt withNewUpdate upd1)
        resolveKnownDesc(storeOpt = Some(store), edge)

      case None =>
        // Somehow we don't have an old updateExt, create a new one
        val edge = GraphEdge(desc, ChannelUpdateExt fromUpdate upd1)
        resolveKnownDesc(storeOpt = Some(store), edge)

      case _ =>
        // Our updateExt is newer
        data
    }
  }

  def resolveKnownDesc(storeOpt: Option[NetworkBag], edge: GraphEdge): Data = storeOpt match {
    // Resolves channel updates which we extract from remote node errors while trying to route payments
    // store is optional to make sure private normal/hosted channel updates never make it to our database

    case Some(store) if edge.updExt.update.htlcMaximumMsat.isEmpty =>
      // Will be queried on next sync and will most likely be excluded
      store.removeChannelUpdate(edge.updExt.update.shortChannelId)
      data.copy(graph = data.graph removeEdge edge.desc)

    case Some(store) =>
      // This is a legitimate public update, refresh everywhere
      store.addChannelUpdateByPosition(edge.updExt.update)
      data.copy(graph = data.graph replaceEdge edge)

    case None =>
      // This is a legitimate private/unknown-public update
      extraEdges.put(edge.updExt.update.shortChannelId, edge)
      // Don't save this in DB but update runtime graph
      data.copy(graph = data.graph replaceEdge edge)
  }

  def nodeIdFromUpdate(cu: ChannelUpdate): Option[Crypto.PublicKey] =
    data.channels.get(cu.shortChannelId).map(_.ann getNodeIdSameSideAs cu) orElse
      data.hostedChannels.get(cu.shortChannelId).map(_.ann getNodeIdSameSideAs cu) orElse
      extraEdgesMap.get(cu.shortChannelId).map(_.desc.from)

  def attemptPHCSync: Unit = {
    if (LNParams.syncParams.phcSyncNodes.nonEmpty) {
      val master = new PHCSyncMaster(data) { def onSyncComplete(pure: CompleteHostedRoutingData): Unit = me process pure }
      master process SyncMasterPHCData(LNParams.syncParams.phcSyncNodes, getPHCExtraNodes, Set.empty)
    } else updateLastTotalResyncStamp(System.currentTimeMillis)
  }

  def getAvgHopParams: AvgHopParams = {
    val sample = data.channels.values.toVector.flatMap(pc => pc.update1Opt ++ pc.update2Opt)
    val noFeeOutliers = Statistics.removeExtremeOutliers(sample)(_.update.feeProportionalMillionths)
    val cltvExpiryDelta = CltvExpiryDelta(Statistics.meanBy(noFeeOutliers)(_.update.cltvExpiryDelta.toInt).toInt)
    val proportional = Statistics.meanBy(noFeeOutliers)(_.update.feeProportionalMillionths).toLong
    val base = MilliSatoshi(Statistics.meanBy(noFeeOutliers)(_.update.feeBaseMsat).toLong)
    AvgHopParams(cltvExpiryDelta, proportional, base, noFeeOutliers.size)
  }
}
