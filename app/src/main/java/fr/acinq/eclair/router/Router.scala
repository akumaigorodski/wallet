package fr.acinq.eclair.router

import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair._
import fr.acinq.eclair.router.Graph.GraphStructure._
import fr.acinq.eclair.router.Graph.RichWeight
import fr.acinq.eclair.wire._
import immortan.crypto.Tools._
import scodec.bits.ByteVector


object ChannelUpdateExt {
  def fromUpdate(update: ChannelUpdate): ChannelUpdateExt = ChannelUpdateExt(update, Sync.getChecksum(update), score = 1L, useHeuristics = false)
}

case class ChannelUpdateExt(update: ChannelUpdate, crc32: Long, score: Long, useHeuristics: Boolean) {
  def withNewUpdate(cu: ChannelUpdate): ChannelUpdateExt = copy(crc32 = Sync.getChecksum(cu), update = cu)
  lazy val capacity: MilliSatoshi = update.htlcMaximumMsat.get
}

object Router {
  case class ChannelDesc(shortChannelId: Long, from: PublicKey, to: PublicKey)

  case class RouterConf(initRouteMaxLength: Int, routeMaxCltv: CltvExpiryDelta = CltvExpiryDelta(2016), maxChannelFailures: Int = 4, maxStrangeNodeFailures: Int = 6, maxRemoteAttempts: Int = 10)

  case class PublicChannel(update1Opt: Option[ChannelUpdateExt], update2Opt: Option[ChannelUpdateExt], ann: ChannelAnnouncement) {
    def getChannelUpdateSameSideAs(cu: ChannelUpdate): Option[ChannelUpdateExt] = if (cu.position == ChannelUpdate.POSITION1NODE) update1Opt else update2Opt
  }

  trait Hop {
    def nodeId: PublicKey
    def nextNodeId: PublicKey
    def fee(amount: MilliSatoshi): MilliSatoshi
    def cltvExpiryDelta: CltvExpiryDelta
  }

  case class ChannelHop(edge: GraphEdge) extends Hop {
    override def toString: String = s"node: ${nodeId.toString}, base: ${edge.updExt.update.feeBaseMsat}, ppm: ${edge.updExt.update.feeProportionalMillionths}, chanId: ${edge.desc.shortChannelId.toString}"
    override def fee(amount: MilliSatoshi): MilliSatoshi = nodeFee(edge.updExt.update.feeBaseMsat, edge.updExt.update.feeProportionalMillionths, amount)
    override val cltvExpiryDelta: CltvExpiryDelta = edge.updExt.update.cltvExpiryDelta
    override val nextNodeId: PublicKey = edge.desc.to
    override val nodeId: PublicKey = edge.desc.from
  }

  case class NodeHop(nodeId: PublicKey, nextNodeId: PublicKey, cltvExpiryDelta: CltvExpiryDelta, fee: MilliSatoshi) extends Hop {
    override def toString: String = s"Trampoline, node: ${nodeId.value.toHex}, fee reserve: $fee"
    override def fee(amount: MilliSatoshi): MilliSatoshi = fee
  }

  case class RouteParams(feeReserve: MilliSatoshi, routeMaxLength: Int, routeMaxCltv: CltvExpiryDelta)

  case class RouteRequest(fullTag: FullPaymentTag, partId: ByteVector, source: PublicKey,
                          target: PublicKey, amount: MilliSatoshi, localEdge: GraphEdge, routeParams: RouteParams,
                          ignoreNodes: Set[PublicKey] = Set.empty, ignoreChannels: Set[ChannelDesc] = Set.empty)

  type RoutedPerHop = (MilliSatoshi, Hop)

  type RoutedPerChannelHop = (MilliSatoshi, ChannelHop)

  case class Route(hops: Seq[Hop], weight: RichWeight) {
    lazy val fee: MilliSatoshi = weight.costs.head - weight.costs.last

    lazy val routedPerHop: Seq[RoutedPerHop] = weight.costs.tail.zip(hops.tail)

    lazy val routedPerChannelHop: Seq[RoutedPerChannelHop] = routedPerHop.collect { case (amt, chanHop: ChannelHop) => amt -> chanHop }

    def getEdgeForNode(nodeId: PublicKey): Option[GraphEdge] = routedPerChannelHop.secondItems.collectFirst { case chanHop if nodeId == chanHop.nodeId => chanHop.edge }

    def asString: String = routedPerHop.secondItems.map(_.toString).mkString(s"${weight.costs.head}\n->\n", "\n->\n", s"\n->\n${weight.costs.last}\n---\nroute fee: $fee")

    require(hops.nonEmpty, "Route cannot be empty")
  }

  sealed trait RouteResponse { def fullTag: FullPaymentTag }

  case class NoRouteAvailable(fullTag: FullPaymentTag, partId: ByteVector) extends RouteResponse

  case class RouteFound(route: Route, fullTag: FullPaymentTag, partId: ByteVector) extends RouteResponse

  case class Data(channels: Map[Long, PublicChannel], hostedChannels: Map[Long, PublicChannel], graph: DirectedGraph)

  def getDesc(cu: ChannelUpdate, ann: ChannelAnnouncement): ChannelDesc = {
    if (Announcements isNode1 cu.channelFlags) ChannelDesc(cu.shortChannelId, ann.nodeId1, ann.nodeId2)
    else ChannelDesc(cu.shortChannelId, ann.nodeId2, ann.nodeId1)
  }
}