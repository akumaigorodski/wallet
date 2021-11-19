package fr.acinq.eclair.router

import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.{ByteVector32, ByteVector64}
import fr.acinq.eclair._
import fr.acinq.eclair.payment.PaymentRequest.ExtraHops
import fr.acinq.eclair.router.Graph.GraphStructure.{DirectedGraph, GraphEdge}
import fr.acinq.eclair.router.Graph.RichWeight
import fr.acinq.eclair.router.Router._
import fr.acinq.eclair.wire.{ChannelUpdate, ExtraHop}
import immortan.LNParams
import immortan.crypto.Tools.Any2Some

import scala.concurrent.duration._


object RouteCalculation {
  def handleRouteRequest(graph: DirectedGraph, r: RouteRequest): RouteResponse =
    findRouteInternal(graph, r.source, r.target, r.amount, r.ignoreChannels, r.ignoreNodes, r.routeParams) match {
      case Some(searchResult) => RouteFound(Route(searchResult.path.map(ChannelHop), searchResult.weight), r.fullTag, r.partId)
      case _ => NoRouteAvailable(r.fullTag, r.partId)
    }

  def makeExtraEdges(assistedRoutes: List[ExtraHops], target: PublicKey): Set[GraphEdge] = {
    val converter = routeToEdges(_: ExtraHops, target)
    assistedRoutes.flatMap(converter).toSet
  }

  def routeToEdges(extraHops: ExtraHops, targetNodeId: PublicKey): Graph.GraphStructure.GraphEdges = {
    // BOLT 11: "For each entry, the pubkey is the node ID of the start of the channel", and the last node is the destination
    val protoDescs = (extraHops.map(_.shortChannelId), extraHops.map(_.nodeId), extraHops.map(_.nodeId).drop(1) :+ targetNodeId)
    protoDescs.zipped.toList.map(ChannelDesc.tupled).zip(extraHops map toFakeUpdate).map(GraphEdge.tupled)
  }

  def toFakeUpdate(extraHop: ExtraHop): ChannelUpdateExt = {
    // Lets assume this fake channel's capacity is 1000 BTC, it will be corrected by failed-at-amount mechanism
    val update = ChannelUpdate(signature = ByteVector64.Zeroes, chainHash = ByteVector32.Zeroes, extraHop.shortChannelId,
      System.currentTimeMillis.milliseconds.toSeconds, messageFlags = 1, channelFlags = 0, extraHop.cltvExpiryDelta,
      LNParams.minPayment, extraHop.feeBase, extraHop.feeProportionalMillionths, 1000000000000000L.msat.asSome)

    ChannelUpdateExt.fromUpdate(update)
  }

  private def findRouteInternal(graph: DirectedGraph, localNodeId: PublicKey, targetNodeId: PublicKey, amount: MilliSatoshi,
                                ignoredEdges: Set[ChannelDesc] = Set.empty, ignoredVertices: Set[PublicKey] = Set.empty,
                                routeParams: RouteParams): Option[Graph.WeightedPath] = {

    def feeOk(fee: MilliSatoshi): Boolean = fee <= routeParams.feeReserve
    def lengthOk(length: Int): Boolean = length <= routeParams.routeMaxLength
    def cltvOk(cltv: CltvExpiryDelta): Boolean = cltv <= routeParams.routeMaxCltv

    val boundaries: RichWeight => Boolean = weight => feeOk(weight.costs.head - amount) && cltvOk(weight.cltv) && lengthOk(weight.length)
    Graph.bestPath(graph, localNodeId, targetNodeId, amount, ignoredEdges, ignoredVertices, boundaries)
  }
}
