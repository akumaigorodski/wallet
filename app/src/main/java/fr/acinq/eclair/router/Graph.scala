/*
 * Copyright 2019 ACINQ SAS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.acinq.eclair.router

import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair._
import fr.acinq.eclair.router.Graph.GraphStructure.{DirectedGraph, GraphEdge}
import fr.acinq.eclair.router.Router._

import scala.collection.JavaConversions._
import scala.collection.mutable


object Graph {
  /**
   * The cumulative weight of a set of edges (path in the graph).
   *
   * @param costs   amount to send to the recipient + each edge's fees per hop
   * @param length number of edges in the path
   * @param cltv   sum of each edge's cltv
   */
  case class RichWeight(costs: List[MilliSatoshi], length: Int, cltv: CltvExpiryDelta, weight: Double) extends Ordered[RichWeight] {
    override def compare(that: RichWeight): Int = weight.compareTo(that.weight)
  }

  case class WeightedNode(key: PublicKey, weight: RichWeight)

  case class WeightedPath(path: Seq[GraphEdge], weight: RichWeight)

  /**
   * This comparator must be consistent with the "equals" behavior, thus for two weighted nodes with the same weight we distinguish them by their public key
   * See https://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html
   */
  object NodeComparator extends Ordering[WeightedNode] {
    override def compare(x: WeightedNode, y: WeightedNode): Int = {
      val weightCmp = x.weight.compareTo(y.weight)
      if (weightCmp == 0) x.key.toString.compareTo(y.key.toString)
      else weightCmp
    }
  }

  implicit object PathComparator extends Ordering[WeightedPath] {
    override def compare(x: WeightedPath, y: WeightedPath): Int = y.weight.compare(x.weight)
  }

  def bestPath(graph: DirectedGraph,
               sourceNode: PublicKey, targetNode: PublicKey, amount: MilliSatoshi,
               ignoredEdges: Set[ChannelDesc], ignoredVertices: Set[PublicKey],
               boundaries: RichWeight => Boolean): Option[WeightedPath] = {

    val latestBlockExpectedStampMsecs = System.currentTimeMillis
    val targetWeight = RichWeight(List(amount), length = 0, CltvExpiryDelta(0), weight = 0)
    val shortestPath = dijkstraShortestPath(graph, sourceNode, targetNode, ignoredEdges, ignoredVertices, targetWeight, boundaries, latestBlockExpectedStampMsecs)

    if (shortestPath.nonEmpty) {
      val weight = shortestPath.foldRight(targetWeight) { case (edge, prev) =>
        RoutingHeuristics.addEdgeWeight(sourceNode, edge, prev, latestBlockExpectedStampMsecs)
      }

      val weightedPath = WeightedPath(shortestPath, weight)
      Some(weightedPath)
    } else None
  }

  /**
   * Finds the shortest path in the graph, uses a modified version of Dijkstra's algorithm that computes the shortest
   * path from the target to the source (this is because we want to calculate the weight of the edges correctly). The
   * graph @param g is optimized for querying the incoming edges given a vertex.
   *
   * @param g                             the graph on which will be performed the search
   * @param sourceNode                    the starting node of the path we're looking for
   * @param targetNode                    the destination node of the path
   * @param ignoredEdges                  channels that should be avoided
   * @param ignoredVertices               nodes that should be avoided
   * @param initialWeight                 weight that will be applied to the target node
   * @param boundaries                    a predicate function that can be used to impose limits on the outcome of the search
   */
  private def dijkstraShortestPath(g: DirectedGraph, sourceNode: PublicKey, targetNode: PublicKey,
                                   ignoredEdges: Set[ChannelDesc], ignoredVertices: Set[PublicKey], initialWeight: RichWeight,
                                   boundaries: RichWeight => Boolean, latestBlockExpectedStampMsecs: Long): Seq[GraphEdge] = {

    val sourceNotInGraph = !g.containsVertex(sourceNode)
    val targetNotInGraph = !g.containsVertex(targetNode)
    if (sourceNotInGraph || targetNotInGraph) return Seq.empty
    // Conservative estimation to avoid over-allocating memory
    val initialCapacity = 100

    val bestWeights = new DefaultHashMap[PublicKey, RichWeight](RichWeight(List(Long.MaxValue.msat), Int.MaxValue, CltvExpiryDelta(Int.MaxValue), Double.MaxValue), initialCapacity)
    val bestEdges = new java.util.HashMap[PublicKey, GraphEdge](initialCapacity)

    // We want the elements with smallest weight first, hence the reverse
    val toExplore = mutable.PriorityQueue.empty[WeightedNode](NodeComparator.reverse)
    var targetFound = false

    // initialize the queue and cost array with the initial weight
    toExplore enqueue WeightedNode(targetNode, initialWeight)
    bestWeights.put(targetNode, initialWeight)

    while (!targetFound && toExplore.nonEmpty) {
      // Node with the smallest distance from the target

      val current = toExplore.dequeue
      targetFound = current.key == sourceNode

      if (!targetFound) {
        // build the neighbors with optional extra edges
        g.getIncomingEdgesOf(current.key).foreach { edge =>
          val currentCost: MilliSatoshi = current.weight.costs.head
          val neighborWeight = RoutingHeuristics.addEdgeWeight(sourceNode, edge, current.weight, latestBlockExpectedStampMsecs)
          val canRelayAmount = currentCost <= edge.updExt.capacity && currentCost >= edge.updExt.update.htlcMinimumMsat
          val neighbor = edge.desc.from

          if (boundaries(neighborWeight) && !ignoredEdges.contains(edge.desc) && !ignoredVertices.contains(neighbor) && canRelayAmount) {
            // if this path between neighbor and the target has a shorter distance than previously known, we select it
            if (neighborWeight.weight < bestWeights.getOrDefaultValue(neighbor).weight) {
              // update the best edge for this vertex
              bestEdges.put(neighbor, edge)
              // add this updated node to the list for further exploration
              toExplore enqueue WeightedNode(neighbor, neighborWeight) // O(1)
              // update the minimum known distance array
              bestWeights.put(neighbor, neighborWeight)
            }
          }
        }
      } else {
        targetFound = true
      }
    }

    if (targetFound) {
      val edgePath = new mutable.ArrayBuffer[GraphEdge](RouteCalculation.ROUTE_MAX_LENGTH)
      var current = bestEdges.get(sourceNode)

      while (null != current) {
        edgePath += current
        current = bestEdges.get(current.desc.to)
      }

      edgePath
    } else {
      Seq.empty
    }
  }

  private def addEdgeFees(edge: GraphEdge, amountToForward: MilliSatoshi): MilliSatoshi = {
    // Calculate the minimum amount that the start node needs to receive to be able to forward @amountWithFees to the end node
    amountToForward + nodeFee(edge.updExt.update.feeBaseMsat, edge.updExt.update.feeProportionalMillionths, amountToForward)
  }

  object RoutingHeuristics {
    val BLOCK_300K: Int = 300000

    val BLOCK_300K_STAMP_MSEC: Long = 1399680000000L

    val AVG_BLOCK_INTERVAL_MSEC: Long = 10 * 60 * 1000L

    val CAPACITY_CHANNEL_LOW: MilliSatoshi = fr.acinq.bitcoin.MilliBtc(10).toMilliSatoshi

    val CAPACITY_CHANNEL_HIGH: MilliSatoshi = fr.acinq.bitcoin.Btc(10).toMilliSatoshi

    val CLTV_LOW = 9

    val CLTV_HIGH = 2016

    val SCORE_LOW = 1

    val SCORE_HIGH = 1000

    def normalize(value: Double, min: Double, max: Double): Double =
      if (value <= min) 0D else if (value >= max) 1D else (value - min) / (max - min)

    def addEdgeWeight(sender: PublicKey, edge: GraphEdge, prev: RichWeight, latestBlockExpectedStampMsecs: Long): RichWeight = {
      // Every edge is weighted by its routing success score, higher score adds less weight
      val successFactor = 1 - normalize(edge.updExt.score, SCORE_LOW, SCORE_HIGH)

      val factor = if (edge.updExt.useHeuristics) {
        val blockNum = ShortChannelId.blockHeight(edge.desc.shortChannelId)
        val ageFactor = normalize(BLOCK_300K_STAMP_MSEC + (blockNum - BLOCK_300K) * AVG_BLOCK_INTERVAL_MSEC, BLOCK_300K_STAMP_MSEC, latestBlockExpectedStampMsecs)
        val capFactor = 1 - normalize(edge.updExt.capacity.toLong, CAPACITY_CHANNEL_LOW.toLong, CAPACITY_CHANNEL_HIGH.toLong)
        val cltvFactor = normalize(edge.updExt.update.cltvExpiryDelta.toInt, CLTV_LOW, CLTV_HIGH)
        ageFactor + capFactor + cltvFactor + successFactor
      } else {
        // Minimize all heuristics except success rate on assisted and hosted channels
        // this makes these channels more likely to be used for routing at first
        successFactor
      }

      val totalCost = if (edge.desc.from == sender) prev.costs else addEdgeFees(edge, prev.costs.head) +: prev.costs
      val totalCltv = if (edge.desc.from == sender) prev.cltv else prev.cltv + edge.updExt.update.cltvExpiryDelta

      // Every heuristic adds 0 - 100 imgainary SAT to edge weight (which is based on fee cost in msat), the smaller heuristic is the less SAT it adds
      val totalWeight = if (edge.desc.from == sender) prev.weight else prev.weight + totalCost.head.toLong + factor * 100000L
      RichWeight(totalCost, prev.length + 1, totalCltv, totalWeight)
    }
  }

  object GraphStructure {
    type GraphEdges = List[GraphEdge]

    case class DescAndCapacity(desc: ChannelDesc, capacity: MilliSatoshi)

    case class GraphEdge(desc: ChannelDesc, updExt: ChannelUpdateExt) {
      def toDescAndCapacity: DescAndCapacity = DescAndCapacity(desc, updExt.capacity)
    }

    // Graph data structure that uses an adjacency list, stores the incoming edges of the neighbors
    case class DirectedGraph(vertices: Map[PublicKey, GraphEdges] = Map.empty) {

      def replaceEdge(edge: GraphEdge): DirectedGraph = {
        val isAlreadyPresent: Boolean = graphContainsEdge(edge.desc)

        if (isAlreadyPresent) {
          removeEdge(edge.desc).replaceEdge(edge)
        } else {
          val withVertices = addVertex(edge.desc.from).addVertex(edge.desc.to)
          val vertices1: GraphEdges = edge :: withVertices.vertices(edge.desc.to)
          val vertices2 = withVertices.vertices.updated(edge.desc.to, vertices1)
          DirectedGraph(vertices2)
        }
      }

      def removeEdge(desc: ChannelDesc): DirectedGraph = {
        val isAlreadyPresent: Boolean = graphContainsEdge(desc)

        if (isAlreadyPresent) {
          val vertices1 = vertices(desc.to).filterNot(_.desc == desc)
          val vertices2 = vertices.updated(desc.to, vertices1)
          DirectedGraph(vertices2)
        } else {
          this
        }
      }

      def removeEdges(descList: Iterable[ChannelDesc]): DirectedGraph = descList.foldLeft(this)(_ removeEdge _)

      def addEdges(edges: Iterable[GraphEdge]): DirectedGraph = edges.foldLeft(this)(_ replaceEdge _)

      def getIncomingEdgesOf(keyB: PublicKey): GraphEdges = vertices.getOrElse(keyB, Nil)

      def addVertex(key: PublicKey): DirectedGraph =
        if (vertices contains key) this else {
          val vertices1 = vertices.updated(key, Nil)
          DirectedGraph(vertices1)
        }

      def containsVertex(key: PublicKey): Boolean = vertices.contains(key)

      def graphContainsEdge(desc: ChannelDesc): Boolean = vertices.get(desc.to) match {
        case Some(adj) => adj.exists(neighbor => neighbor.desc.shortChannelId == desc.shortChannelId && neighbor.desc.from == desc.from)
        case None => false
      }
    }

    object DirectedGraph {
      def empty: DirectedGraph = new DirectedGraph(Map.empty)

      def apply(edges: GraphEdges): DirectedGraph = empty.addEdges(edges)

      def makeGraph(channels: Map[ShortChannelId, PublicChannel] = Map.empty): DirectedGraph = {
        // Initialize the map with the appropriate size to avoid resizing during the graph initialization
        val mutableMap = new DefaultHashMap[PublicKey, GraphEdges](Nil, channels.size + 1)

        channels.values.foreach { channel =>
          channel.update1Opt.foreach { update1 =>
            val desc1 = Router.getDesc(update1.update, channel.ann)
            val edges1 = mutableMap.getOrDefaultValue(desc1.to)
            val edges2 = GraphEdge(desc1, update1) :: edges1
            mutableMap.put(desc1.to, edges2)
          }

          channel.update2Opt.foreach { update2 =>
            val desc2 = Router.getDesc(update2.update, channel.ann)
            val edges1 = mutableMap.getOrDefaultValue(desc2.to)
            val edges2 =GraphEdge(desc2, update2) :: edges1
            mutableMap.put(desc2.to, edges2)
          }
        }

        DirectedGraph(mutableMap.toMap)
      }
    }
  }
}
