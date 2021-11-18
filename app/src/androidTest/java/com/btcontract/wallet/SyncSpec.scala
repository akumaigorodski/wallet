package com.btcontract.wallet

import org.junit.{Ignore, Test}
import immortan.{LNParams, PureRoutingData, SyncMaster, SyncMasterShortIdData, SyncParams}
import fr.acinq.eclair.router.Graph.GraphStructure.DirectedGraph
import androidx.test.ext.junit.runners.AndroidJUnit4
import fr.acinq.eclair.router.Router.Data
import com.google.common.io.Files
import org.junit.runner.RunWith
import immortan.wire.ExtCodecs
import scodec.bits.ByteVector
import fr.acinq.bitcoin.Block
import java.io.File


//@Ignore
@RunWith(classOf[AndroidJUnit4])
class SyncSpec {

  @Test
  def syncAndPackGraph: Unit = {
    run(DBSpec.randomDBName, makeSnapshot = true)
    this synchronized wait(6000000L)
  }

  def run(dbName: String, makeSnapshot: Boolean): Unit = {
    val (normalStore, _, dbInterface) = DBSpec.getNetworkStores(dbName)

    LNParams.chainHash = Block.LivenetGenesisBlock.hash
    LNParams.ourInit = LNParams.createInit

    LNParams.syncParams = new SyncParams {
      override val maxNodesToSyncFrom = 2
      override val acceptThreshold = 1
    }

    val channelMap0 = normalStore.getRoutingData
    val data0 = Data(channelMap0, hostedChannels = Map.empty, graph = DirectedGraph makeGraph channelMap0)
    val setupData = SyncMasterShortIdData(LNParams.syncParams.syncNodes, extInfos = Set.empty, activeSyncs = Set.empty, ranges = Map.empty, LNParams.syncParams.maxNodesToSyncFrom)

    val syncMaster = new SyncMaster(normalStore.listExcludedChannels, data0) {
      def onShortIdsSyncComplete(state: SyncMasterShortIdData): Unit = println("onShortIdsSyncComplete")

      def onChunkSyncComplete(pure: PureRoutingData): Unit = {
        println(s"Chunk complete, announces=${pure.announces.size}, updates=${pure.updates.size}, excluded=${pure.excluded.size}")
        val a = System.currentTimeMillis
        normalStore.processPureData(pure)
        println(s"DB chunk processing took ${System.currentTimeMillis - a} msec")
      }

      def onTotalSyncComplete: Unit = {
        val data1 = normalStore.getRoutingData
        println(s"Total sync complete, we have ${data1.keys.size} channels")

        val a1 = System.currentTimeMillis
        val oneSidedShortIds = normalStore.listChannelsWithOneUpdate
        normalStore.removeGhostChannels(data1.keySet.diff(provenShortIds), oneSidedShortIds)
        println(s"Removing of ghost channels took ${System.currentTimeMillis - a1} msec")

        val a2 = System.currentTimeMillis
        val data2 = normalStore.getRoutingData
        println(s"Post-processing data took ${System.currentTimeMillis - a2} msec")
        println(s"Total sync complete, we have ${data2.keys.size} purified channels")

        val a3 = System.currentTimeMillis
        val graph = DirectedGraph.makeGraph(data2)
        assert(graph.vertices.forall { case (nodeId, incomingEdges) => incomingEdges.forall(_.desc.to == nodeId) })
        println(s"Making graph took ${System.currentTimeMillis - a3} msec")
        assert(data2.nonEmpty)

        dbInterface.base.execSQL("VACUUM")
        normalStore.clearDataTables
        println(s"Cleared data tables")
        val dataBaseFile = new File(WalletApp.app.getDatabasePath(dbName).getPath)
        val plainBytes = ByteVector(Files toByteArray dataBaseFile)
        println(s"Size of graph db is ${plainBytes.size}")

        val compressedPlainBytes = ExtCodecs.compressedByteVecCodec.encode(plainBytes).require.toByteVector
        println(s"Size of compressed graph db is ${compressedPlainBytes.size}")

        val decompressedPlainBytes = ExtCodecs.compressedByteVecCodec.decode(compressedPlainBytes.toBitVector).require.value
        println(s"Size of decompressed graph db is ${decompressedPlainBytes.size}")
        assert(plainBytes == decompressedPlainBytes)
        println("Done")

        if (makeSnapshot) {
          val backupSpec = new LocalBackupSpec
          backupSpec.writeGraphSnapshot(compressedPlainBytes)
        }

        run(dbName, makeSnapshot = false)
      }
    }

    syncMaster process setupData
  }
}
