package immortan.sqlite

import fr.acinq.bitcoin.ByteVector64
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair._
import fr.acinq.eclair.router.Router.PublicChannel
import fr.acinq.eclair.router.{ChannelUpdateExt, Sync}
import fr.acinq.eclair.wire.{ChannelAnnouncement, ChannelUpdate}
import immortan.SyncMaster.ShortChanIdSet
import immortan._
import scodec.bits.ByteVector

import java.lang.{Integer => JInt, Long => JLong}


class SQLiteNetwork(val db: DBInterface, val updateTable: ChannelUpdateTable, val announceTable: ChannelAnnouncementTable, val excludedTable: ExcludedChannelTable) extends NetworkBag {
  def addChannelAnnouncement(ca: ChannelAnnouncement, newSqlPQ: PreparedQuery): Unit = db.change(newSqlPQ, Array.emptyByteArray, ca.shortChannelId: JLong, ca.nodeId1.value.toArray, ca.nodeId2.value.toArray)

  def addExcludedChannel(shortId: Long, untilStamp: Long, newSqlPQ: PreparedQuery): Unit = db.change(newSqlPQ, shortId: JLong, System.currentTimeMillis + untilStamp: JLong)

  def listExcludedChannels: Set[Long] = db.select(excludedTable.selectSql, System.currentTimeMillis.toString).set(_ long excludedTable.shortChannelId)

  def listChannelsWithOneUpdate: ShortChanIdSet = db.select(updateTable.selectHavingOneUpdate).set(_ long updateTable.sid)

  def incrementScore(cu: ChannelUpdateExt): Unit = db.change(updateTable.updScoreSql, cu.update.shortChannelId: JLong)

  def removeChannelUpdate(shortId: Long, killSqlPQ: PreparedQuery): Unit = db.change(killSqlPQ, shortId: JLong)

  def addChannelUpdateByPosition(cu: ChannelUpdate, newSqlPQ: PreparedQuery, updSqlPQ: PreparedQuery): Unit = {
    val feeProportionalMillionths: JLong = cu.feeProportionalMillionths
    val cltvExpiryDelta: JInt = cu.cltvExpiryDelta.underlying
    val htlcMinimumMsat: JLong = cu.htlcMinimumMsat.toLong
    val htlcMaxMsat: JLong = cu.htlcMaximumMsat.get.toLong
    val messageFlags: JInt = cu.messageFlags.toInt
    val channelFlags: JInt = cu.channelFlags.toInt
    val feeBaseMsat: JLong = cu.feeBaseMsat.toLong
    val timestamp: JLong = cu.timestamp

    val crc32: JLong = Sync.getChecksum(cu)
    db.change(newSqlPQ, cu.shortChannelId: JLong, timestamp, messageFlags, channelFlags, cltvExpiryDelta, htlcMinimumMsat, feeBaseMsat, feeProportionalMillionths, htlcMaxMsat, cu.position, 1L: JLong, crc32)
    db.change(updSqlPQ, timestamp, messageFlags, channelFlags, cltvExpiryDelta, htlcMinimumMsat, feeBaseMsat, feeProportionalMillionths, htlcMaxMsat, crc32, cu.shortChannelId: JLong, cu.position)
  }

  def removeChannelUpdate(shortId: Long): Unit = {
    val removeChannelUpdateNewSqlPQ = db.makePreparedQuery(updateTable.killSql)
    removeChannelUpdate(shortId, removeChannelUpdateNewSqlPQ)
    removeChannelUpdateNewSqlPQ.close
  }

  def addChannelUpdateByPosition(cu: ChannelUpdate): Unit = {
    val addChannelUpdateByPositionNewSqlPQ = db.makePreparedQuery(updateTable.newSql)
    val addChannelUpdateByPositionUpdSqlPQ = db.makePreparedQuery(updateTable.updSQL)
    addChannelUpdateByPosition(cu, addChannelUpdateByPositionNewSqlPQ, addChannelUpdateByPositionUpdSqlPQ)
    addChannelUpdateByPositionNewSqlPQ.close
    addChannelUpdateByPositionUpdSqlPQ.close
  }

  def listChannelAnnouncements: Iterable[ChannelAnnouncement] = db.select(announceTable.selectAllSql).iterable { rc =>
    ChannelAnnouncement(nodeSignature1 = ByteVector64.Zeroes, nodeSignature2 = ByteVector64.Zeroes, bitcoinSignature1 = ByteVector64.Zeroes,
      bitcoinSignature2 = ByteVector64.Zeroes, features = Features.empty, chainHash = LNParams.chainHash, shortChannelId = rc long announceTable.shortChannelId,
      nodeId1 = PublicKey(rc byteVec announceTable.nodeId1), nodeId2 = PublicKey(rc byteVec announceTable.nodeId2), bitcoinKey1 = invalidPubKey, bitcoinKey2 = invalidPubKey)
  }

  def listChannelUpdates: Iterable[ChannelUpdateExt] =
    db.select(updateTable.selectAllSql).iterable { rc =>
      val cltvExpiryDelta = CltvExpiryDelta(rc int updateTable.cltvExpiryDelta)
      val htlcMinimumMsat = MilliSatoshi(rc long updateTable.minMsat)
      val htlcMaximumMsat = MilliSatoshi(rc long updateTable.maxMsat)
      val feeBaseMsat = MilliSatoshi(rc long updateTable.base)
      val channelFlags = rc int updateTable.chanFlags
      val messageFlags = rc int updateTable.msgFlags
      val shortChannelId = rc long updateTable.sid

      val update = ChannelUpdate(signature = ByteVector64.Zeroes, chainHash = LNParams.chainHash, shortChannelId,
        timestamp = rc long updateTable.timestamp, messageFlags.toByte, channelFlags.toByte, cltvExpiryDelta,
        htlcMinimumMsat, feeBaseMsat, feeProportionalMillionths = rc long updateTable.proportional,
        htlcMaximumMsat = Some(htlcMaximumMsat), unknownFields = ByteVector.empty)

      ChannelUpdateExt(update, rc long updateTable.crc32, rc long updateTable.score, updateTable.useHeuristics)
    }

  def getRoutingData: Map[Long, PublicChannel] = {
    val shortId2Updates = listChannelUpdates.groupBy(_.update.shortChannelId)

    val tuples = listChannelAnnouncements.flatMap { ann =>
      shortId2Updates.get(ann.shortChannelId) collectFirst {
        case List(u1, u2) if ChannelUpdate.POSITION1NODE == u1.update.position => ann.shortChannelId -> PublicChannel(Some(u1), Some(u2), ann)
        case List(u2, u1) if ChannelUpdate.POSITION2NODE == u2.update.position => ann.shortChannelId -> PublicChannel(Some(u1), Some(u2), ann)
        case List(u1) if ChannelUpdate.POSITION1NODE == u1.update.position => ann.shortChannelId -> PublicChannel(Some(u1), None, ann)
        case List(u2) if ChannelUpdate.POSITION2NODE == u2.update.position => ann.shortChannelId -> PublicChannel(None, Some(u2), ann)
      }
    }

    tuples.toMap
  }

  def removeGhostChannels(ghostIds: ShortChanIdSet, oneSideIds: ShortChanIdSet): Unit = db txWrap {
    val addExcludedChannelNewSqlPQ = db.makePreparedQuery(excludedTable.newSql)
    val removeChannelUpdateNewSqlPQ = db.makePreparedQuery(updateTable.killSql)

    for (shortId <- oneSideIds) addExcludedChannel(shortId, 1000L * 3600 * 24 * 14, addExcludedChannelNewSqlPQ) // Exclude for two weeks, maybe second update will show up later
    for (shortId <- ghostIds ++ oneSideIds) removeChannelUpdate(shortId, removeChannelUpdateNewSqlPQ) // Make sure we only have known channels with both updates

    addExcludedChannelNewSqlPQ.close
    removeChannelUpdateNewSqlPQ.close

    db.change(excludedTable.killPresentInChans) // Remove from excluded if present in channels (minority says it's bad, majority says it's good)
    db.change(announceTable.killNotPresentInChans) // Remove from announces if not present in channels (announce for excluded channel)
    db.change(excludedTable.killOldSql, System.currentTimeMillis: JLong) // Give old excluded channels a second chance
  }

  def processPureData(pure: PureRoutingData): Unit = db txWrap {
    val addChannelAnnouncementNewSqlPQ = db.makePreparedQuery(announceTable.newSql)
    val addChannelUpdateByPositionNewSqlPQ = db.makePreparedQuery(updateTable.newSql)
    val addChannelUpdateByPositionUpdSqlPQ = db.makePreparedQuery(updateTable.updSQL)
    val addExcludedChannelNewSqlPQ = db.makePreparedQuery(excludedTable.newSql)

    for (announce <- pure.announces) addChannelAnnouncement(announce, addChannelAnnouncementNewSqlPQ)
    for (update <- pure.updates) addChannelUpdateByPosition(update, addChannelUpdateByPositionNewSqlPQ, addChannelUpdateByPositionUpdSqlPQ)
    for (core <- pure.excluded) addExcludedChannel(core.shortChannelId, 1000L * 3600 * 24 * 300, addExcludedChannelNewSqlPQ)

    addChannelAnnouncementNewSqlPQ.close
    addChannelUpdateByPositionNewSqlPQ.close
    addChannelUpdateByPositionUpdSqlPQ.close
    addExcludedChannelNewSqlPQ.close
  }

  def processCompleteHostedData(pure: CompleteHostedRoutingData): Unit = db txWrap {
    // Unlike normal channels here we allow one-sided-update channels to be used for now
    // First, clear out everything in hosted channel databases
    db.change(announceTable.killAllSql)
    db.change(updateTable.killAllSql)

    val addChannelAnnouncementNewSqlPQ = db.makePreparedQuery(announceTable.newSql)
    val addChannelUpdateByPositionNewSqlPQ = db.makePreparedQuery(updateTable.newSql)
    val addChannelUpdateByPositionUpdSqlPQ = db.makePreparedQuery(updateTable.updSQL)

    // Then insert new data
    for (announce <- pure.announces) addChannelAnnouncement(announce, addChannelAnnouncementNewSqlPQ)
    for (update <- pure.updates) addChannelUpdateByPosition(update, addChannelUpdateByPositionNewSqlPQ, addChannelUpdateByPositionUpdSqlPQ)

    addChannelAnnouncementNewSqlPQ.close
    addChannelUpdateByPositionNewSqlPQ.close
    addChannelUpdateByPositionUpdSqlPQ.close

    // And finally remove announces without any updates
    db.change(announceTable.killNotPresentInChans)
  }
}
