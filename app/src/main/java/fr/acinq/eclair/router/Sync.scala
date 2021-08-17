/*
 * Copyright 2020 ACINQ SAS
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

import fr.acinq.eclair.router.Router._
import fr.acinq.eclair.wire._
import fr.acinq.eclair._
import fr.acinq.eclair.ShortChannelId
import com.google.common.hash.Hashing
import scodec.bits.ByteVector
import shapeless.HNil

object Sync {
  def shouldRequestUpdate(ourTimestamp: Long, ourChecksum: Long, theirTimestamp: Long, theirChecksum: Long): Boolean = {
    // we request their channel_update if all those conditions are met:
    // - it is more recent than ours
    // - it is different from ours, or it is the same but ours is about to be stale
    // - it is not stale
    val theirsIsMoreRecent = ourTimestamp < theirTimestamp
    val areDifferent = ourChecksum != theirChecksum
    val oursIsAlmostStale = StaleChannels.isAlmostStale(ourTimestamp)
    val theirsIsStale = StaleChannels.isStale(theirTimestamp)
    theirsIsMoreRecent && (areDifferent || oursIsAlmostStale) && !theirsIsStale
  }

  def getChannelDigestInfo(channels: Map[ShortChannelId, PublicChannel])(shortChannelId: ShortChannelId): (ReplyChannelRangeTlv.Timestamps, ReplyChannelRangeTlv.Checksums) = {
    val c = channels(shortChannelId)
    val timestamp1 = c.update1Opt.map(_.update.timestamp).getOrElse(0L)
    val timestamp2 = c.update2Opt.map(_.update.timestamp).getOrElse(0L)
    val checksum1 = c.update1Opt.map(_.crc32).getOrElse(0L)
    val checksum2 = c.update2Opt.map(_.crc32).getOrElse(0L)
    (ReplyChannelRangeTlv.Timestamps(timestamp1 = timestamp1, timestamp2 = timestamp2), ReplyChannelRangeTlv.Checksums(checksum1 = checksum1, checksum2 = checksum2))
  }

  def crc32c(data: ByteVector): Long = {
    Hashing.crc32c.hashBytes(data.toArray).asInt() & 0xFFFFFFFFL
  }

  def getChecksum(u: ChannelUpdate): Long = {
    val data = serializationResult(LightningMessageCodecs.channelUpdateChecksumCodec.encode(u.chainHash :: u.shortChannelId :: u.messageFlags :: u.channelFlags ::
      u.cltvExpiryDelta :: u.htlcMinimumMsat :: u.feeBaseMsat :: u.feeProportionalMillionths :: u.htlcMaximumMsat :: HNil))
    crc32c(data)
  }
}
