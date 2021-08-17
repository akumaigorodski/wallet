package immortan.sqlite

import fr.acinq.eclair._
import java.lang.{Long => JLong}
import immortan.{ChannelMaster, LNUrlLinkInfo}
import fr.acinq.bitcoin.Crypto
import scodec.bits.ByteVector


object SQLiteLNUrl {
  def toLocator(info: LNUrlLinkInfo): String = {
    val material = info.domain + info.payString + info.nextWithdrawString
    val materialBytes = ByteVector.view(material.trim.getBytes)
    Crypto.sha256(materialBytes).toHex
  }
}

class SQLiteLNUrl(db: DBInterface) {
  def updateLabel(locator: String, newLabel: String, domain: String): Unit = {
    db.change(LNUrlTable.newVirtualSql, newLabel, domain)
    db.change(LNUrlTable.updLabelSql, newLabel, locator)
    ChannelMaster.next(ChannelMaster.payMarketDbStream)
  }

  def remove(locator: String): Unit = {
    db.change(LNUrlTable.killSql, locator)
    ChannelMaster.next(ChannelMaster.payMarketDbStream)
  }

  def saveLink(locator: String, info: LNUrlLinkInfo, proof: Crypto.PrivateKey = randomKey): Unit = db txWrap {
    db.change(LNUrlTable.newSql, info.domain, locator, info.payString, info.nextWithdrawString, info.payMetaString,
      info.lastMsat.toLong: JLong, info.lastDate: JLong, info.lastHashString, info.lastPayNodeIdString,
      info.lastBalanceLong: JLong, info.lastPayCommentString, proof.value.toHex, info.label)

    db.change(LNUrlTable.updPayInfoSql, info.payMetaString, info.lastMsat.toLong: JLong, info.lastDate: JLong,
      info.lastHashString, info.lastPayNodeIdString, info.lastPayCommentString, proof.value.toHex, info.locator)

    db.change(LNUrlTable.newVirtualSql, info.payMetaData.get.queryText, info.domain)
    ChannelMaster.next(ChannelMaster.payMarketDbStream)
  }

  def searchLinks(rawSearchQuery: String): RichCursor = db.search(LNUrlTable.searchSql, rawSearchQuery)

  def listRecentLinks(limit: Int): RichCursor = db.select(LNUrlTable.selectRecentSql, limit.toString)

  def toLinkInfo(rc: RichCursor): LNUrlLinkInfo =
    LNUrlLinkInfo(domain = rc string LNUrlTable.domain, locator = rc string LNUrlTable.locator, payString = rc string LNUrlTable.lnurlPay,
      nextWithdrawString = rc string LNUrlTable.nextLnurlWithdraw, payMetaString = rc string LNUrlTable.payMeta, lastMsat = MilliSatoshi(rc long LNUrlTable.lastMsat),
      lastDate = rc long LNUrlTable.lastDate, lastHashString = rc string LNUrlTable.lastHash, lastPayNodeIdString = rc string LNUrlTable.lastPayNodeId,
      lastBalanceLong = rc long LNUrlTable.lastBalance, lastPayCommentString = rc string LNUrlTable.lastPayComment,
      labelString = rc string LNUrlTable.label)
}