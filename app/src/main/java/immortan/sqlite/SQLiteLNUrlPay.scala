package immortan.sqlite

import spray.json._
import immortan.utils.ImplicitJsonFormats._
import immortan.{ChannelMaster, LNUrlDescription, LNUrlPayLink}
import java.lang.{Long => JLong}


class SQLiteLNUrlPay(db: DBInterface) {
  def updDescription(description: LNUrlDescription, domain: String, pay: String): Unit = db txWrap {
    for (label <- description.label) db.change(LNUrlPayTable.newVirtualSql, label, domain)
    db.change(LNUrlPayTable.updateDescriptionSql, description.toJson.compactPrint, pay)
    ChannelMaster.next(ChannelMaster.payMarketDbStream)
  }

  def remove(pay: String): Unit = {
    db.change(LNUrlPayTable.killSql, pay)
    ChannelMaster.next(ChannelMaster.payMarketDbStream)
  }

  def saveLink(info: LNUrlPayLink): Unit = db txWrap {
    val descriptionString = info.description.toJson.compactPrint
    db.change(LNUrlPayTable.newSql, info.domain, info.payMetaString, info.updatedAt: JLong, descriptionString, info.lastHashString, info.lastNodeIdString, info.lastCommentString)
    db.change(LNUrlPayTable.updInfoSql, info.payMetaString, info.updatedAt: JLong, descriptionString, info.lastHashString, info.lastNodeIdString, info.lastCommentString)
    db.change(LNUrlPayTable.newVirtualSql, info.payMetaData.get.queryText, info.domain)
    ChannelMaster.next(ChannelMaster.payMarketDbStream)
  }

  def searchLinks(rawSearchQuery: String): RichCursor = db.search(LNUrlPayTable.searchSql, rawSearchQuery)

  def listRecentLinks(limit: Int): RichCursor = db.select(LNUrlPayTable.selectRecentSql, limit.toString)

  def toLinkInfo(rc: RichCursor): LNUrlPayLink =
    LNUrlPayLink(domain = rc string LNUrlPayTable.domain, payString = rc string LNUrlPayTable.domain, payMetaString = rc string LNUrlPayTable.payMeta,
      updatedAt = rc long LNUrlPayTable.updatedAt, description = to[LNUrlDescription](rc string LNUrlPayTable.description), lastHashString = rc string LNUrlPayTable.lastHash,
      lastNodeIdString = rc string LNUrlPayTable.lastNodeId, lastCommentString = rc string LNUrlPayTable.lastComment)
}