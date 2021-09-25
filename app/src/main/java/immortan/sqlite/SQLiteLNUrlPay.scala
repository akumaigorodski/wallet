package immortan.sqlite

import spray.json._
import immortan.utils.ImplicitJsonFormats._
import immortan.{ChannelMaster, LNUrlDescription, LNUrlPayLink}
import java.lang.{Long => JLong}


class SQLiteLNUrlPay(db: DBInterface) {
  def updDescription(description: LNUrlDescription, domain: String, pay: String): Unit = db txWrap {
    db.change(LNUrlPayTable.updateDescriptionSql, description.toJson.compactPrint, pay)
    for (label <- description.label) addSearchableLink(label, domain)
    ChannelMaster.next(ChannelMaster.payMarketDbStream)
  }

  def remove(pay: String): Unit = {
    db.change(LNUrlPayTable.killSql, pay)
    ChannelMaster.next(ChannelMaster.payMarketDbStream)
  }

  def saveLink(info: LNUrlPayLink): Unit = db txWrap {
    val descriptionString = info.description.toJson.compactPrint
    db.change(LNUrlPayTable.newSql, info.domain, info.payString, info.payMetaString, info.updatedAt: JLong, descriptionString, info.lastNodeIdString, info.lastCommentString)
    db.change(LNUrlPayTable.updInfoSql, info.payMetaString, info.updatedAt: JLong, descriptionString, info.lastNodeIdString, info.lastCommentString, info.payString)
    addSearchableLink(info.payMetaData.get.queryText, info.domain)
    ChannelMaster.next(ChannelMaster.payMarketDbStream)
  }

  def addSearchableLink(search: String, domain: String): Unit = db.change(LNUrlPayTable.newVirtualSql, search.toLowerCase, domain)

  def searchLinks(rawSearchQuery: String): RichCursor = db.search(LNUrlPayTable.searchSql, rawSearchQuery.toLowerCase)

  def listRecentLinks(limit: Int): RichCursor = db.select(LNUrlPayTable.selectRecentSql, limit.toString)

  def toLinkInfo(rc: RichCursor): LNUrlPayLink =
    LNUrlPayLink(domain = rc string LNUrlPayTable.domain, payString = rc string LNUrlPayTable.pay, payMetaString = rc string LNUrlPayTable.payMeta,
      updatedAt = rc long LNUrlPayTable.updatedAt, description = to[LNUrlDescription](rc string LNUrlPayTable.description),
      lastNodeIdString = rc string LNUrlPayTable.lastNodeId, lastCommentString = rc string LNUrlPayTable.lastComment)
}