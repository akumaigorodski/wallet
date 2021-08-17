package immortan.sqlite


class SQLiteUsedDomains(db: DBInterface) {
  def add(domain: String, protocol: String): Unit = db.change(UsedDomainsTable.newSql, domain, System.currentTimeMillis: java.lang.Long, protocol)

  def getCount(domain: String): Option[Long] = db.select(UsedDomainsTable.selectCountSql, domain).headTry(_ long 0).toOption
}
