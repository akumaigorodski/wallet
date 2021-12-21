package fr.acinq.eclair


case class CltvExpiry(underlying: Long) extends AnyVal {
  def +(d: CltvExpiryDelta): CltvExpiry = CltvExpiry(underlying + d.underlying)
  def -(d: CltvExpiryDelta): CltvExpiry = CltvExpiry(underlying - d.underlying)
  def >=(d: CltvExpiry): Boolean = underlying >= d.underlying
  def <=(d: CltvExpiry): Boolean = underlying <= d.underlying
  def <(d: CltvExpiry): Boolean = underlying < d.underlying

  def -(other: CltvExpiry): CltvExpiryDelta = {
    val difference = underlying - other.underlying
    CltvExpiryDelta(difference.toInt)
  }
}

case class CltvExpiryDelta(underlying: Int) extends AnyVal {
  def toCltvExpiry(blockHeight: Long): CltvExpiry = CltvExpiry(blockHeight + underlying)
  def +(d: CltvExpiryDelta): CltvExpiryDelta = CltvExpiryDelta(underlying + d.underlying)
  def -(d: CltvExpiryDelta): CltvExpiryDelta = CltvExpiryDelta(underlying - d.underlying)
  def <=(d: CltvExpiryDelta): Boolean = underlying <= d.underlying
  def >(d: CltvExpiryDelta): Boolean = underlying > d.underlying
}
