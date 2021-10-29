package immortan.utils

import java.text._

import fr.acinq.eclair._


object Denomination {
  val locale = new java.util.Locale("en", "US")
  val symbols = new DecimalFormatSymbols(locale)
  val formatFiatPrecise = new DecimalFormat("#,###,###.##")
  val formatFiat = new DecimalFormat("#,###,###")
  val btcFactor = 100000000000L

  formatFiatPrecise setDecimalFormatSymbols symbols
  formatFiat setDecimalFormatSymbols symbols

  def satCeil(msat: MilliSatoshi): MilliSatoshi = (1000L * (msat.toLong / 1000D).ceil).toLong.msat

  def mast2BtcBigDecimal(msat: MilliSatoshi): BigDecimal = BigDecimal(msat.toLong) / btcFactor

  def btcBigDecimal2MSat(btc: BigDecimal): MilliSatoshi = (btc * btcFactor).toLong.msat
}

trait Denomination {
  def parsed(msat: MilliSatoshi, mainColor: String, zeroColor: String): String

  def fromMsat(amount: MilliSatoshi): BigDecimal = BigDecimal(amount.toLong) / factor

  def parsedWithSign(msat: MilliSatoshi, mainColor: String, zeroColor: String): String = parsed(msat, mainColor, zeroColor) + "\u00A0" + sign

  def directedWithSign(incoming: MilliSatoshi, outgoing: MilliSatoshi, inColor: String, outColor: String, zeroColor: String, isIncoming: Boolean): String = {
    if (isIncoming && incoming == 0L.msat) parsedWithSign(incoming, inColor, zeroColor)
    else if (isIncoming) "+&#160;" + parsedWithSign(incoming, inColor, zeroColor)
    else if (outgoing == 0L.msat) parsedWithSign(outgoing, outColor, zeroColor)
    else "-&#160;" + parsedWithSign(outgoing, outColor, zeroColor)
  }

  val fmt: DecimalFormat
  val factor: Long
  val sign: String
}

object SatDenomination extends Denomination { me =>
  val fmt: DecimalFormat = new DecimalFormat("###,###,###")
  fmt.setDecimalFormatSymbols(Denomination.symbols)
  val factor = 1000L
  val sign = "sat"

  def parsed(msat: MilliSatoshi, mainColor: String, zeroColor: String): String = {
    // Zero color is not used in SAT denomination since it has no decimal parts
    val basicFormatted = fmt.format(me fromMsat msat)
    s"<font color=$mainColor>$basicFormatted</font>"
  }
}