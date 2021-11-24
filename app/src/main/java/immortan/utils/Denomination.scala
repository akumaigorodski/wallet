package immortan.utils

import java.text._

import fr.acinq.eclair._


object Denomination {
  val locale = new java.util.Locale("en", "US")
  val symbols = new DecimalFormatSymbols(locale)
  val formatFiatPrecise = new DecimalFormat("#,###,###.##")
  val formatFiat = new DecimalFormat("#,###,###")

  formatFiatPrecise setDecimalFormatSymbols symbols
  formatFiat setDecimalFormatSymbols symbols

  def btcBigDecimal2MSat(btc: BigDecimal): MilliSatoshi = (btc * BtcDenomination.factor).toLong.msat

  def msat2BtcBigDecimal(msat: MilliSatoshi): BigDecimal = BigDecimal(msat.toLong) / BtcDenomination.factor
}

trait Denomination { me =>
  def parsed(msat: MilliSatoshi, mainColor: String, zeroColor: String): String

  def parsedWithSign(msat: MilliSatoshi, mainColor: String, zeroColor: String): String

  def fromMsat(amount: MilliSatoshi): BigDecimal = BigDecimal(amount.toLong) / factor

  def directedWithSign(incoming: MilliSatoshi, outgoing: MilliSatoshi,
                       inColor: String, outColor: String, zeroColor: String,
                       isIncoming: Boolean): String = {

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

  def parsedWithSign(msat: MilliSatoshi, mainColor: String, zeroColor: String): String =
    parsed(msat, mainColor, zeroColor) + "\u00A0" + sign

  def parsed(msat: MilliSatoshi, mainColor: String, zeroColor: String): String =
    s"<font color=$mainColor>" + fmt.format(me fromMsat msat) + "</font>"
}

object BtcDenomination extends Denomination { me =>
  val fmt: DecimalFormat = new DecimalFormat("##0.00000000")
  fmt.setDecimalFormatSymbols(Denomination.symbols)
  val factor = 100000000000L
  val sign = "btc"

  def parsedWithSign(msat: MilliSatoshi, mainColor: String, zeroColor: String): String = parsed(msat, mainColor, zeroColor)

  def parsed(msat: MilliSatoshi, mainColor: String, zeroColor: String): String = {
    // Alpha channel does not work on Android when set as HTML attribute
    // hence zero color is supplied to match different backgrounds well
    if (0L == msat.toLong) return "0"

    val basicFormatted = fmt.format(me fromMsat msat)
    val (whole, decimal) = basicFormatted.splitAt(basicFormatted indexOf ".")
    val bld = new StringBuilder(decimal).insert(3, ",").insert(7, ",").insert(0, whole)
    val splitIndex = bld.indexWhere(char => char != '0' && char != '.' && char != ',')
    val finalSplitIndex = if (".00000000" == decimal) splitIndex - 1 else splitIndex
    val (finalWhole, finalDecimal) = bld.splitAt(finalSplitIndex)

    new StringBuilder("<font color=").append(zeroColor).append('>').append(finalWhole).append("</font>")
      .append("<font color=").append(mainColor).append('>').append(finalDecimal).append("</font>")
      .toString
  }
}