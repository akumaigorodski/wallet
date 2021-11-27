package immortan.utils

import immortan.DataBag
import immortan.utils.ImplicitJsonFormats._
import immortan.crypto.{CanBeShutDown, Tools}
import rx.lang.scala.{Observable, Subscription}


object FiatRates {
  type BlockchainInfoItemMap = Map[String, BlockchainInfoItem]
  type CoinGeckoItemMap = Map[String, CoinGeckoItem]
  type BitpayItemList = List[BitpayItem]
}

class FiatRates(bag: DataBag) extends CanBeShutDown {
  val customFiatSymbols: Map[String, String] = Map("rub" -> "\u20BD", "usd" -> "$", "inr" -> "₹", "gbp" -> "£", "cny" -> "CN¥", "jpy" -> "¥", "brl" -> "R$", "eur" -> "€", "krw" -> "₩")

  val universallySupportedSymbols: Map[String, String] = Map("usd" -> "US Dollar", "eur" -> "Euro", "jpy" -> "Japanese Yen", "cny" -> "Chinese Yuan", "inr" -> "Indian Rupee", "cad" -> "Canadian Dollar",
    "rub" -> "Русский Рубль", "brl" -> "Real Brasileiro", "czk" -> "Česká Koruna", "gbp" -> "Pound Sterling", "aud" -> "Australian Dollar", "try" -> "Turkish Lira", "nzd" -> "New Zealand Dollar",
    "thb" -> "Thai Baht", "twd" -> "New Taiwan Dollar", "krw" -> "South Korean won", "clp" -> "Chilean Peso", "sgd" -> "Singapore Dollar", "hkd" -> "Hong Kong Dollar", "pln" -> "Polish złoty",
    "dkk" -> "Danish Krone", "sek" -> "Swedish Krona", "chf" -> "Swiss franc", "huf" -> "Hungarian forint")

  def reloadData: Tools.Fiat2Btc = fr.acinq.eclair.secureRandom nextInt 3 match {
    case 0 => to[CoinGecko](Tools.get("https://api.coingecko.com/api/v3/exchange_rates").string).rates.map { case (code, item) => code.toLowerCase -> item.value }
    case 1 => to[FiatRates.BlockchainInfoItemMap](Tools.get("https://blockchain.info/ticker").string).map { case (code, item) => code.toLowerCase -> item.last }
    case _ => to[Bitpay](Tools.get("https://bitpay.com/rates").string).data.map { case BitpayItem(code, rate) => code.toLowerCase -> rate }.toMap
  }

  override def becomeShutDown: Unit = {
    subscription.unsubscribe
    listeners = Set.empty
  }

  var listeners: Set[FiatRatesListener] = Set.empty
  var info: FiatRatesInfo = bag.tryGetFiatRatesInfo getOrElse {
    FiatRatesInfo(rates = Map.empty, oldRates = Map.empty, stamp = 0L)
  }

  private[this] val periodSecs = 60 * 30
  private[this] val retryRepeatDelayedCall: Observable[Tools.Fiat2Btc] = {
    val retry = Rx.retry(Rx.ioQueue.map(_ => reloadData), Rx.incSec, 3 to 18 by 3)
    val repeat = Rx.repeat(retry, Rx.incSec, periodSecs to Int.MaxValue by periodSecs)
    Rx.initDelay(repeat, info.stamp, periodSecs * 1000L)
  }

  val subscription: Subscription = retryRepeatDelayedCall.subscribe(newRates => {
    info = FiatRatesInfo(newRates, info.rates, System.currentTimeMillis)
    for (lst <- listeners) lst.onFiatRates(info)
  }, Tools.none)
}

trait FiatRatesListener {
  def onFiatRates(rates: FiatRatesInfo): Unit
}

case class CoinGeckoItem(value: Double)
case class BlockchainInfoItem(last: Double)
case class BitpayItem(code: String, rate: Double)

case class Bitpay(data: FiatRates.BitpayItemList)
case class CoinGecko(rates: FiatRates.CoinGeckoItemMap)

case class FiatRatesInfo(rates: Tools.Fiat2Btc, oldRates: Tools.Fiat2Btc, stamp: Long) {
  def pctDifference(code: String): Option[String] = List(rates get code, oldRates get code) match {
    case Some(fresh) :: Some(old) :: Nil if fresh > old => Some(s"<font color=#8BD670><small>▲</small> ${Denomination.formatFiatPrecise format pctChange(fresh, old).abs}%</font>")
    case Some(fresh) :: Some(old) :: Nil if fresh < old => Some(s"<small>▼</small> ${Denomination.formatFiatPrecise format pctChange(fresh, old).abs}%")
    case _ => None
  }

  def pctChange(fresh: Double, old: Double): Double = (fresh - old) / old * 100
}
