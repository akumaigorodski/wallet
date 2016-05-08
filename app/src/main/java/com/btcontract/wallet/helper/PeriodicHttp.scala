package com.btcontract.wallet.helper

import scala.util.{Try, Success}
import rx.lang.scala.{Scheduler, Observable => Obs}
import scala.concurrent.duration.{Duration, DurationInt}
import com.btcontract.wallet.Utils.{Rates, nullFail, rand, app}
import com.btcontract.wallet.Utils.{strDollar, strEuro, strYuan}

import com.github.kevinsawicki.http.HttpRequest
import rx.lang.scala.schedulers.IOScheduler
import org.bitcoinj.core.Coin

import spray.json._
import JsonHttpUtils._
import DefaultJsonProtocol._


object JsonHttpUtils {
  type Selector = (Throwable, Int) => Duration
  def pickInc(err: Throwable, next: Int) = next.second
  def obsOn[T](provider: => T, scheduler: Scheduler) = Obs.just(null).subscribeOn(scheduler).map(_ => provider)
  def retry[T](obs: Obs[T], pick: Selector, times: Range) = obs.retryWhen(_.zipWith(Obs from times)(pick) flatMap Obs.timer)
  def withTor(http: HttpRequest) = if (app.orbotAllowed && app.orbotOnline) http.useProxy("127.0.0.1", 8118) else http

  def to[T : JsonFormat](raw: String) = raw.parseJson.convertTo[T]
  val post = HttpRequest.post(_: String, true) connectTimeout 15000
  val get = HttpRequest.get(_: String, true) connectTimeout 15000
}

object FiatRates { me =>
  type RatesMap = Map[String, Rate]
  type BitpayList = List[BitpayRate]
  var rates: Try[Rates] = nullFail

  implicit val avgRateFmt = jsonFormat[Double, AvgRate](AvgRate, "ask")
  implicit val chainRateFmt = jsonFormat[Double, ChainRate](ChainRate, "last")
  implicit val blockchainFmt = jsonFormat[ChainRate, ChainRate, ChainRate, Blockchain](Blockchain, "USD", "EUR", "CNY")
  implicit val bitaverageFmt = jsonFormat[AvgRate, AvgRate, AvgRate, Bitaverage](Bitaverage, "USD", "EUR", "CNY")
  implicit val bitpayRateFmt = jsonFormat[String, Double, BitpayRate](BitpayRate, "code", "rate")

  // Normalizing incoming json data and converting it to rates map
  def toRates(src: RateProvider) = Map(strDollar -> src.usd.now, strEuro -> src.eur.now, strYuan -> src.cny.now)
  def toRates(src: RatesMap) = Map(strDollar -> src("USD").now, strEuro -> src("EUR").now, strYuan -> src("CNY").now)
  def bitpayNorm(src: String) = src.parseJson.asJsObject.fields("data").convertTo[BitpayList].map(rt => rt.code -> rt).toMap

  def reloadData = rand nextInt 3 match {
    case 0 => me toRates to[Bitaverage](get("https://api.bitcoinaverage.com/ticker/global/all").body)
    case 1 => me toRates to[Blockchain](get("https://blockchain.info/ticker").body)
    case _ => me toRates bitpayNorm(get("https://bitpay.com/rates").body)
  }

  def go = retry(obsOn(reloadData, IOScheduler.apply), pickInc, 1 to 30)
    .repeatWhen(_ delay 15.minute).subscribe(fresh => rates = Success apply fresh)
}

// Fiat rates containers
trait Rate { def now: Double }
case class AvgRate(ask: Double) extends Rate { def now = ask }
case class ChainRate(last: Double) extends Rate { def now = last }
case class BitpayRate(code: String, rate: Double) extends Rate { def now = rate }

trait RateProvider { val usd, eur, cny: Rate }
case class Blockchain(usd: ChainRate, eur: ChainRate, cny: ChainRate) extends RateProvider
case class Bitaverage(usd: AvgRate, eur: AvgRate, cny: AvgRate) extends RateProvider

object Fee { me =>
  var rate = Coin valueOf 25000L
  val default = Coin valueOf 10000L

  implicit val cypherFeeFmt = jsonFormat[Long, CypherFee](CypherFee, "medium_fee_per_kb")
  implicit val insightFeeFmt = jsonFormat[BigDecimal, InsightFee](InsightFee, "3")
  implicit val bitgoFeeFmt = jsonFormat[Long, BitgoFee](BitgoFee, "feePerKb")

  def reloadData = rand nextInt 4 match {
    case 0 => to[InsightFee](get("https://blockexplorer.com/api/utils/estimatefee?nbBlocks=3").body)
    case 1 => to[InsightFee](get("http://bitlox.io/api/utils/estimatefee?nbBlocks=3").body)
    case 2 => to[BitgoFee](get("https://www.bitgo.com/api/v1/tx/fee?numBlocks=3").body)
    case _ => to[CypherFee](get("http://api.blockcypher.com/v1/btc/main").body)
  }

  def go = retry(obsOn(reloadData, IOScheduler.apply), pickInc, 1 to 30)
    .repeatWhen(_ delay 30.minute).subscribe(prov => rate = Coin valueOf prov.fee)
}

// Fee rates providers
trait FeeProvider { def fee: Long }
case class BitgoFee(feePerKb: Long) extends FeeProvider { def fee = feePerKb }
case class CypherFee(medium_fee_per_kb: Long) extends FeeProvider { def fee = medium_fee_per_kb }
case class InsightFee(f3: BigDecimal) extends FeeProvider { def fee = (f3 * 100000000L).toLong }

object Insight {
  def reloadData(suffix: String) = rand nextInt 3 match {
    case 0 => get(s"https://insight.bitpay.com/api/$suffix").body
    case 1 => get(s"https://blockexplorer.com/api/$suffix").body
    case _ => get(s"https://bitlox.io/api/$suffix").body
  }

  type TxList = List[Tx]
  implicit val txFmt = jsonFormat[String, Tx](Tx, "txid")
  def txs(addr: String) = retry(obsOn(reloadData(s"addrs/$addr/txs?from=0&to=50").parseJson
    .asJsObject.fields("items").convertTo[TxList], IOScheduler.apply), pickInc, 1 to 3)
}

// Insight API formats
case class Tx(txid: String)