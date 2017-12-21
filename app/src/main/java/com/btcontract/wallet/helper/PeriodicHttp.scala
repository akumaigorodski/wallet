package com.btcontract.wallet.helper

import spray.json._
import JsonHttpUtils._
import DefaultJsonProtocol._

import scala.util.{Try, Success}
import rx.lang.scala.{Scheduler, Observable => Obs}
import scala.concurrent.duration.{Duration, DurationInt}
import com.btcontract.wallet.Utils.{nullFail, rand, app, strDollar, strEuro, strYuan}

import com.github.kevinsawicki.http.HttpRequest
import rx.lang.scala.schedulers.IOScheduler
import java.net.ProtocolException
import org.bitcoinj.core.Coin


object JsonHttpUtils {
  def obsOn[T](provider: => T, scheduler: Scheduler) =
    Obs.just(null).subscribeOn(scheduler).map(_ => provider)

  def retry[T](obs: Obs[T], pick: (Throwable, Int) => Duration, times: Range) =
    obs.retryWhen(_.zipWith(Obs from times)(pick) flatMap Obs.timer)

  def pickInc(err: Throwable, next: Int) = next.seconds
  def to[T : JsonFormat](raw: String) = raw.parseJson.convertTo[T]
  val get = HttpRequest.get(_: String, true) connectTimeout 15000

  // Observable which processes responses of form [ok, ...] or [error, why]
  def lncloud[T](path: String, trans: Vector[JsValue] => T, params: Object*) = {
    val httpRequest = HttpRequest.post(s"http://10.0.2.2:9001/$path", true, params:_*)
    if (app.orbotOnline) httpRequest.useProxy("127.0.0.1", 8118)

    obsOn(httpRequest.connectTimeout(15000).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: responseParams) => trans(responseParams)
      case _ => throw new Throwable
    }
  }
}

// Fiat rates containers
trait Rate { def now: Double }
case class AvgRate(ask: Double) extends Rate { def now = ask }
case class ChainRate(last: Double) extends Rate { def now = last }
case class BitpayRate(code: String, rate: Double) extends Rate { def now = rate }

trait RateProvider { val usd, eur, cny: Rate }
case class Blockchain(usd: ChainRate, eur: ChainRate, cny: ChainRate) extends RateProvider
case class Bitaverage(usd: AvgRate, eur: AvgRate, cny: AvgRate) extends RateProvider

object FiatRates { me =>
  type Rates = Map[String, Double]
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

  def go = retry(obsOn(reloadData, IOScheduler.apply), pickInc, 1 to 10)
    .repeatWhen(_ delay 15.minute).subscribe(fresh => rates = Success apply fresh)
}

// Fee rates providers
trait FeeProvider { def fee: Long }
case class BitgoFee(feePerKb: Long) extends FeeProvider { def fee = feePerKb }
case class CypherFee(medium_fee_per_kb: Long) extends FeeProvider { def fee = medium_fee_per_kb }
case class InsightFee(f6: BigDecimal) extends FeeProvider { def fee = (f6 * 100000000L).toLong }

object Fee { me =>
  var rate = Coin valueOf 100000L
  val default = Coin valueOf 100000L

  implicit val cypherFeeFmt = jsonFormat[Long, CypherFee](CypherFee, "medium_fee_per_kb")
  implicit val insightFeeFmt = jsonFormat[BigDecimal, InsightFee](InsightFee, "6")
  implicit val bitgoFeeFmt = jsonFormat[Long, BitgoFee](BitgoFee, "feePerKb")

  def reloadData = rand nextInt 5 match {
    case 0 => to[InsightFee](get("https://bitlox.io/api/utils/estimatefee?nbBlocks=6").body)
    case 1 => to[InsightFee](get("https://blockexplorer.com/api/utils/estimatefee?nbBlocks=6").body)
    case 2 => to[InsightFee](get(s"https://live.coin.space/api/utils/estimatefee?nbBlocks=6").body)
    case 3 => to[BitgoFee](get("https://www.bitgo.com/api/v1/tx/fee?numBlocks=6").body)
    case _ => to[CypherFee](get("http://api.blockcypher.com/v1/btc/main").body)
  }

  def go = retry(obsOn(reloadData, IOScheduler.apply), pickInc, 1 to 10)
    .repeatWhen(_ delay 20.minute).subscribe(prov => rate = Coin valueOf prov.fee)
}

// Tx Insight API formats
case class TxInput(txid: String, addr: String)
case class Tx(txid: String, vin: List[TxInput], confirmations: Int)

object Insight {
  type TxList = List[Tx]
  implicit val txInputFmt = jsonFormat[String, String, TxInput](TxInput, "txid", "addr")
  implicit val txFmt = jsonFormat[String, List[TxInput], Int, Tx](Tx, "txid", "vin", "confirmations")

  def reloadData(suffix: String) = rand nextInt 6 match {
    case 0 => get(s"https://localbitcoinschain.com/api/$suffix").body
    case 1 => get(s"https://search.bitaccess.co/api/$suffix").body
    case 2 => get(s"https://insight.bitpay.com/api/$suffix").body
    case 3 => get(s"https://blockexplorer.com/api/$suffix").body
    case 4 => get(s"https://live.coin.space/api/$suffix").body
    case _ => get(s"https://bitlox.io/api/$suffix").body
  }

  // Usage 1: watch transaction depth by it's id
  // Usage 2: search for a tx whose input has our anchor txid,
  // if such a tx is found it means our anchor output has been spent!
  def txs(addr: String) = retry(obsOn(reloadData(s"addrs/$addr/txs").parseJson.asJsObject
    .fields("items").convertTo[TxList], IOScheduler.apply), pickInc, 1 to 5) flatMap Obs.just
}