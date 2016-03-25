package com.btcontract.wallet.helper

import scala.util.{Try, Success}
import com.btcontract.wallet.Utils.app
import concurrent.ExecutionContext.Implicits.global
import com.github.kevinsawicki.http.HttpRequest
import scala.concurrent.Future
import org.bitcoinj.core.Coin
import java.util.TimerTask

import spray.json._
import JsonHttpUtils._
import DefaultJsonProtocol._
import com.btcontract.wallet.Utils._


object JsonHttpUtils {
  def to[T : JsonFormat](raw: String) = raw.parseJson.convertTo[T]
  val post = HttpRequest.post(_: String, true) connectTimeout 15000
  val get = HttpRequest.get(_: String, true) connectTimeout 15000
  def fetch(ready: HttpRequest) = Future(ready.body)

  // Future with side effects
  def %[T](result: Future[T], onErr: => Unit, onOk: => Unit) = {
    result onComplete { case Success(res) => onOk case _ => onErr }
    result
  }
}

object FiatRates { me =>
  type RatesMap = Map[String, Rate]
  type BitpayList = List[BitpayRate]
  var rates: Try[Rates] = nullFail

  implicit val bitpayRateFmt = jsonFormat[String, Double, BitpayRate](BitpayRate.apply, "code", "rate")
  implicit val blockchainRateFmt = jsonFormat[Double, BlockchainRate](BlockchainRate.apply, "last")
  implicit val bitaverageRateFmt = jsonFormat[Double, BitaverageRate](BitaverageRate.apply, "ask")

  implicit val blockchainFmt = jsonFormat[BlockchainRate, BlockchainRate,
    BlockchainRate, Blockchain](Blockchain.apply, "USD", "EUR", "CNY")

  implicit val bitaverageFmt = jsonFormat[BitaverageRate, BitaverageRate,
    BitaverageRate, Bitaverage](Bitaverage.apply, "USD", "EUR", "CNY")

  def toRates(src: RateProvider) = Map(strDollar -> src.usd.now, strEuro -> src.eur.now, strYuan -> src.cny.now)
  def toRates(src: RatesMap) = Map(strDollar -> src("USD").now, strEuro -> src("EUR").now, strYuan -> src("CNY").now)
  def bitpayNorm(src: String) = src.parseJson.asJsObject.fields("data").convertTo[BitpayList].map(rt => rt.code -> rt).toMap
  def go = %(reloadData, me retry 2000, me retry 1200000) onComplete { case rateData@Success(_) => rates = rateData case _ => }
  def retry(waitPeriod: Long) = app.timer.schedule(task, waitPeriod)
  def task: TimerTask = new TimerTask { def run = go }

  def reloadData = rand nextInt 3 match {
    case 0 => (get andThen fetch)("https://api.bitcoinaverage.com/ticker/global/all") map to[Bitaverage] map toRates
    case 1 => (get andThen fetch)("https://blockchain.info/ticker") map to[Blockchain] map toRates
    case _ => (get andThen fetch)("https://bitpay.com/rates") map bitpayNorm map toRates
  }
}

// Fiat rates containers
trait Rate { def now: Double }
case class BitaverageRate(ask: Double) extends Rate { def now = ask }
case class BlockchainRate(last: Double) extends Rate { def now = last }
case class BitpayRate(code: String, rate: Double) extends Rate { def now = rate }

trait RateProvider { val usd, eur, cny: Rate }
case class Blockchain(usd: BlockchainRate, eur: BlockchainRate, cny: BlockchainRate) extends RateProvider
case class Bitaverage(usd: BitaverageRate, eur: BitaverageRate, cny: BitaverageRate) extends RateProvider

object Fee { me =>
  var rate = Coin valueOf 15000L
  val default = Coin valueOf 10000L
  implicit val bitgoFeeFmt = jsonFormat[Long, BitgoFee](BitgoFee.apply, "feePerKb")
  implicit val insightFeeFmt = jsonFormat[BigDecimal, InsightFee](InsightFee.apply, "12")
  implicit val blockcypherFeeFmt = jsonFormat[Long, BlockcypherFee](BlockcypherFee.apply, "low_fee_per_kb")
  def go = %(reloadData, me retry 2000, me retry 1800000) foreach { prov => rate = Coin valueOf prov.fee }
  def retry(waitPeriod: Long) = app.timer.schedule(task, waitPeriod)
  def task: TimerTask = new TimerTask { def run = go }

  def reloadData = rand nextInt 3 match {
    case 0 => (get andThen fetch)("https://www.bitgo.com/api/v1/tx/fee?numBlocks=12") map to[BitgoFee]
    case 1 => (get andThen fetch)("https://blockexplorer.com/api/utils/estimatefee?nbBlocks=12") map to[InsightFee]
    case 2 => (get andThen fetch)("http://api.blockcypher.com/v1/btc/main") map to[BlockcypherFee]
  }
}

// Fee rates providers
trait FeeProvider { def fee: Long }
case class BitgoFee(feePerKb: Long) extends FeeProvider { def fee = feePerKb }
case class BlockcypherFee(low_fee_per_kb: Long) extends FeeProvider { def fee = low_fee_per_kb }
case class InsightFee(fee12: BigDecimal) extends FeeProvider { def fee = (fee12 * 100000000).toLong }