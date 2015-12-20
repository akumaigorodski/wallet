package com.btcontract.wallet

import scala.util.{Success, Try}
import org.json.{JSONObject, JSONArray}
import Utils.{Rates, strDollar, strEuro, strYuan, rand}
import concurrent.ExecutionContext.Implicits.global
import com.github.kevinsawicki.http.HttpRequest
import scala.concurrent.Future
import java.util.TimerTask


object FiatRates { me =>
  val timer = new java.util.Timer
  var rates: Try[Rates] = Utils.nullFail
  val providers = List(Blockchain, BitcoinCharts, BitcoinAverage, Bitpay)
  def reloadRates = me loadCurrentRates providers(rand nextInt providers.size)
  def again(msec: Long) = timer.schedule(new TimerTask { def run = reloadRates }, msec)

  def loadCurrentRates(provider: RatesProvider): Unit = {
    val result = Future(provider fromJSON HttpRequest.get(provider.url).body)
    result onComplete { case Success(ok) => me again 300000 case _ => me again 2500 }
    result onComplete { case ok: Success[Rates] => rates = ok case _ => }
  }
}

abstract class RatesProvider(val url: String) {
  val names = Map("USD" -> strDollar, "EUR" -> strEuro, "CNY" -> strYuan)
  def fromJSON(rawJsonData: String): Rates
}

class SimProv(override val url: String, key: String) extends RatesProvider(url) {
  def fromJSON(rawJsonData: String) = new JSONObject(rawJsonData) match { case json =>
    for (codeName <- names) yield (codeName._2, json getJSONObject codeName._1 getDouble key)
  }
}

object Blockchain extends SimProv("https://blockchain.info/ticker", "last")
object BitcoinCharts extends SimProv("http://api.bitcoincharts.com/v1/weighted_prices.json", "24h")
object BitcoinAverage extends SimProv("https://api.bitcoinaverage.com/ticker/global/all", "ask")

object Bitpay extends RatesProvider("https://bitpay.com/rates") {
  def toTuple(jsonObj: JSONObject) = (jsonObj getString "code", jsonObj getDouble "rate")
  def jsonArray2Seq(arr: JSONArray) = for (num <- 0 until arr.length) yield toTuple(arr getJSONObject num)

  def fromJSON(rawJsonData: String) = {
    val curMap = jsonArray2Seq(new JSONObject(rawJsonData) getJSONArray "data").toMap
    for (Tuple2(code, name) <- names) yield (name, curMap apply code)
  }
}