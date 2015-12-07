package com.btcontract.wallet

import scala.util.{Failure, Success, Try}
import Utils.{Rates, strDollar, strEuro, strYuan, rand}
import concurrent.ExecutionContext.Implicits.global
import com.github.kevinsawicki.http.HttpRequest
import scala.concurrent.Future
import java.util.TimerTask
import org.json.JSONObject


object FiatRates { me =>
  val timer = new java.util.Timer
  var rates: Try[Rates] = Failure(new NoSuchElementException)
  val providers = List(Blockchain, Blockchain, BitcoinCharts, BitcoinCharts, BitcoinAverage)
  def again(msec: Long) = timer.schedule(new TimerTask { def run = reloadRates }, msec)
  def reloadRates = me loadCurrentRates providers(rand nextInt providers.size)

  def loadCurrentRates(provider: RatesProvider): Unit = {
    val res = Future(provider fromJSON HttpRequest.get(provider.url).body)
    res onComplete { case Success(ok) => me again 300000 case _ => me again 5000 }
    res onComplete { case ok: Success[Rates] => rates = ok case _ => }
  }
}

trait RatesProvider {
  def fromJSON(raw: String): Rates
  val url: String
}

object Blockchain extends RatesProvider {
  val url = "https://blockchain.info/ticker"
  def fromJSON(raw: String) = new JSONObject(raw) match { case json =>
    Map.empty.updated(strDollar, json getJSONObject "USD" getDouble "last")
      .updated(strEuro, json getJSONObject "EUR" getDouble "last")
      .updated(strYuan, json getJSONObject "CNY" getDouble "last")
  }
}

object BitcoinCharts extends RatesProvider {
  val url = "http://api.bitcoincharts.com/v1/weighted_prices.json"
  def fromJSON(raw: String) = new JSONObject(raw) match { case json =>
    Map.empty.updated(strDollar, json getJSONObject "USD" getDouble "24h")
      .updated(strEuro, json getJSONObject "EUR" getDouble "24h")
      .updated(strYuan, json getJSONObject "CNY" getDouble "24h")
  }
}

object BitcoinAverage extends RatesProvider {
  val url = "https://api.bitcoinaverage.com/ticker/global/all"
  def fromJSON(raw: String) = new JSONObject(raw) match { case json =>
    Map.empty.updated(strDollar, json getJSONObject "USD" getDouble "ask")
      .updated(strEuro, json getJSONObject "EUR" getDouble "ask")
      .updated(strYuan, json getJSONObject "CNY" getDouble "ask")
  }
}