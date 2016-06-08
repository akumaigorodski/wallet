package com.btcontract.wallet.lightning.thundercloud

import java.math.BigInteger
import com.btcontract.wallet.R.string.lang
import spray.json._
import ThundercloudProtocol._
import org.bitcoinj.core.Utils.HEX
import com.btcontract.wallet.lightning.Tools
import com.btcontract.wallet.lightning.StateMachine
import com.btcontract.wallet.helper.JsonHttpUtils.thunder
import com.btcontract.wallet.Utils.{Bytes, app}
import org.bitcoinj.core.ECKey
import com.btcontract.wallet.AbstractKit
import spray.json.{JsNumber => JN, JsString => JS}


trait BlindState
case class BlindTerms(signerQ: ECKey, sesKey: ECKey, amount: Int, price: Int) extends BlindState
case class BlindMemo(params: List[BlindParam], clear: List[BigInteger], sesHash: String, rHash: String) extends BlindState

object BlindHandler {

}

class BlindHandler extends StateMachine[BlindState](Nil, null) {
  def fetchSignKeys = thunder("blindtokens/info", identity) foreach {
    case JS(signerQ) +: JS(signerR) +: JN(qty) +: JN(price) +: rest =>

      // Only if proposed terms are within range
      if (qty < app.LNData.minTokensNum) become(null, 'qtyTooLow)
      else if (price > app.LNData.maxPriceSat) become(null, 'priceTooHigh)
      else this process BlindTerms(ECKey.fromPublicOnly(HEX decode signerQ),
        ECKey.fromPublicOnly(HEX decode signerR), qty.toInt, price.toInt)

    case _ =>
      // Some weird response...
      become(null, 'wrongFormat)
  }

  def provideTokens(bt: BlindTerms) = {
    val blinder = new ECBlind(bt.signerQ.getPubKeyPoint, bt.sesKey.getPubKeyPoint)
    val (params, clears) = (blinder makeList bt.amount, blinder.generator take bt.amount)
    val blindTokens = params zip clears map { case (parameter, token) => parameter blind token }

    // Send purchase request and wait for lightning charge as response
    val obs = thunder("blindtokens/buy", _.to[Charge], "lang", app getString lang,
      "tokens", Tools stringToHex blindTokens.toJson.toString,
      "seskey", bt.sesKey.getPubKeyHash)

    obs foreach { charge =>
      val bm = BlindMemo(params.toList, clears.toList, bt.sesKey.getPublicKeyAsHex, "rHash")
      app.prefs.edit.putString(AbstractKit.BLIND_LAST_PARAMS, bm.toJson.toString).commit
      become(bm, 'awaitPayment)
    }
  }

  def addTokens(memo: BlindMemo) = {

  }

  def doProcess(change: Any) = (change, data) match {
    case (rValue: Bytes, memo: BlindMemo) => addTokens(memo)
    case (blindTerms: BlindTerms, null) => provideTokens(blindTerms)
    case ('initialize, null) => fetchSignKeys
  }
}