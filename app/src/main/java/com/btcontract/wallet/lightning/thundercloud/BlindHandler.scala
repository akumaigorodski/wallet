package com.btcontract.wallet.lightning.thundercloud

import spray.json._
import ThundercloudProtocol._
import com.btcontract.wallet.helper.JsonHttpUtils._
import spray.json.{JsNumber => JN, JsString => JS}
import com.btcontract.wallet.R.string.lang
import com.btcontract.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import org.bitcoinj.core.ECKey
import java.math.BigInteger
import scala.util.Try


// Store tokens info and their connection to payment data in memory
case class BlindMemo(params: Seq[BlindParam], clears: Seq[BigInteger], sesKeyHex: String, rHash: String) {
  def completeCopy(charge: Charge) = (copy(rHash = HEX encode charge.lnPaymentData).toJson.toString, charge)
}

object BlindHandler {
  def sendData = thunder("blindtokens/info", identity) flatMap {
    case JS(signerQ) +: JS(signerR) +: JN(qty) +: JN(price) +: rest =>
      val signerSessionPubKey = ECKey.fromPublicOnly(HEX decode signerR)
      val signerMasterPubKey = ECKey.fromPublicOnly(HEX decode signerQ)
      val sesKeyHex = signerSessionPubKey.getPublicKeyAsHex

      // Prepare a list of BlindParam and a list of BigInteger clear tokens for each BlindParam
      val blinder = new ECBlind(signerMasterPubKey.getPubKeyPoint, signerSessionPubKey.getPubKeyPoint)
      val memo = BlindMemo(blinder makeList qty.toInt, blinder.generator take qty.toInt, sesKeyHex, rHash = null)
      val blindTokens = memo.clears zip memo.params map { case (clearToken, param) => param blind clearToken }

      // Only if proposed terms are within range
      if (qty < app.LNData.minTokensNum) throw new Exception("tooFewTokens")
      else if (price > app.LNData.maxPriceSat) throw new Exception("tooHighPrice")
      else thunder("blindtokens/buy", vec => memo completeCopy vec.head.convertTo[Charge],
        "lang", app getString lang, "tokens", blindTokens.toJson.toString, "seskey", sesKeyHex)
  }

  def tryRestore(raw: String) = Try apply to[BlindMemo](raw)
  def getClearSigs(rValue: String, memo: BlindMemo) = thunder("blindtokens/redeem",
    sigHats => for (sigHat <- sigHats) yield sigHat.convertTo[BigInteger],
    "rvalue", rValue, "seskey", memo.sesKeyHex)
}