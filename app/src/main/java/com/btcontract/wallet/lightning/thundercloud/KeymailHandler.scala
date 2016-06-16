package com.btcontract.wallet.lightning.thundercloud

import spray.json._
import ThundercloudProtocol._
import com.btcontract.wallet.helper.JsonHttpUtils._
import org.bitcoinj.core.ECKey.ECDSASignature
import org.bitcoinj.core.Utils.HEX


object KeymailHandler {
  private val toSignedMail: Vector[JsValue] => SignedMail = _.head.convertTo[SignedMail]
  def get(keymail: String) = thunder("keymail/get", toSignedMail, "value", keymail) map verify
  def confirm(secret: String) = thunder("keymail/confirm", toSignedMail, "secret", secret) map verify

  def verify(signedMail: SignedMail) = {
    val clientSig = ECDSASignature decodeFromDER HEX.decode(signedMail.signature)
    signedMail.pubKeyClass.verifyOrThrow(signedMail.emailHash, clientSig)
    signedMail
  }
}
