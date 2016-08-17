package com.btcontract.wallet.lightning.lncloud

import spray.json._
import ThundercloudProtocol._
import com.btcontract.wallet.helper.JsonHttpUtils._


object KeymailHandler {
  private val toSignedMail: Vector[JsValue] => SignedMail = _.head.convertTo[SignedMail]
  def get(keymail: String) = lncloud("keymail/get", toSignedMail, "value", keymail) map verify
  def confirm(secret: String) = lncloud("keymail/confirm", toSignedMail, "secret", secret) map verify
  def verify(sm: SignedMail) = if (sm.checkSig) sm else throw new SecurityException
}