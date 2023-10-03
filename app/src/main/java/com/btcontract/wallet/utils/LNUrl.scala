package com.btcontract.wallet.utils

import com.btcontract.wallet.WalletApp
import com.google.common.base.CharMatcher
import fr.acinq.bitcoin.{Bech32, ByteVector32, ByteVector64, Crypto}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet
import fr.acinq.eclair.wire.NodeAddress
import immortan.utils.ImplicitJsonFormats._
import immortan.utils.Rx
import immortan.utils.uri.Uri
import rx.lang.scala.Observable
import spray.json._

import scala.util.Try


object LNUrl {
  def fromIdentifier(identifier: String): LNUrl = {
    val (user, domain) = identifier.splitAt(identifier indexOf '@')
    val isOnionDomain: Boolean = domain.endsWith(NodeAddress.onionSuffix)
    if (isOnionDomain) LNUrl(s"http://$domain/.well-known/lnurlp/$user")
    else LNUrl(s"https://$domain/.well-known/lnurlp/$user")
  }

  def fromBech32(bech32url: String): LNUrl = {
    val Tuple3(_, dataBody, _) = Bech32.decode(bech32url)
    val request = new String(Bech32.five2eight(dataBody), "UTF-8")
    LNUrl(request)
  }

  def checkHost(host: String): Uri = Uri.parse(host) match { case uri =>
    val isOnion = host.startsWith("http://") && uri.getHost.endsWith(NodeAddress.onionSuffix)
    val isSSLPlain = host.startsWith("https://") && !uri.getHost.endsWith(NodeAddress.onionSuffix)
    require(isSSLPlain || isOnion, "URI is neither Plain/HTTPS nor Onion/HTTP request")
    uri
  }

  def guardResponse(raw: String): String = {
    val parseAttempt = Try(raw.parseJson.asJsObject.fields)
    val hasErrorDescription = parseAttempt.map(_ apply "reason").map(json2String)
    val hasError = parseAttempt.map(_ apply "status").map(json2String).filter(_.toUpperCase == "ERROR")
    if (hasErrorDescription.isSuccess) throw new Exception(s"Error from vendor: ${hasErrorDescription.get}")
    else if (hasError.isSuccess) throw new Exception(s"Error from vendor: no description provided")
    else if (parseAttempt.isFailure) throw new Exception(s"Invalid json from vendor: $raw")
    raw
  }

  def level2DataResponse(bld: Uri.Builder): Observable[String] = Rx.ioQueue.map { _ =>
    guardResponse(ElectrumWallet.connectionProvider.get(bld.build.toString).string)
  }
}

case class LNUrl(request: String) {
  val uri: Uri = LNUrl.checkHost(request)
  val warnUri: String = uri.getHost.map { char =>
    if (CharMatcher.ascii matches char) char.toString
    else s"<b>[$char]</b>"
  }.mkString

  lazy val k1: Try[String] = Try(uri getQueryParameter "k1")
  lazy val isAuth: Boolean = Try(uri.getQueryParameter("tag").toLowerCase == "login").getOrElse(false)
  lazy val authAction: String = Try(uri.getQueryParameter("action").toLowerCase).getOrElse("login")
}

case class LNUrlAuthSpec(host: String, k1: ByteVector32) {
  val linkingPrivKey: Crypto.PrivateKey = WalletApp.secret.keys.makeLinkingKey(host)
  val linkingPubKey: String = linkingPrivKey.publicKey.toString

  def signature: ByteVector64 = Crypto.sign(k1, linkingPrivKey)
  def derSignatureHex: String = Crypto.compact2der(signature).toHex
}