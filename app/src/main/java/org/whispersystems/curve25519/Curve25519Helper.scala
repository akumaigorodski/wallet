package org.whispersystems.curve25519

import com.btcontract.wallet.lightning.Tools.Bytes
import com.btcontract.wallet.Utils.rand
import org.bitcoinj.core.Sha256Hash


object Curve25519Helper {
  private[this] val provider = new OpportunisticCurve25519Provider
  def agreementHash(priv: Bytes, pub: Bytes) = Sha256Hash hashTwice provider.calculateAgreement(priv, pub)
  def verify(src: Bytes, signature: Bytes, pub25519: Bytes) = provider.verifySignature(pub25519, src, signature)
  def sign(src: Bytes, priv25519: Bytes) = provider.calculateSignature(rand getBytes 32, priv25519, src)

  def keyPair(rand32: Bytes) = {
    val privateKey = provider generatePrivateKey rand32
    List(provider generatePublicKey privateKey, privateKey)
  }
}