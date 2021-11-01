package com.btcontract.wallet

import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, curve, one}
import fr.acinq.bitcoin.{Base58, Base58Check, ByteVector64, Crypto}
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import androidx.test.ext.junit.runners.AndroidJUnit4
import org.bouncycastle.crypto.signers.ECDSASigner
import fr.acinq.bitcoin.Base58.Prefix
import org.bitcoin.Secp256k1Context
import org.junit.Assert.assertTrue
import org.junit.runner.RunWith
import scodec.bits.ByteVector
import java.math.BigInteger

import immortan.utils.Haiku
import org.junit.Test


@RunWith(classOf[AndroidJUnit4])
class NativeSpec {

  def decodeSignatureCompact(signature: ByteVector64): (BigInteger, BigInteger) = {
    val r = new BigInteger(1, signature.take(32).toArray)
    val s = new BigInteger(1, signature.takeRight(32).toArray)
    (r, s)
  }

  def verifySignatureFallback(data: ByteVector, signature: ByteVector64, publicKey: PublicKey): Boolean = {
    val (r, s) = decodeSignatureCompact(signature)
    require(r.compareTo(one) >= 0, "r must be >= 1")
    require(r.compareTo(curve.getN) < 0, "r must be < N")
    require(s.compareTo(one) >= 0, "s must be >= 1")
    require(s.compareTo(curve.getN) < 0, "s must be < N")

    val signer = new ECDSASigner
    val params = new ECPublicKeyParameters(publicKey.ecpoint, curve)
    signer.init(false, params)
    signer.verifySignature(data.toArray, r, s)
  }

  @Test
  def useAppContext: Unit = {
    assertTrue(Secp256k1Context.isEnabled)
  }

  @Test
  def utxoNames: Unit = {
    val generated = for (_ <- 0 to 100) yield Haiku.name(fr.acinq.eclair.randomBytes32)
    assert(generated.toSet.size == generated.size)
  }

  @Test
  def nativeSecpIsFast: Unit = {
    val privateKey = PrivateKey.fromBase58("cRp4uUnreGMZN8vB7nQFX6XWMHU5Lc73HMAhmcDEwHfbgRS66Cqp", Base58.Prefix.SecretKeyTestnet)._1
    val data = Crypto.sha256(ByteVector("this is a test".getBytes))
    val publicKey = privateKey.publicKey

    val native = {
      val sig = Crypto.sign(data, privateKey)
      val a = System.currentTimeMillis()
      for (_ <- 0 to 100) {
        Crypto.verifySignature(data, sig, publicKey)
      }
      System.currentTimeMillis() - a
    }

    val fallback = {
      val sig = Crypto.sign(data, privateKey)
      val a = System.currentTimeMillis()
      for (_ <- 0 to 100) {
        verifySignatureFallback(data, sig, publicKey)
      }
      System.currentTimeMillis() - a
    }

    println(s"fallback: $fallback")
    println(s"native: $native")

    assertTrue(fallback / 10 > native)
  }

  @Test
  def generatePubFromPriv: Unit = {
    val privateKey = PrivateKey(ByteVector.fromValidHex("18E14A7B6A307F426A94F8114701E7C8E774E7F9A47E2C2035DB29A206321725"))
    val publicKey = privateKey.publicKey

    assertTrue(publicKey.toUncompressedBin == ByteVector.fromValidHex("0450863ad64a87ae8a2fe83c1af1a8403cb53f53e48" +
      "6d8511dad8a04887e5b23522cd470243453a299fa9e77237716103abc11a1df38855ed6f2ee187e9c582ba6"))

    val address = Base58Check.encode(Prefix.PubkeyAddress, Crypto.hash160(publicKey.toUncompressedBin))

    assertTrue(address == "16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM")
  }

  @Test
  def signAndVerifySigs: Unit = {
    val privateKey = PrivateKey.fromBase58("cRp4uUnreGMZN8vB7nQFX6XWMHU5Lc73HMAhmcDEwHfbgRS66Cqp", Base58.Prefix.SecretKeyTestnet)._1
    val data = Crypto.sha256(ByteVector("this is a test".getBytes))
    val sig = Crypto.sign(data, privateKey)
    val publicKey = privateKey.publicKey

    assertTrue(Crypto.verifySignature(data, sig, publicKey))
  }

  @Test
  def generateDeterministicSigs: Unit = {
    // dataset from https://bitcointalk.org/index.php?topic=285142.msg3299061#msg3299061
    val dataset = Seq(
      (
        ByteVector.fromValidHex("0000000000000000000000000000000000000000000000000000000000000001"),
        "Satoshi Nakamoto",
        ByteVector.fromValidHex("3045022100934b1ea10a4b3c1757e2b0c017d0b6143ce3c9a7e6a4a49860d7a6ab210ee3d802202442ce9d2b916064108014783e923ec36b49743e2ffa1c4496f01a512aafd9e5")
      ),
      (
        ByteVector.fromValidHex("0000000000000000000000000000000000000000000000000000000000000001"),
        "Everything should be made as simple as possible, but not simpler.",
        ByteVector.fromValidHex("3044022033a69cd2065432a30f3d1ce4eb0d59b8ab58c74f27c41a7fdb5696ad4e6108c902206f807982866f785d3f6418d24163ddae117b7db4d5fdf0071de069fa54342262")
      ),
      (
        ByteVector.fromValidHex("0000000000000000000000000000000000000000000000000000000000000001"),
        "All those moments will be lost in time, like tears in rain. Time to die...",
        ByteVector.fromValidHex("30450221008600dbd41e348fe5c9465ab92d23e3db8b98b873beecd930736488696438cb6b0220547fe64427496db33bf66019dacbf0039c04199abb0122918601db38a72cfc21")
      ),
      (
        ByteVector.fromValidHex("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140"),
        "Satoshi Nakamoto",
        ByteVector.fromValidHex("3045022100fd567d121db66e382991534ada77a6bd3106f0a1098c231e47993447cd6af2d002206b39cd0eb1bc8603e159ef5c20a5c8ad685a45b06ce9bebed3f153d10d93bed5")
      ),
      (
        ByteVector.fromValidHex("f8b8af8ce3c7cca5e300d33939540c10d45ce001b8f252bfbc57ba0342904181"),
        "Alan Turing",
        ByteVector.fromValidHex("304402207063ae83e7f62bbb171798131b4a0564b956930092b33b07b395615d9ec7e15c022058dfcc1e00a35e1572f366ffe34ba0fc47db1e7189759b9fb233c5b05ab388ea")
      ),
      (
        ByteVector.fromValidHex("e91671c46231f833a6406ccbea0e3e392c76c167bac1cb013f6f1013980455c2"),
        "There is a computer disease that anybody who works with computers knows about. It's a very serious disease and it interferes completely with the work. The trouble with computers is that you 'play' with them!",
        ByteVector.fromValidHex("3045022100b552edd27580141f3b2a5463048cb7cd3e047b97c9f98076c32dbdf85a68718b0220279fa72dd19bfae05577e06c7c0c1900c371fcd5893f7e1d56a37d30174671f6")
      )
    )

    dataset.foreach {
      case (k, m, s) =>
        val sig: ByteVector = Crypto.compact2der(Crypto.sign(Crypto.sha256(ByteVector.view(m.getBytes("UTF-8"))), PrivateKey(k)))
        assertTrue(sig == s)
    }
  }

  @Test
  def recoverPubKeyFromSig: Unit = {
    val priv = PrivateKey(ByteVector.fromValidHex("0101010101010101010101010101010101010101010101010101010101010101"))
    val message = ByteVector.fromValidHex("0202020202020202020202020202020202020202020202020202020202020202")
    val pub = priv.publicKey
    val sig64 = Crypto.sign(message, priv)
    val (pub1, pub2) = Crypto.recoverPublicKey(sig64, message)

    assertTrue(Crypto.verifySignature(message, sig64, pub1))
    assertTrue(Crypto.verifySignature(message, sig64, pub2))
    assertTrue(pub == pub1 || pub == pub2)
  }

  @Test
  def recoverPubKeyFromSig2: Unit = {
    val priv: PrivateKey = PrivateKey(ByteVector.fromValidHex("a6f0d82981c7d7fd424c97548be1b246161097532e102c0457f46ad587069891"))
    val message: ByteVector = ByteVector.fromValidHex("47326140f20c22e45e2fd0c7ec600b4a0134388083b713a19bb3596629c9e447")
    val pub: PublicKey = PublicKey(ByteVector.fromValidHex("03eec785a16054b40bfe15c287beca7f214f88742501fabbe18251502c0ea0588f"))
    val recid: Int = 1
    assertTrue(priv.publicKey == pub)
    val sig64 = Crypto.sign(message, priv)
    val check = Crypto.recoverPublicKey(sig64, message, recid)
    assertTrue(check == pub)
  }
}