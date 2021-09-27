package com.btcontract.wallettest

import fr.acinq.bitcoin.{ByteVector32, Crypto}
import androidx.test.ext.junit.runners.AndroidJUnit4
import immortan.LightningNodeKeys
import org.junit.runner.RunWith
import scodec.bits.ByteVector
import org.junit.Test


@RunWith(classOf[AndroidJUnit4])
class LnUrlAuthSpec {

  @Test
  def backwardCompatWithBLWv1: Unit = {
    val refSeed = ByteVector.fromValidHex("1b51da6b34675f0aa4bb0b5f0af193ef75294cf6a28d1b2541304e486b76b2538bd680b66f618faf0e9cd4fd98823b5439fc89c242102387eb1bc5e29a052007")
    val keys = LightningNodeKeys.makeFromSeed(refSeed.toArray)

    val refSite = "www.site.com"
    val linkingKey = keys.makeLinkingKey(refSite)
    val refLinkingKeyFormat = ByteVector.fromValidHex("4116700cd356bcc94f2de10b328a41e2e2932152ec7ba720e6d12b4f54eee134")
    assert(linkingKey.value.bytes == refLinkingKeyFormat)

    val refK1 = ByteVector32.fromValidHex("7f9d3443b2ece9485a7a8d98df6a79fd269ce938880266769bfb4ba511ff8761")
    val refDerSignature = ByteVector.fromValidHex("304402205757f98d52e0d3b72d4be1a46d7d62c0f7d8caf0987707352089d8ec29b8f238022019c7f508c1606234d7688116a14d812bc5e8d699c93f5b7cfa1de17b1ef523e1")
    assert(Crypto.compact2der(Crypto.sign(refK1, linkingKey)) == refDerSignature)
  }
}
