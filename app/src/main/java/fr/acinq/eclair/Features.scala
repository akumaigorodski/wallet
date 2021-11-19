package fr.acinq.eclair

import fr.acinq.eclair.FeatureSupport.{Mandatory, Optional}
import scodec.bits.{BitVector, ByteVector}


sealed trait FeatureSupport

object FeatureSupport {
  case object Mandatory extends FeatureSupport {
    override def toString: String = "mandatory"
  }

  case object Optional extends FeatureSupport {
    override def toString: String = "optional"
  }
}

trait Feature {
  def rfcName: String

  def mandatory: Int

  def optional: Int = mandatory + 1

  def supportBit(support: FeatureSupport): Int = support match {
    case Mandatory => mandatory case Optional => optional
  }
}

case class UnknownFeature(bitIndex: Int)

case class Features(activated: Map[Feature, FeatureSupport], unknown: Set[UnknownFeature] = Set.empty) {

  def hasFeature(feature: Feature, support: Option[FeatureSupport] = None): Boolean =
    support match {
      case Some(sup) => activated.get(feature).contains(sup)
      case None => activated.contains(feature)
    }

  def areSupported(remoteFeatures: Features): Boolean = {
    val knownFeaturesOk = remoteFeatures.activated.forall {
      case (feature, Mandatory) => hasFeature(feature)
      case (_, Optional) => true
    }

    val unknownFeaturesOk = remoteFeatures.unknown.forall(1 == _.bitIndex % 2)
    unknownFeaturesOk && knownFeaturesOk
  }

  def toByteVector: ByteVector = {
    val unknownIndexes = for (feature <- unknown) yield feature.bitIndex
    val activatedIndexes = activated.map { case (feature, sup) => feature supportBit sup }

    val activatedBytes = toByteVectorFromIndex(activatedIndexes.toSet)
    val unknownBytes = toByteVectorFromIndex(unknownIndexes)
    val max = activatedBytes.size.max(unknownBytes.size)

    activatedBytes.padLeft(max) | unknownBytes.padLeft(max)
  }

  private def toByteVectorFromIndex(indexes: Set[Int] = Set.empty): ByteVector = {
    if (indexes.isEmpty) return ByteVector.empty

    var buf = BitVector.fill(indexes.max + 1)(high = false).bytes.bits
    indexes.foreach { index => buf = buf set index }
    buf.reverse.bytes
  }
}

object Features {
  val empty: Features = {
    val noFeatures = Map.empty[Feature, FeatureSupport]
    Features(noFeatures)
  }

  def apply(features: (Feature, FeatureSupport)*): Features = Features(features.toMap)

  def apply(bytes: ByteVector): Features = apply(bytes.bits)

  def apply(bits: BitVector): Features = {
    val all = bits.toIndexedSeq.reverse.zipWithIndex.collect {
      case (true, idx) if knownFeatures.exists(_.optional == idx) => Right((knownFeatures.find(_.optional == idx).get, Optional))
      case (true, idx) if knownFeatures.exists(_.mandatory == idx) => Right((knownFeatures.find(_.mandatory == idx).get, Mandatory))
      case (true, idx) => Left(UnknownFeature(idx))
    }

    Features(
      activated = all.collect { case Right((feature, support)) => feature -> support }.toMap,
      unknown = all.collect { case Left(inf) => inf }.toSet
    )
  }

  case object OptionDataLossProtect extends Feature {
    val rfcName = "Data loss protect"
    val mandatory = 0
  }

  case object InitialRoutingSync extends Feature {
    val rfcName = "Initial routing sync"
    val mandatory = 2
  }

  case object ChannelRangeQueries extends Feature {
    val rfcName = "Basic gossip queries"
    val mandatory = 6
  }

  case object VariableLengthOnion extends Feature {
    val rfcName = "Advanced onion"
    val mandatory = 8
  }

  case object ChannelRangeQueriesExtended extends Feature {
    val rfcName = "Fast graph sync"
    val mandatory = 10
  }

  case object StaticRemoteKey extends Feature {
    val rfcName = "Direct balance refund"
    val mandatory = 12
  }

  case object PaymentSecret extends Feature {
    val rfcName = "Payment secret"
    val mandatory = 14
  }

  case object BasicMultiPartPayment extends Feature {
    val rfcName = "Multipart payments"
    val mandatory = 16
  }

  case object Wumbo extends Feature {
    val rfcName = "Large channels"
    val mandatory = 18
  }

  case object TrampolinePayment extends Feature {
    val rfcName = "Trampoline payments"
    val mandatory = 50
  }

  case object ChainSwap extends Feature {
    val rfcName = "Chain swaps"
    val mandatory = 32770
  }

  case object HostedChannels extends Feature {
    val rfcName = "Hosted channels"
    val mandatory = 32972
  }

  case object ResizeableHostedChannels extends Feature {
    val rfcName = "Resizeable Hosted channels"
    val mandatory = 32974
  }

  case object PrivateRouting extends Feature {
    val rfcName = "Private routing"
    val mandatory = 33174
  }

  case object ShutdownAnySegwit extends Feature {
    val rfcName = "Any shutdown script"
    val mandatory = 26
  }

  val knownFeatures: Set[Feature] =
    Set(ChannelRangeQueriesExtended, OptionDataLossProtect, BasicMultiPartPayment,
      ChannelRangeQueries, VariableLengthOnion, InitialRoutingSync, PrivateRouting,
      ShutdownAnySegwit, TrampolinePayment, StaticRemoteKey, HostedChannels,
      ResizeableHostedChannels, PaymentSecret, ChainSwap, Wumbo)

  // Returns true if both feature sets are compatible
  def areCompatible(ours: Features, theirs: Features): Boolean =
    ours.areSupported(theirs) && theirs.areSupported(ours)

  // returns true if both have at least optional support
  def canUseFeature(localFeatures: Features, remoteFeatures: Features, feature: Feature): Boolean =
    localFeatures.hasFeature(feature) && remoteFeatures.hasFeature(feature)
}
