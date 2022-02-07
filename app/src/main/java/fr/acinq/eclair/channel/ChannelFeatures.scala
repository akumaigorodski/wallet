package fr.acinq.eclair.channel

import fr.acinq.eclair.Features.{ResizeableHostedChannels, StaticRemoteKey}
import fr.acinq.eclair.transactions.Transactions.{CommitmentFormat, DefaultCommitmentFormat}
import fr.acinq.eclair.{Feature, FeatureScope}


case class ChannelFeatures(activated: Set[Feature with FeatureScope] = Set.empty) {
  def hasFeature(feature: Feature with FeatureScope): Boolean = activated.contains(feature)

  lazy val paysDirectlyToWallet: Boolean = hasFeature(StaticRemoteKey)
  lazy val hostedResizeable: Boolean = hasFeature(ResizeableHostedChannels)
  lazy val commitmentFormat: CommitmentFormat = DefaultCommitmentFormat
}

object ChannelFeatures {
  def apply(features: Feature with FeatureScope*): ChannelFeatures = ChannelFeatures(features.toSet)
}
