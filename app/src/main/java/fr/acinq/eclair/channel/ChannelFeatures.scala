package fr.acinq.eclair.channel

import fr.acinq.eclair.Features.{HostedChannels, ResizeableHostedChannels, StaticRemoteKey, Wumbo}
import fr.acinq.eclair.transactions.Transactions.{CommitmentFormat, DefaultCommitmentFormat}
import fr.acinq.eclair.{Feature, Features}


case class ChannelFeatures(activated: Set[Feature] = Set.empty) {
  def hasFeature(feature: Feature): Boolean = activated.contains(feature)

  lazy val paysDirectlyToWallet: Boolean = hasFeature(StaticRemoteKey)
  lazy val hostedResizeable: Boolean = hasFeature(ResizeableHostedChannels)
  lazy val commitmentFormat: CommitmentFormat = DefaultCommitmentFormat
}

object ChannelFeatures {
  def apply(features: Feature*): ChannelFeatures = ChannelFeatures(features.toSet)
  def pickChannelFeatures(localFeatures: Features, remoteFeatures: Features): ChannelFeatures = {
    def canUseCheck(feature: Feature): Boolean = Features.canUseFeature(localFeatures, remoteFeatures, feature)
    val availableFeatures = Set[Feature](StaticRemoteKey, Wumbo, HostedChannels, ResizeableHostedChannels).filter(canUseCheck)
    ChannelFeatures(availableFeatures)
  }
}
