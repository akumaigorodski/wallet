package com.btcontract.wallettest.utils


object Addon {
  val MINIMIZED = "state-minimized"
  val ACTIVE = "state-active"
}

sealed trait Addon {
  val view: Option[immortan.crypto.CanBeRepliedTo]
  val addon: AddonData
}

sealed trait AddonData {
  def authToken: Option[String]
  def supportEmail: String
  def description: String
  def domain: String
}

case class UsedAddons(addons: List[AddonData] = Nil) { me =>
  def withAddedAddonData(data: AddonData): UsedAddons =
    copy(addons = addons :+ data)

  def withRemovedAddonData(domain: String): UsedAddons = {
    val updatedAddons = addons.filterNot(_.domain == domain)
    copy(addons = updatedAddons)
  }

  def withUpdatedAddonData(data: AddonData): UsedAddons = {
    val oldAddonIndex = addons.indexWhere(_.domain == data.domain)
    val addons1 = addons.patch(oldAddonIndex, data :: Nil, 1)
    if (oldAddonIndex < 0) me else copy(addons = addons1)
  }
}

case class BasicAddon(authToken: Option[String], supportEmail: String, description: String, domain: String) extends AddonData