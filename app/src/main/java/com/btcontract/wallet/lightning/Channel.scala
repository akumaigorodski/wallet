package com.btcontract.wallet.lightning

import Tools._
import org.bitcoinj.core._
import com.softwaremill.quicklens._
import com.btcontract.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import crypto.ShaChain


abstract class Channel(state: List[Symbol], data: ChannelData)
extends StateMachine[ChannelData](state, data) { me =>

  val authHandler: AuthHandler

  def doProcess(change: Any) = (data, change, state) match {
    case (paramsWithAnchor: OurChannelParams, null, 'Inactive :: _) if paramsWithAnchor.anchorAmount.isDefined =>
      authHandler process paramsWithAnchor.toOpenProto(proto.open_channel.anchor_offer.WILL_CREATE_ANCHOR)
      become(paramsWithAnchor, 'OpenWaitOpenWithAnchor)

    case (paramsNoAnchor: OurChannelParams, null, 'Inactive :: _) if paramsNoAnchor.anchorAmount.isEmpty =>
      authHandler process paramsNoAnchor.toOpenProto(proto.open_channel.anchor_offer.WONT_CREATE_ANCHOR)
      become(paramsNoAnchor, 'OpenWaitOpenNoAnchor)
  }
}
