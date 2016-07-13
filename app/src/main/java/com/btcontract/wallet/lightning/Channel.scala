package com.btcontract.wallet.lightning


abstract class Channel extends StateMachine[ChannelData]('inactive :: Nil, null) { me =>

  val authHandler: AuthHandler

  def doProcess(change: Any) = (change, data, state) match {
    case (noAnchorParams @ OurChannelParams(delay, None, commitPrivKey,
      finalPrivKey, minDepth, initialFeeRate, shaSeed), null, 'inactive :: Nil) =>

    case (yesAnchorParams @ OurChannelParams(delay, Some(amt), commitPrivKey,
      finalPrivKey, minDepth, initialFeeRate, shaSeed), null, 'inactive :: Nil) =>

  }
}
