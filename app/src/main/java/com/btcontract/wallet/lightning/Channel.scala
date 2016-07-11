package com.btcontract.wallet.lightning


abstract class Channel(params: OurChannelParams)
extends StateMachine[ChannelData]('inactive :: Nil, null) { me =>

  val authHandler: AuthHandler
}
