package com.btcontract.wallet.lightning


abstract class Channel(state: List[Symbol], data: ChannelData)
extends StateMachine[ChannelData](state, data) { me =>

  val authHandler: AuthHandler
}
