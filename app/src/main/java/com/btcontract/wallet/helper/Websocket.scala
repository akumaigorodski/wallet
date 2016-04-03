package com.btcontract.wallet.helper

import com.neovisionaries.ws.client.{WebSocketFrame, WebSocketAdapter}
import com.neovisionaries.ws.client.{WebSocket, WebSocketFactory}
import rx.lang.scala.{Observable => Obs, Subscription}
import com.btcontract.wallet.lightning.Tools.Bytes


object Websocket {
  type Header = java.util.List[String]
  type SocketHeaders = java.util.Map[String, Header]
  val factory = new WebSocketFactory setConnectionTimeout 15000
  var currentSocket = Option.empty[WebSocket]

  def init(run: => Unit, uri: String) = Obs.create[Bytes] { obs =>
    val sock = factory createSocket uri addListener new WebSocketAdapter {
      override def onConnected(ws: WebSocket, sockHeaders: SocketHeaders) = run
      override def onBinaryMessage(ws: WebSocket, msg: Bytes) = obs onNext msg
      override def onDisconnected(ws: WebSocket, sf: WebSocketFrame,
        mf: WebSocketFrame, byServ: Boolean) = obs.onCompleted
    }

    // Connect and delegate error handling to subscribers
    try sock.connect catch { case e: Throwable => obs onError e }
    currentSocket = Some apply sock
    Subscription.apply
  }
}
