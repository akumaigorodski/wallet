package com.btcontract.wallet.helper

import com.neovisionaries.ws.client.{WebSocketFrame, WebSocketAdapter}
import com.neovisionaries.ws.client.{WebSocket, WebSocketFactory}
import rx.lang.scala.{Observable => Obs, Subscription}
import com.btcontract.wallet.Utils.app


object Websocket {
  type Header = java.util.List[String]
  type SocketHeaders = java.util.Map[String, Header]
  var currentSocket = Option.empty[WebSocket]

  def factory = {
    val useTor = app.orbotAllowed && app.orbotOnline
    val fac = new WebSocketFactory setConnectionTimeout 15000
    if (useTor) fac.getProxySettings setHost "127.0.0.1" setPort 8118
    fac
  }

  def init(run: => Unit, uri: String) = Obs.create[String] { obs =>
    val sock = factory createSocket uri addListener new WebSocketAdapter {
      override def onConnected(ws: WebSocket, sockHeaders: SocketHeaders) = run
      override def onTextMessage(ws: WebSocket, msg: String) = obs onNext msg
      override def onDisconnected(ws: WebSocket, sf: WebSocketFrame,
        mf: WebSocketFrame, byServ: Boolean) = obs.onCompleted
    }

    // Connect and delegate error handling to subscribers
    try sock.connect catch { case e: Throwable => obs onError e }
    currentSocket = Some apply sock
    Subscription.apply
  }
}
