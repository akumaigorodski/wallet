package com.btcontract.wallet.helper

import com.neovisionaries.ws.client.{WebSocketFrame, WebSocketAdapter}
import com.neovisionaries.ws.client.{WebSocket, WebSocketFactory}
import com.btcontract.wallet.Utils.{Bytes, app, none}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class Websocket(url: String) { me =>
  type SockHeader = java.util.List[String]
  type Heads = java.util.Map[String, SockHeader]
  var stack: Iterator[Any] = Iterator.empty
  var socket = Option.empty[WebSocket]
  var reactors = List.empty[WsReactor]

  private val adapter = new WebSocketAdapter {
    override def onConnected(ws: WebSocket, hs: Heads) = for (rc <- reactors) rc onConnect ws
    override def onTextMessage(ws: WebSocket, text: String) = for (rc <- reactors) rc onText text
    override def onBinaryMessage(ws: WebSocket, binary: Bytes) = for (rc <- reactors) rc onBinary binary
    override def onDisconnected(ws: WebSocket, sf: WebSocketFrame, mf: WebSocketFrame, byServ: Boolean) =
      for (rc <- reactors) rc.onDisconnect
  }

  def connect = {
    val factory = new WebSocketFactory setConnectionTimeout 15000
    if (app.orbotOnline) factory.getProxySettings setHost "127.0.0.1" setPort 8118
    val attempt = Future(factory.createSocket(url).addListener(adapter).connect)
    attempt onFailure { case _ => for (rc <- reactors) rc.onDisconnect }
    attempt onSuccess { case sock => socket = Some apply sock }
    attempt onSuccess { case sock => me send sock }
  }

  def add(data: Any) = {
    stack = stack ++ Iterator(data)
    for (sock <- socket if sock.isOpen) send(sock)
  }

  // stack is cleared automatically on each call
  def send(sock: WebSocket) = for (vs <- stack) vs match {
    case binaryMessage: Bytes => sock sendBinary binaryMessage
    case textMessage: String => sock sendText textMessage
    case _ => /* format unknown so do nothing */
  }

  def close = {
    reactors = List.empty
    for (sock <- socket) sock.disconnect
  }
}

class WsReactor {
  def onConnect(ws: WebSocket): Unit = none
  def onBinary(raw: Bytes): Unit = none
  def onText(raw: String): Unit = none
  def onDisconnect: Unit = none
}