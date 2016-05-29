package com.btcontract.wallet.helper

import com.neovisionaries.ws.client.{WebSocketFrame, WebSocketAdapter}
import com.neovisionaries.ws.client.{WebSocket, WebSocketFactory}
import com.btcontract.wallet.Utils.{Bytes, runAnd, app, none}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}


class Websocket(urlAddress: String) {
  type Heads = java.util.Map[String, SockHeader]
  type SockHeader = java.util.List[String]
  var socket = Option.empty[WebSocket]
  var reactors = List.empty[WsReactor]

  val adapter = new WebSocketAdapter {
    override def onConnected(ws: WebSocket, hs: Heads) = for (rc <- reactors) rc onConnect ws
    override def onTextMessage(ws: WebSocket, text: String) = for (rc <- reactors) rc onText text
    override def onBinaryMessage(ws: WebSocket, binary: Bytes) = for (rc <- reactors) rc onBinary binary
    override def onDisconnected(ws: WebSocket, sf: WebSocketFrame, mf: WebSocketFrame, byServ: Boolean) =
      for (rc <- reactors) rc.onDisconnect
  }

  def connect = {
    val factory = new WebSocketFactory setConnectionTimeout 15000
    if (app.orbotOnline) factory.getProxySettings setHost "127.0.0.1" setPort 8118
    Future(factory.createSocket(urlAddress).addListener(adapter).connect) onComplete {
      case Success(ws) => socket = Some(ws) case _ => for (rc <- reactors) rc.onDisconnect
    }
  }

  def sendOr[T](data: T)(or: => Unit) = socket match {
    case Some(webSocket) if webSocket.isOpen => send(data)
    case _ => or
  }

  def send[T](data: T) = for (sock <- socket) data match {
    case binaryMessage: Bytes => sock sendBinary binaryMessage
    case textMessage: String => sock sendText textMessage
    case _ => /* format unknown so do nothing */
  }

  def close = {
    reactors = List.empty
    socket.foreach(_.disconnect)
  }
}

class WsReactor {
  def onConnect(ws: WebSocket): Unit = none
  def onBinary(raw: Bytes): Unit = none
  def onText(raw: String): Unit = none
  def onDisconnect: Unit = none
}