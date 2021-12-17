package com.btcontract.wallet

import java.net.{InetSocketAddress, Socket}
import java.util.concurrent.TimeUnit

import android.content.{BroadcastReceiver, Context, Intent, IntentFilter}
import immortan.ConnectionProvider
import immortan.crypto.Tools._
import okhttp3.OkHttpClient
import org.torproject.jni.TorService


class TorConnectionProvider(context: Context) extends ConnectionProvider {
  override val proxyAddress: Option[InetSocketAddress] = new InetSocketAddress("127.0.0.1", 9050).asSome

  private val proxy = new java.net.Proxy(java.net.Proxy.Type.SOCKS, proxyAddress.get)

  override val okHttpClient: OkHttpClient = (new OkHttpClient.Builder).proxy(proxy).connectTimeout(20, TimeUnit.SECONDS).build

  private var shouldReconnect: Boolean = false

  def doWhenReady(action: => Unit): Unit = {
    lazy val once: BroadcastReceiver = new BroadcastReceiver {
      override def onReceive(context: Context, intent: Intent): Unit =
        if (intent.getStringExtra(TorService.EXTRA_STATUS) == TorService.STATUS_ON) {
          try context.unregisterReceiver(once) catch none
          action
        }
    }

    lazy val track: BroadcastReceiver = new BroadcastReceiver {
      override def onReceive(context: Context, intent: Intent): Unit =
        if (intent.getStringExtra(TorService.EXTRA_STATUS) == TorService.STATUS_OFF) {
          try context.unregisterReceiver(track) catch none
          try context.unregisterReceiver(once) catch none
          shouldReconnect = true
        }
    }

    val torServiceClassReference = classOf[TorService]
    val serviceIntent = new Intent(context, torServiceClassReference)
    val intentFilter = new IntentFilter(TorService.ACTION_STATUS)
    context.registerReceiver(track, intentFilter)
    context.registerReceiver(once, intentFilter)
    context.startService(serviceIntent)
  }

  override def getSocket: Socket = new Socket(proxy)

  override def notifyAppAvailable: Unit = {
    if (shouldReconnect) doWhenReady(none)
    shouldReconnect = false
  }
}
