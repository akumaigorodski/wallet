package com.btcontract.wallet

import java.net.{InetSocketAddress, Socket}
import java.util.concurrent.TimeUnit

import android.content.{BroadcastReceiver, Context, Intent, IntentFilter}
import immortan.ConnectionProvider
import immortan.crypto.Tools._
import okhttp3.OkHttpClient
import org.torproject.jni.TorService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class TorConnectionProvider(context: Context) extends ConnectionProvider {
  override val proxyAddress: Option[InetSocketAddress] = new InetSocketAddress("127.0.0.1", 9050).asSome

  private val proxy = new java.net.Proxy(java.net.Proxy.Type.SOCKS, proxyAddress.get)

  override val okHttpClient: OkHttpClient = (new OkHttpClient.Builder).proxy(proxy).connectTimeout(15, TimeUnit.SECONDS).build

  def doWhenReady(action: => Unit): Unit = {
    val broadcastReceiver = new BroadcastReceiver { self =>
      override def onReceive(context: Context, intent: Intent): Unit = {
        if (intent.getStringExtra(TorService.EXTRA_STATUS) == TorService.STATUS_ON) {
          context.unregisterReceiver(self)
          action
        }
      }
    }

    val torServiceClass = classOf[TorService]
    val serviceIntent = new Intent(context, torServiceClass)
    val intentFilter = new IntentFilter(TorService.ACTION_STATUS)
    context.registerReceiver(broadcastReceiver, intentFilter)
    context.startService(serviceIntent)
  }

  override def getSocket: Socket = new Socket(proxy)
}
