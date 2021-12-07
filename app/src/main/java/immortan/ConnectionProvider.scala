package immortan

import okhttp3.{OkHttpClient, Request, ResponseBody}
import java.net.{InetSocketAddress, Socket}
import java.util.concurrent.TimeUnit


trait ConnectionProvider {
  val proxyAddress: Option[InetSocketAddress]

  val okHttpClient: OkHttpClient

  def doWhenReady(action: => Unit): Unit

  def get(url: String): ResponseBody = {
    val request = (new Request.Builder).url(url).get
    okHttpClient.newCall(request.build).execute.body
  }

  def getSocket: Socket
}

class ClearnetConnectionProvider extends ConnectionProvider {
  override val okHttpClient: OkHttpClient = (new OkHttpClient.Builder).connectTimeout(15, TimeUnit.SECONDS).build

  override val proxyAddress: Option[InetSocketAddress] = Option.empty

  def doWhenReady(action: => Unit): Unit = action

  override def getSocket: Socket = new Socket
}