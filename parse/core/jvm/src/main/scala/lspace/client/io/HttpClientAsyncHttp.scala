package lspace.client.io

import sttp.client.asynchttpclient.WebSocketHandler
import sttp.client.asynchttpclient.monix.AsyncHttpClientMonixBackend

object HttpClientAsyncHttp extends HttpClient {
  type WS[R] = WebSocketHandler[R]
  lazy val backend = AsyncHttpClientMonixBackend()
}
