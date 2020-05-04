package lspace.parse.util

import sttp.client.asynchttpclient.WebSocketHandler
import sttp.client.asynchttpclient.monix.AsyncHttpClientMonixBackend

object HttpClientImpl extends HttpClient[WebSocketHandler] {
  lazy val backend = AsyncHttpClientMonixBackend()
}
