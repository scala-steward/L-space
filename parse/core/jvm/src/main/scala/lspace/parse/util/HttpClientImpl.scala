package lspace.parse.util

import sttp.client.okhttp.monix.OkHttpMonixBackend
import sttp.client.okhttp.WebSocketHandler

object HttpClientImpl extends HttpClient[WebSocketHandler] {
  val backend = OkHttpMonixBackend()
}
