package lspace.client.io

import sttp.client3.asynchttpclient.monix.AsyncHttpClientMonixBackend

object HttpClientAsyncHttp extends HttpClient {
  lazy val backend = AsyncHttpClientMonixBackend()
}
