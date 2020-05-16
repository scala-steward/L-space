package lspace.client.io

import monix.eval.Task
import sttp.client.NothingT
import sttp.client.impl.monix.FetchMonixBackend

object HttpClientAsyncHttp extends HttpClient {
  type WS[R] = NothingT[R]
  val backend = Task { FetchMonixBackend() }
}
