package lspace.client.io

import monix.eval.Task
import sttp.client3.impl.monix.FetchMonixBackend

object HttpClientAsyncHttp extends HttpClient {
  val backend = Task(FetchMonixBackend())
}
