package lspace.parse.util

import monix.eval.Task
import sttp.client.impl.monix.FetchMonixBackend
import sttp.client.NothingT

object HttpClientImpl extends HttpClient[NothingT] {
  val backend = Task { FetchMonixBackend() }
}
