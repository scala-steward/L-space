package lspace.parse.util

import com.softwaremill.sttp.impl.monix.FetchMonixBackend

object HttpClientImpl extends HttpClient {
  implicit val backend = FetchMonixBackend()

  val g = scalajs.js.Dynamic.global
  g.fetch = g.require("node-fetch")
  g.require("abortcontroller-polyfill/dist/polyfill-patch-fetch")
  g.Headers = g.require("fetch-headers")
}
