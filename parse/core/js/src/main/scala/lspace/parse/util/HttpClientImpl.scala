package lspace.parse.util

import com.softwaremill.sttp.impl.monix.FetchMonixBackend
import scala.scalajs.js

object HttpClientImpl extends HttpClient {
  implicit val backend = FetchMonixBackend()
}
