package lspace.parse.util

import com.softwaremill.sttp.impl.monix.FetchMonixBackend

object HttpClientImpl extends HttpClient {
  implicit val backend = FetchMonixBackend()
}
