package lspace.parse.util

import com.softwaremill.sttp.okhttp.monix.OkHttpMonixBackend

object HttpClientImpl extends HttpClient {
  implicit val backend = OkHttpMonixBackend()
}
