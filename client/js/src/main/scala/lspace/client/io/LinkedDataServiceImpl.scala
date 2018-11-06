package lspace.client.io

import com.softwaremill.sttp.impl.monix.FetchMonixBackend

object LinkedDataServiceImpl extends LinkedDataService {

  implicit val backend = FetchMonixBackend()
  //  implicit val backend = HttpURLConnectionBackend
}
