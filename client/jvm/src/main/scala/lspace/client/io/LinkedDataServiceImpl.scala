package lspace.client.io

import com.softwaremill.sttp.okhttp.monix.OkHttpMonixBackend

object LinkedDataServiceImpl extends LinkedDataService {

  implicit val backend = OkHttpMonixBackend()
  //  implicit val backend = TryHttpURLConnectionBackend()
}
