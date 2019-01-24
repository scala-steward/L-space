package lspace.client.io

import com.softwaremill.sttp.okhttp.monix.OkHttpMonixBackend
import lspace.librarian.structure.Graph
import lspace.parse.JsonLD

object LinkedDataServiceImpl {
  def apply(graph: Graph): LinkedDataServiceImpl = new LinkedDataServiceImpl(graph)
}
class LinkedDataServiceImpl(graph: Graph) extends LinkedDataService {
  val jsonld = JsonLD(graph)

  implicit val backend = OkHttpMonixBackend()
  //  implicit val backend = TryHttpURLConnectionBackend()
}
