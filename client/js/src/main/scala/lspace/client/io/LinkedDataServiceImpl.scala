package lspace.client.io

import com.softwaremill.sttp.impl.monix.FetchMonixBackend
import lspace.librarian.structure.Graph
import lspace.parse.json.JsonLD

object LinkedDataServiceImpl {
  def apply(graph: Graph): LinkedDataServiceImpl = new LinkedDataServiceImpl(graph)
}
class LinkedDataServiceImpl(graph: Graph) extends LinkedDataService {
  val jsonld = JsonLD(graph)

  implicit val backend = FetchMonixBackend()
  //  implicit val backend = HttpURLConnectionBackend
}
