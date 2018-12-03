package lspace.lgraph.provider.elasticsearch

import com.sksamuel.elastic4s.RefreshPolicy
import com.sksamuel.elastic4s.http.search.SearchResponse
import com.sksamuel.elastic4s.http.Response
import com.sksamuel.elastic4s.http.{ElasticClient, ElasticProperties}
import lspace.lgraph.LGraph
import lspace.lgraph.index.IndexManager

class ESIndexManager[G <: LGraph](graph: G) extends IndexManager(graph) {

  import com.sksamuel.elastic4s.http.ElasticDsl._

  val client = ElasticClient(ElasticProperties("http://localhost:9200"))

  def close(): Unit = client.close()

}
