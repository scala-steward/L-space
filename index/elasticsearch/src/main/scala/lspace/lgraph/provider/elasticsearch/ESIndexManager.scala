package lspace.lgraph.provider.elasticsearch

import com.sksamuel.elastic4s.RefreshPolicy
import com.sksamuel.elastic4s.http.search.SearchResponse
import com.sksamuel.elastic4s.http.Response
import com.sksamuel.elastic4s.http.ElasticClient
import lspace.codec.{NativeTypeDecoder, NativeTypeEncoder}
import lspace.lgraph.LGraph
import lspace.lgraph.index.IndexManager
import lspace.lgraph.provider.file.{DecodeLDFS, EncodeLDFS}
import monix.eval.Task

class ESIndexManager[G <: LGraph, Json](override val graph: G, val client: ElasticClient)(
    implicit baseEncoder: NativeTypeEncoder.Aux[Json],
    baseDecoder: NativeTypeDecoder.Aux[Json])
    extends IndexManager(graph) {

  val encoder: EncodeLDFS[Json] = EncodeLDFS()
  val decoder: DecodeLDFS[Json] = DecodeLDFS(graph)
  import decoder.{baseDecoder => _, Json => _, _}
  import encoder.{baseEncoder => _, Json => _, _}

  // you must import the DSL to use the syntax helpers
  import com.sksamuel.elastic4s.http.ElasticDsl._

  client.execute {
    bulk(
      indexInto("myindex" / "mytype").fields("country" -> "Mongolia", "capital" -> "Ulaanbaatar"),
      indexInto("myindex" / "mytype").fields("country" -> "Namibia", "capital"  -> "Windhoek")
    ).refresh(RefreshPolicy.WaitFor)
  }.await

  val response: Response[SearchResponse] = client.execute {
    search("myindex").matchQuery("capital", "ulaanbaatar")
  }.await

  // prints out the original json
  println(response.result.hits.hits.head.sourceAsString)

  def close(): Task[Unit] = Task.now(client.close())

}
