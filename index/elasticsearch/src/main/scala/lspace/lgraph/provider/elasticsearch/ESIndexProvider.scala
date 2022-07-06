package lspace.lgraph.provider.elasticsearch

import lspace.codec.json.jsonld.{JsonLDDecoder, JsonLDEncoder}
import lspace.lgraph.LGraph
import lspace.lgraph.index.{IndexManager, IndexProvider}

object ESIndexProvider {
  def apply[Json](iri: String, host: String, port: Int)(implicit
    encoder: JsonLDEncoder[Json],
    decoder: JsonLDDecoder[Json]
  ): ESIndexProvider[Json] =
    new ESIndexProvider(iri, host, port)

//  val keySpaceBuilders: concurrent.Map[StoragePoint, KeySpaceBuilder] =
//    new ConcurrentHashMap[StoragePoint, KeySpaceBuilder]().asScala
}

//TODO: support multi-node cluster
class ESIndexProvider[Json](val iri: String, host: String, port: Int)(implicit
  encoder: JsonLDEncoder[Json],
  decoder: JsonLDDecoder[Json]
) extends IndexProvider {

  import org.apache.http.HttpHost
  import org.elasticsearch.client.{RestClient, RestHighLevelClient}
  val client = new RestHighLevelClient(RestClient.builder(new HttpHost("localhost", 9200, "http")))

  import org.elasticsearch.action.index.IndexRequest
  val request = new IndexRequest("posts")
  request.id("1")
  val jsonString =
    "{" +
      "\"user\":\"kimchy\"," +
      "\"postDate\":\"2013-01-30\"," +
      "\"message\":\"trying out Elasticsearch\"" + "}"
  import org.elasticsearch.common.xcontent.XContentType
  request.source(jsonString, XContentType.JSON)
  import org.elasticsearch.client.RequestOptions
  import org.elasticsearch.action.ActionListener
  import org.elasticsearch.action.index.IndexResponse
  def listener[T] = new ActionListener[T]() {
    def onResponse(indexResponse: T): Unit =
      println(s"got response: \n${indexResponse.toString}")

    def onFailure(e: Exception): Unit = {}
  }
  client.indexAsync(request, RequestOptions.DEFAULT, listener[IndexResponse])

  import org.elasticsearch.action.get.GetRequest
  val getRequest = new GetRequest("posts", "1")
  import org.elasticsearch.action.get.GetResponse
  client.getAsync(getRequest, RequestOptions.DEFAULT, listener[GetResponse])

  import org.elasticsearch.action.search.{SearchRequest, SearchResponse}
  val searchRequest = new SearchRequest("posts")
  client.searchAsync(searchRequest, RequestOptions.DEFAULT, listener[SearchResponse])

  // c.close()
//  def ep(prefix: String) =
//    ElasticNodeEndpoint("http", host, port, Some(iri.replace('.', '_').replace('-', '_') + prefix))
//  def properties(prefix: String) = ElasticProperties(Seq(ep(prefix)))
//
//  def client(prefix: String) = ElasticClient(properties(prefix))
//
//  lazy val dataClient = client("data")
//  lazy val nsClient   = client("data")
//
//  def dataManager[G <: LGraph](graph: G): IndexManager[G] = new ESIndexManager(graph, dataClient)
//  def nsManager[G <: LGraph](graph: G): IndexManager[G]   = new ESIndexManager(graph, nsClient)
////  def indexIndexManager[G <: LGraph](graph: G): IndexManager[G] = new ESIndexManager(graph)
}
