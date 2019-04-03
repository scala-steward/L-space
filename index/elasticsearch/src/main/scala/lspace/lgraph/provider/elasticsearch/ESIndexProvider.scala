package lspace.lgraph.provider.elasticsearch

import com.sksamuel.elastic4s.ElasticsearchClientUri
import com.sksamuel.elastic4s.http.{ElasticClient, ElasticNodeEndpoint, ElasticProperties, HttpClient}
import lspace.codec.{NativeTypeDecoder, NativeTypeEncoder}
import lspace.lgraph.LGraph
import lspace.lgraph.index.{IndexManager, IndexProvider}

object ESIndexProvider {
  def apply[Json](iri: String, host: String, port: Int)(
      implicit baseEncoder: NativeTypeEncoder.Aux[Json],
      baseDecoder: NativeTypeDecoder.Aux[Json]): ESIndexProvider[Json] =
    new ESIndexProvider(iri, host, port)

//  val keySpaceBuilders: concurrent.Map[StoragePoint, KeySpaceBuilder] =
//    new ConcurrentHashMap[StoragePoint, KeySpaceBuilder]().asScala
}

//TODO: support multi-node cluster
class ESIndexProvider[Json](val iri: String, host: String, port: Int)(implicit baseEncoder: NativeTypeEncoder.Aux[Json],
                                                                      baseDecoder: NativeTypeDecoder.Aux[Json])
    extends IndexProvider {

  def ep(prefix: String) =
    ElasticNodeEndpoint("http", host, port, Some(iri.replace('.', '_').replace('-', '_') + prefix))
  def properties(prefix: String) = ElasticProperties(Seq(ep(prefix)))

  def client(prefix: String) = ElasticClient(properties(prefix))

  lazy val dataClient = client("data")
  lazy val nsClient   = client("data")

  def dataManager[G <: LGraph](graph: G): IndexManager[G] = new ESIndexManager(graph, dataClient)
  def nsManager[G <: LGraph](graph: G): IndexManager[G]   = new ESIndexManager(graph, nsClient)
//  def indexIndexManager[G <: LGraph](graph: G): IndexManager[G] = new ESIndexManager(graph)
}
