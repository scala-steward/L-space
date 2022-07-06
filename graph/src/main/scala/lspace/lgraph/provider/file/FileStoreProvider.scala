package lspace.lgraph.provider.file

import lspace.client.io.HttpClient
import lspace.codec.json.{JsonDecoder, JsonEncoder}
import lspace.lgraph.store.{StoreManager, StoreProvider}
import lspace.lgraph.{GraphManager, LGraph}

object FileStoreProvider {
  def apply[T](iri: String, path: String)(implicit
    encoder: JsonEncoder[T],
    decoder: JsonDecoder[T],
    httpClient: HttpClient
  ): FileStoreProvider[T] =
    new FileStoreProvider[T](
      iri,
      path
    )
}
class FileStoreProvider[T](val iri: String, path: String)(implicit
  encoder: JsonEncoder[T],
  decoder: JsonDecoder[T],
  httpClient: HttpClient
) extends StoreProvider {
  import FileStoreProvider._

  override def stateManager[G <: LGraph](graph: G): GraphManager[G] = FileGraphManager(graph, path + "/graph")

  override def dataManager[G <: LGraph](graph: G): StoreManager[G] = FileStoreManager(graph, path + "/data")

  override def nsManager[G <: LGraph](graph: G): StoreManager[G] = FileStoreManager(graph, path + "/ns")

  override def nsIndexManager[G <: LGraph](graph: G): StoreManager[G] = FileStoreManager(graph, path + "/ns/index")

  override def indexManager[G <: LGraph](graph: G): StoreManager[G] = FileStoreManager(graph, path + "/index")

//  override def indexIndexManager[G <: LGraph](graph: G): StoreManager[G] =
//    FileStoreManager(graph, path + "/index/index")
}
