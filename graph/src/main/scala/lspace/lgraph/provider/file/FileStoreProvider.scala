package lspace.lgraph.provider.file

import lspace.lgraph.store.{StoreManager, StoreProvider}
import lspace.lgraph.{GraphManager, LGraph}

object FileStoreProvider {
  def apply(iri: String, path: String): FileStoreProvider = new FileStoreProvider(
    iri,
    path
  )
}
class FileStoreProvider(val iri: String, path: String) extends StoreProvider {
  import FileStoreProvider._

  override def stateManager[G <: LGraph](graph: G): GraphManager[G] = FileGraphManager(graph, path + "/graph")

  override def dataManager[G <: LGraph](graph: G): StoreManager[G] = FileStoreManager(graph, path + "/data")

  override def nsManager[G <: LGraph](graph: G): StoreManager[G] = FileStoreManager(graph, path + "/ns")

  override def nsIndexManager[G <: LGraph](graph: G): StoreManager[G] = FileStoreManager(graph, path + "/ns/index")

  override def indexManager[G <: LGraph](graph: G): StoreManager[G] = FileStoreManager(graph, path + "/index")

//  override def indexIndexManager[G <: LGraph](graph: G): StoreManager[G] =
//    FileStoreManager(graph, path + "/index/index")
}
