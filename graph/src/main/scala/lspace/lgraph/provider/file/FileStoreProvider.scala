package lspace.lgraph.provider.file

import lspace.lgraph.store.{StoreManager, StoreProvider}
import lspace.lgraph.{GraphManager, LGraph}

class FileStoreProvider(val iri: String, path: String) extends StoreProvider {

  override def stateManager[G <: LGraph](graph: G): GraphManager[G] = FileGraphManager(graph)

  override def dataManager[G <: LGraph](graph: G): StoreManager[G] = FileStoreManager(graph, path)

  override def nsManager[G <: LGraph](graph: G): StoreManager[G] = FileStoreManager(graph, path)

  override def nsIndexManager[G <: LGraph](graph: G): StoreManager[G] = FileStoreManager(graph, path)

  override def indexManager[G <: LGraph](graph: G): StoreManager[G] = FileStoreManager(graph, path)

  override def indexIndexManager[G <: LGraph](graph: G): StoreManager[G] = FileStoreManager(graph, path)
}
