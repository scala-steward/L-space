package lspace.lgraph.provider.mem

import lspace.lgraph.{GraphManager, LGraph}
import lspace.lgraph.store.{StoreManager, StoreProvider}

object MemStoreProvider {
  def apply(iri: String): MemStoreProvider = new MemStoreProvider(iri)
}
class MemStoreProvider(val iri: String) extends StoreProvider {

  override def stateManager[G <: LGraph](graph: G): GraphManager[G] = MemGraphManager(graph)

  override def dataManager[G <: LGraph](graph: G): StoreManager[G] = MemStoreManager(graph)

  override def nsManager[G <: LGraph](graph: G): StoreManager[G] = MemStoreManager(graph)

  override def nsIndexManager[G <: LGraph](graph: G): StoreManager[G] = MemStoreManager(graph)

  override def indexManager[G <: LGraph](graph: G): StoreManager[G] = MemStoreManager(graph)

//  override def indexIndexManager[G <: LGraph](graph: G): StoreManager[G] = MemStoreManager(graph)
}
