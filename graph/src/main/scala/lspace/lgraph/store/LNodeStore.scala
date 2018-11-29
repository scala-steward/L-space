package lspace.lgraph.store

import lspace.lgraph.LGraph
import lspace.librarian.structure.Node
import lspace.librarian.structure.Property.default.{`@id`, `@ids`}
import lspace.librarian.structure.store.NodeStore
import lspace.util.CacheStatus

object LNodeStore {
  def apply[G <: LGraph](iri: String, graph: G): LNodeStore[G] = new LNodeStore(iri, graph)
}

class LNodeStore[G <: LGraph](val iri: String, val graph: G) extends LStore[G] with NodeStore[G] {

  override def byId(id: Long): Option[T] = super.byId(id).orElse(graph.storeManager.nodeById(id).headOption)

  def byId(ids: List[Long]): Stream[T] = {
    val byCache          = ids.map(id => id -> byId(id))
    val (noCache, cache) = byCache.partition(_._2.isEmpty)
    (cache.flatMap(_._2) toStream) ++ (if (noCache.nonEmpty) graph.storeManager.nodesById(noCache.map(_._1))
                                       else Stream())
  }

  override def byIri(iri: String): Stream[T] = super.byIri(iri) ++ graph.storeManager.nodeByIri(iri) distinct

  override def store(node: T): Unit = {
    super.store(node)
    graph.storeManager.storeNodes(List(node))
  }

  override def store(nodes: List[T]): Unit = {
    nodes.foreach(super.store)
    graph.storeManager.storeNodes(nodes)
  }

  override def all(): Stream[T] = graph.storeManager.nodes
}
