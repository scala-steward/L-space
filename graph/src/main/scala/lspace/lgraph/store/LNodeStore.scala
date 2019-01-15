package lspace.lgraph.store

import java.time.Instant

import lspace.lgraph.LGraph
import lspace.librarian.structure.store.NodeStore

object LNodeStore {
  def apply[G <: LGraph](iri: String, graph: G): LNodeStore[G] = new LNodeStore(iri, graph)
}

class LNodeStore[G <: LGraph](val iri: String, val graph: G) extends LStore[G] with NodeStore[G] {

  override def hasId(id: Long): Option[T2] =
    if (isDeleted(id)) None else cachedById(id).orElse(graph.storeManager.nodeById(id).headOption)

  def hasId(ids: List[Long]): Stream[T2] = {
    val (deleted, tryable) = ids.partition(isDeleted)
    val byCache            = tryable.map(id => id -> cachedById(id))
    val (noCache, cache)   = byCache.partition(_._2.isEmpty)
    (cache.flatMap(_._2) toStream) ++ (if (noCache.nonEmpty) graph.storeManager.nodesById(noCache.map(_._1))
                                       else Stream())
  }

  override def hasIri(iri: String): Stream[T2] =
    cachedByIri(iri).filterNot(n => isDeleted(n.id)) ++ graph.storeManager.nodeByIri(iri) distinct

  override def store(node: T): Unit = {
    super.store(node)
    graph.storeManager
      .storeNodes(List(node))
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
      .runToFuture(monix.execution.Scheduler.global)
  }

  override def store(nodes: List[T]): Unit = {
    nodes.foreach(super.store)
    graph.storeManager
      .storeNodes(nodes)
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
      .runToFuture(monix.execution.Scheduler.global)
  }

  override def delete(node: T): Unit = {
    _deleted += node.id -> Instant.now()
    super.delete(node)
    graph.storeManager
      .deleteNodes(List(node))
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
      .runToFuture(monix.execution.Scheduler.global)
  }

  override def delete(nodes: List[T]): Unit = {
    val delTime = Instant.now()
    nodes.foreach(node => _deleted += node.id -> delTime)
    nodes.foreach(super.delete)
    graph.storeManager
      .deleteNodes(nodes)
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
      .runToFuture(monix.execution.Scheduler.global)
  }

  override def all(): Stream[T2] =
    _cache.toStream.map(_._2).filterNot(n => isDeleted(n.id)) ++ graph.storeManager.nodes distinct
  def count(): Long = graph.storeManager.nodeCount()
}
