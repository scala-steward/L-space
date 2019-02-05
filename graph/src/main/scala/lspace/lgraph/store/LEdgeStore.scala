package lspace.lgraph.store

import java.time.Instant

import lspace.lgraph.{LGraph, LResource}
import lspace.librarian.structure.store.EdgeStore
import lspace.librarian.structure.{Edge, Property}

import scala.concurrent.duration._

object LEdgeStore {
  def apply[G <: LGraph](iri: String, graph: G): LEdgeStore[G] = new LEdgeStore(iri, graph)
}

class LEdgeStore[G <: LGraph](val iri: String, val graph: G) extends LStore[G] with EdgeStore[G] {
  override def store(edge: T): Unit = {
//    if ((edge.key == `@id` || edge.key == `@ids`) && edge.to.isInstanceOf[graph._Value[String]])
//      graph.`@idStore`.store(edge.to.asInstanceOf[graph._Value[String]])

    super.store(edge)

    graph.storeManager
      .storeEdges(List(edge))
      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  override def store(edges: List[T]): Unit = {
    edges.foreach(super.store)
    graph.storeManager
      .storeEdges(edges)
      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  override def cache(edge: T): Unit = {

    edge.from
      .asInstanceOf[LResource[Any]]
      ._addOut(edge.asInstanceOf[Edge[Any, _]])
    edge.to
      .asInstanceOf[LResource[Any]]
      ._addIn(edge.asInstanceOf[Edge[_, Any]])

    super.cache(edge)

    if (edge.key == Property.default.`@id` || edge.key == Property.default.`@ids`) edge.from match {
      case node: graph._Node =>
        graph.nodeStore.cache(node.asInstanceOf[graph.GNode])
      case edge: graph._Edge[_, _] =>
        cache(edge.asInstanceOf[graph.GEdge[_, _]])
      case value: graph._Value[_] =>
        graph.valueStore.cache(value.asInstanceOf[graph.GValue[_]])
    }
  }

  override def uncache(edge: T): Unit = {

//    if (edge.key == Property.default.`@id` || edge.key == Property.default.`@ids`) edge.from match {
//      case node: graph.nodeStore.T =>
//        graph.nodeStore.uncacheByIri(node)
//      case edge: T =>
//        uncacheByIri(edge)
//      case value: graph.valueStore.T =>
//        graph.valueStore.uncacheByIri(value)
//    }

    edge.from
      .asInstanceOf[LResource[Any]]
      .removeOut(edge.asInstanceOf[Edge[Any, _]])
    edge.to
      .asInstanceOf[LResource[Any]]
      .removeIn(edge.asInstanceOf[Edge[_, Any]])

    super.uncache(edge)
  }

  override def hasIri(iri: String): Stream[T2] =
    cachedByIri(iri).filterNot(e => isDeleted(e.id)) ++ graph.storeManager
      .edgeByIri(iri)
      .asInstanceOf[Stream[T2]] distinct

  override def hasId(id: Long): Option[T2] =
    cachedById(id).orElse(graph.storeManager.edgeById(id).map(_.asInstanceOf[T2]))

  def hasId(ids: List[Long]): Stream[T2] = {
    val (deleted, tryable) = ids.partition(isDeleted)
    val byCache            = tryable.map(id => id -> cachedById(id))
    val (noCache, cache)   = byCache.partition(_._2.isEmpty)
    (cache.flatMap(_._2) toStream) ++ (if (noCache.nonEmpty)
                                         graph.storeManager.edgesById(noCache.map(_._1)).asInstanceOf[Stream[T2]]
                                       else Stream())
  }

  def byId(fromId: Option[Long] = None, key: Option[Property] = None, toId: Option[Long] = None): Stream[T2] = {
    fromId match {
      case None =>
        key match {
          case None =>
            toId match {
              case None       => all()
              case Some(toId) => graph.storeManager.edgesByToId(toId)
            }
          case Some(key) =>
            toId match {
              case None =>
                val keyId = graph.ns.nodes.hasIri(key.iri).head.id
                Stream() //graph.storeManager.edgesByKey(key)
              case Some(toId) => graph.storeManager.edgesByToIdAndKey(toId, key)
            }
        }
      case Some(fromId) =>
        key match {
          case None =>
            toId match {
              case None       => graph.storeManager.edgesByFromId(fromId)
              case Some(toId) => graph.storeManager.edgesByFromIdAndToId(fromId, toId)
            }
          case Some(key) =>
            toId match {
              case None       => graph.storeManager.edgesByFromIdAndKey(fromId, key)
              case Some(toId) => graph.storeManager.edgesByFromIdAndKeyAndToId(fromId, key, toId)
            }
        }
    }
  }

  def byIri(fromIri: Option[String] = None, key: Option[Property] = None, toIri: Option[String] = None): Stream[T] = ???

  override def delete(edge: T): Unit = {
    _deleted += edge.id -> Instant.now()
    super.delete(edge)
    graph.storeManager
      .deleteEdges(List(edge))
      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  override def delete(edges: List[T]): Unit = {
    val delTime = Instant.now()
    edges.foreach(edge => _deleted += edge.id -> delTime)
    edges.foreach(super.delete)
    graph.storeManager
      .deleteEdges(edges)
      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  def all(): Stream[T2] = _cache.toStream.map(_._2).filterNot(v => isDeleted(v.id)) ++ graph.storeManager.edges distinct
  def count(): Long     = graph.storeManager.edgeCount()
}
