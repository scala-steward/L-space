package lspace.lgraph.store

import java.time.Instant

import lspace.Label
import lspace.lgraph.{LGraph, LResource}
import lspace.structure.store.EdgeStore
import lspace.structure.{Edge, Property}
import monix.eval.Task
import monix.reactive.Observable

import scala.concurrent.duration._

object LEdgeStore {
  def apply[G <: LGraph](iri: String, graph: G): LEdgeStore[G] = new LEdgeStore(iri, graph)
}

class LEdgeStore[G <: LGraph](val iri: String, val graph: G) extends LStore[G] with EdgeStore[G] {
  override def store(edge: T): Task[Unit] = {
//    if ((edge.key == `@id` || edge.key == `@ids`) && edge.to.isInstanceOf[graph._Value[String]])
//      graph.`@idStore`.store(edge.to.asInstanceOf[graph._Value[String]])

    (for {
      _ <- super.store(edge)

      _ <- graph.storeManager
        .storeEdges(List(edge))
    } yield ()).executeOn(LStore.ec).startAndForget
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  override def store(edges: List[T]): Task[Unit] = {
    (for {
      _ <- Task.gatherUnordered(edges.map(super.store))
      _ <- graph.storeManager
        .storeEdges(edges)
    } yield ()).executeOn(LStore.ec).startAndForget
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
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
        graph.nodeStore.cache(node.asInstanceOf[graph._Node])
      case edge: graph._Edge[_, _] =>
        cache(edge.asInstanceOf[graph._Edge[_, _]])
      case value: graph._Value[_] =>
        graph.valueStore.cache(value.asInstanceOf[graph._Value[_]])
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

//    edge.from
//      .asInstanceOf[LResource[Any]]
//      .removeOut(edge.asInstanceOf[Edge[Any, _]])
//    edge.to
//      .asInstanceOf[LResource[Any]]
//      .removeIn(edge.asInstanceOf[Edge[_, Any]])

    super.uncache(edge)
  }

  override def hasIri(iri: String): Observable[T2] = {
    val cachedResult = cachedByIri(iri).filterNot(e => isDeleted(e.id))
    Observable.fromIterable(cachedResult) ++
      graph.storeManager
        .valueByValue(iri, Label.D.`@string`)
        .map(_.id)
        .flatMap(graph.storeManager.edgesByToIdAndKey(_, Label.P.`@id`).asInstanceOf[Observable[T2]])
        .map(_.from)
        .collect { case edge: Edge[_, _] => edge }
        .asInstanceOf[Observable[T2]]
        .filter(v => !cachedResult.toSet.contains(v) && !isDeleted(v.id))
        .executeOn(LStore.ec)
  }

  def hasIri(iri: Set[String]): Observable[T2] = {
    val cachedResult = iri.flatMap(iri => cachedByIri(iri).filterNot(e => isDeleted(e.id)))
    Observable.fromIterable(cachedResult) ++
      graph.storeManager
        .valuesByValue(iri.toList.map(_ -> Label.D.`@string`))
        .map(_.id)
        .flatMap(graph.storeManager.edgesByToIdAndKey(_, Label.P.`@id`).asInstanceOf[Observable[T2]])
        .map(_.from)
        .collect { case edge: Edge[_, _] => edge }
        .asInstanceOf[Observable[T2]]
        .filter(v => !cachedResult.contains(v) && !isDeleted(v.id))
        .executeOn(LStore.ec)
  }

  override def hasId(id: Long): Task[Option[T2]] =
    Task.defer {
      if (isDeleted(id)) Task.now(None)
      else
        cachedById(id) match {
          case Some(e) => Task.now(Some(e))
          case None    => graph.storeManager.edgeById(id).map(_.map(_.asInstanceOf[T2]))
        }
    }

  def hasId(ids: List[Long]): Observable[T2] = {
    val (deleted, tryable) = ids.partition(isDeleted)
    val byCache            = tryable.map(id => id -> cachedById(id))
    val (noCache, cache)   = byCache.partition(_._2.isEmpty)
    Observable.fromIterable(cache.flatMap(_._2)) ++ (if (noCache.nonEmpty)
                                                       graph.storeManager
                                                         .edgesById(noCache.map(_._1))
                                                         .asInstanceOf[Observable[T2]]
                                                         .executeOn(LStore.ec)
                                                     else Observable.empty[T2])
  }

  def byId(fromId: Option[Long] = None, key: Option[Property] = None, toId: Option[Long] = None): Observable[T2] = {
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
//                val keyId = graph.ns.nodes.hasIri(key.iri).head.id
                Observable() //graph.storeManager.edgesByKey(key)
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

  override def delete(edge: T): Task[Unit] = Task.defer {
    _deleted += edge.id -> Instant.now()
    for {
      _ <- super.delete(edge)
      _ <- graph.storeManager
        .deleteEdges(List(edge))
        .executeOn(LStore.ec)
        .startAndForget
    } yield ()
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  override def delete(edges: List[T]): Task[Unit] = Task.defer {
    val delTime = Instant.now()
    edges.foreach(edge => _deleted += edge.id -> delTime)
    for {
      _ <- Task.gatherUnordered(edges.map(super.delete))
      _ <- graph.storeManager
        .deleteEdges(edges)
        .executeOn(LStore.ec)
        .startAndForget
    } yield ()
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  def all(): Observable[T2] = {
    val cached = _cache.values
    Observable.fromIterable(cached).filter(n => !isDeleted(n.id)) ++ graph.storeManager.edges
      .filter(!cached.toSet.contains(_))
  }
  def count(): Task[Long] = graph.storeManager.edgeCount()
}
