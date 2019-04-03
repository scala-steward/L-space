package lspace.lgraph.store

import java.time.Instant

import lspace.Label
import lspace.lgraph.LGraph
import lspace.structure.Node
import lspace.structure.store.NodeStore
import monix.eval.Task
import monix.reactive.Observable

import scala.concurrent.duration._

object LNodeStore {
  def apply[G <: LGraph](iri: String, graph: G): LNodeStore[G] = new LNodeStore(iri, graph)
}

class LNodeStore[G <: LGraph](val iri: String, val graph: G) extends LStore[G] with NodeStore[G] {

  override def hasId(id: Long): Task[Option[T2]] =
    if (isDeleted(id)) Task.now(None)
    else
      cachedById(id) match {
        case Some(e) => Task.now(Some(e))
        case None    => graph.storeManager.nodeById(id).map(_.map(_.asInstanceOf[T2]))
      }

  def hasId(ids: List[Long]): Observable[T2] = {
    val (deleted, tryable) = ids.partition(isDeleted)
    val byCache            = tryable.map(id => id -> cachedById(id))
    val (noCache, cache)   = byCache.partition(_._2.isEmpty)
    Observable.fromIterable(cache.flatMap(_._2)) ++ (if (noCache.nonEmpty)
                                                       graph.storeManager
                                                         .nodesById(noCache.map(_._1))
                                                         .asInstanceOf[Observable[T2]]
                                                         .executeOn(LStore.ec)
                                                     else Observable.empty[T2])
  }

  override def hasIri(iri: String): Observable[T2] = {
    val cachedResult = cachedByIri(iri).filterNot(e => isDeleted(e.id))
    Observable.fromIterable(cachedResult) ++
      graph.storeManager
        .valueByValue(iri, Label.D.`@string`)
        .map(_.id)
        .flatMap(graph.storeManager.edgesByToIdAndKey(_, Label.P.`@id`).asInstanceOf[Observable[graph.GEdge[Any, Any]]])
        .map(_.from)
        .collect { case node: Node => node }
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
        .flatMap(graph.storeManager.edgesByToIdAndKey(_, Label.P.`@id`).asInstanceOf[Observable[graph.GEdge[Any, Any]]])
        .map(_.from)
        .collect { case node: Node => node }
        .asInstanceOf[Observable[T2]]
        .filter(v => !cachedResult.contains(v) && !isDeleted(v.id))
        .executeOn(LStore.ec)
  }

  override def store(node: T): Task[Unit] = {
    for {
      _ <- super.store(node)
      _ <- graph.storeManager
        .storeNodes(List(node))
    } yield ()
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  override def store(nodes: List[T]): Task[Unit] = {
    for {
      _ <- Task.gatherUnordered(nodes.map(super.store))
      _ <- graph.storeManager
        .storeNodes(nodes)
        .executeOn(LStore.ec)
        .forkAndForget
    } yield ()
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  override def delete(node: T): Task[Unit] = Task.defer {
    _deleted += node.id -> Instant.now()
    for {
      _ <- super.delete(node)
      _ <- graph.storeManager
        .deleteNodes(List(node))
        .executeOn(LStore.ec)
    } yield ()
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  override def delete(nodes: List[T]): Task[Unit] = Task.defer {
    val delTime = Instant.now()
    nodes.foreach(node => _deleted += node.id -> delTime)
    for {
      _ <- Task.gatherUnordered(nodes.map(super.delete))
      _ <- graph.storeManager
        .deleteNodes(nodes)
        .executeOn(LStore.ec)
    } yield ()
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  override def all(): Observable[T2] = {
    val cached = _cache.values
    Observable.fromIterable(cached).filter(n => !isDeleted(n.id)) ++ graph.storeManager.nodes
      .filter(!cached.toSet.contains(_))
      .executeOn(LStore.ec)
  }
  def count(): Task[Long] = graph.storeManager.nodeCount()
}
