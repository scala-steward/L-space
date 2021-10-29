package lspace.lgraph.store

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.concurrent.ConcurrentHashMap

import lspace.Label
import lspace.lgraph.LGraph
import lspace.datatype._
import lspace.structure.{ClassType, Value}
import lspace.structure.util.ClassTypeable
import lspace.structure.store.ValueStore
import lspace.types.geo.Point
import monix.eval.Task
import monix.reactive.Observable

import scala.collection.immutable.ListSet
import scala.collection.concurrent
import scala.jdk.CollectionConverters._
import scala.concurrent.duration._

object LValueStore {
  def apply[G <: LGraph](iri: String, graph: G): LValueStore[G] = new LValueStore(iri, graph)
}

class LValueStore[G <: LGraph](val iri: String, val graph: G) extends LStore[G] with ValueStore[G] {

  trait Cache {
    private[this] val cacheLock = new Object

    protected lazy val cache: concurrent.Map[Any, Set[T]] =
      new ConcurrentHashMap[Any, Set[T]]().asScala

    def apply(value: T): Unit = cacheLock.synchronized {
      cache += value.value -> (cache.getOrElse(value.value, Set()) + value
        .asInstanceOf[T])
    }
    def all: Observable[T2] = Observable.fromIterable(cache.flatMap(_._2)).asInstanceOf[Observable[T2]]
    def byValue[V](value: V): Observable[graph.GValue[V]] =
      Observable.fromIterable(cache.get(value).to(LazyList).flatMap(_.toList).map(_.asInstanceOf[graph.GValue[V]]))
    def byValue[V](value: V, dt: DataType[V]): Observable[graph.GValue[V]] =
      Observable.fromIterable(
        cache.get(value).to(LazyList).flatMap(_.toList).filter(_.label == dt).map(_.asInstanceOf[graph.GValue[V]])
      )
    def delete(value: T): Unit = cacheLock.synchronized {
      val values = cache.getOrElse(value.value, Set())
      if (values.exists(_ == value)) cache -= value.value
      else cache += value.value -> (values - value.asInstanceOf[graph.GValue[Any]])
    }

    def clear(): Unit = cache.clear()
  }

  object vcache extends Cache {}

  override def hasId(id: Long): Task[Option[T2]] =
    Task.defer {
      if (isDeleted(id)) Task.now(None)
      else
        cachedById(id) match {
          case Some(e) => Task.now(Some(e))
          case None    => graph.storeManager.valueById(id).map(_.map(_.asInstanceOf[T2]))
        }
    }

  def hasId(ids: List[Long]): Observable[T2] = {
    val (deleted, tryable) = ids.partition(isDeleted)
    val byCache            = tryable.map(id => id -> cachedById(id))
    val (noCache, cache)   = byCache.partition(_._2.isEmpty)
    Observable.fromIterable(cache.flatMap(_._2)) ++ (if (noCache.nonEmpty)
                                                       graph.storeManager
                                                         .valuesById(noCache.map(_._1))
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
        .collect { case value: Value[_] => value }
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
        .collect { case value: Value[_] => value }
        .asInstanceOf[Observable[T2]]
        .filter(v => !cachedResult.contains(v) && !isDeleted(v.id))
        .executeOn(LStore.ec)
  }

  def byValue[V, VOut, CVOut <: DataType[VOut]](value: V)(implicit
    clsTpbl: ClassTypeable.Aux[V, VOut, CVOut]
  ): Observable[graph._Value[V]] =
    byValue(value, clsTpbl.ct.asInstanceOf[DataType[V]])
  def byValue[V](value: V, dt: DataType[V]): Observable[graph._Value[V]] =
    vcache.byValue(value, dt).filter(v => !isDeleted(v.id))

  override def store(value: T): Task[Unit] =
    for {
      _ <- super.store(value)
      _ <- graph.storeManager
        .storeValues(List(value))
        .executeOn(LStore.ec)
        .startAndForget
    } yield ()
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)

  override def store(values: List[T]): Task[Unit] =
    for {
      _ <- Task.parSequenceUnordered(values.map(super.store))
      _ <- graph.storeManager
        .storeValues(values)
        .executeOn(LStore.ec)
        .startAndForget
    } yield ()
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)

  override def cache(value: T): Unit = {
    super.cache(value)
    cacheByValue(value)
  }

  def cacheByValue(value: T): Unit =
    vcache(value)

  override def delete(value: T): Task[Unit] = Task.defer {
    _deleted += value.id -> Instant.now()
    for {
      _ <- super.delete(value)
      _ <- graph.storeManager
        .deleteValues(List(value))
        .executeOn(LStore.ec)
        .startAndForget
    } yield ()
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  override def uncache(value: T): Unit = {
    super.uncache(value)
    uncacheByValue(value)
  }

  def uncacheByValue(value: T): Unit =
    vcache.delete(value)

  override def delete(values: List[T]): Task[Unit] = Task.defer {
    val delTime = Instant.now()
    values.foreach(value => _deleted += value.id -> delTime)
    for {
      _ <- Task.parSequenceUnordered(values.map(super.delete))
      _ <- graph.storeManager
        .deleteValues(values)
        .executeOn(LStore.ec)
        .startAndForget
    } yield ()
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  override def purge: Task[Unit] =
    for {
      _ <- super.purge
      _ <- Task {
        vcache.clear()
      }
    } yield ()

  def all(): Observable[T2] = {
    val cached = _cache.values
    Observable.fromIterable(cached).filter(n => !isDeleted(n.id)) ++ graph.storeManager.values
      .filter(!cached.toSet.contains(_))
      .executeOn(LStore.ec)
  }
  def count(): Task[Long] = graph.storeManager.valueCount()
}
