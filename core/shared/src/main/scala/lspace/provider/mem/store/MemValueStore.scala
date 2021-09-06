package lspace.provider.mem.store

import java.util.concurrent.ConcurrentHashMap

import lspace.datatype._
import lspace.provider.mem.MemGraph
import lspace.structure.Property.default.{`@id`, `@ids`}
import lspace.structure.Value
import lspace.structure.store.ValueStore
import monix.eval.Task
import monix.reactive.Observable

import scala.collection.concurrent
import scala.jdk.CollectionConverters._

object MemValueStore {
  def apply[G <: MemGraph](iri: String, graph: G): MemValueStore[G] = new MemValueStore(iri, graph)
}

class MemValueStore[G <: MemGraph](val iri: String, val graph: G) extends MemStore[G] with ValueStore[G] {

  def hasIri(iri: String): Observable[T2] =
    graph.`@idStore`.byValue(iri, DataType.default.`@string`)
      .map(
        _.in(`@id`, `@ids`)
          .filter(_.isInstanceOf[Value[_]])
          .asInstanceOf[List[T2]]
          .distinct)
      .flatMap(Observable.fromIterable(_))

  def hasIri(iri: Set[String]): Observable[T2] =
    Observable
      .fromTask(
        Observable
          .fromIterable(iri)
          .mergeMap(graph.`@idStore`.byValue(_, DataType.default.`@string`).map(_.in(`@id`, `@ids`)
            .filter(_.isInstanceOf[Value[_]])))
          .flatMap(Observable.fromIterable)
          .toListL
          .map(_.asInstanceOf[List[T2]].distinct))
      .flatMap(Observable.fromIterable(_))

  trait Cache {
    private[this] val cacheLock = new Object
    protected lazy val cache: concurrent.Map[Any, Set[T]] =
      new ConcurrentHashMap[Any, Set[T]]().asScala

    def apply(value: T): Unit = cacheLock.synchronized[Unit] {
      cache += value.value -> (cache.getOrElse(value.value, Set()) + value
        .asInstanceOf[T])
    }
    def all: Observable[T2] = Observable.fromIterable(cache.flatMap(_._2)).asInstanceOf[Observable[T2]]
    def byValue[V](value: V): Observable[graph.GValue[V]] =
      Observable.fromIterable(cache.get(value).to(LazyList).flatMap(_.toList).map(_.asInstanceOf[graph.GValue[V]]))
    def byValue[V](value: V, dt: DataType[V]): Observable[graph.GValue[V]] =
      Observable.fromIterable(
        cache.get(value).to(LazyList).flatMap(_.toList).filter(_.label == dt).map(_.asInstanceOf[graph.GValue[V]]))
    def delete(value: T): Unit = cacheLock.synchronized[Unit] {
      val values = cache.getOrElse(value.value, Set())
      if (values.contains(value)) cache -= value.value
      else cache += value.value -> (values - value.asInstanceOf[graph.GValue[Any]])
    }

    def clear(): Unit = cache.clear()
  }

  object vcache extends Cache {}

  def byValue[V](value: V, dt: DataType[V]): Observable[graph.GValue[V]] =
    vcache.byValue(value, dt)

//  override def store(value: T): Task[Unit] = for { _ <- super.store(value) } yield { cache(value) }
  override def cache(value: T): Unit = {
    super.cache(value)
    vcache(value)
  }

  override def store(resources: List[T]): Task[Unit] = {
    Observable.fromIterable(resources).mapEval(store).completedL
  }

  override def delete(value: T): Task[Unit] =
    for { _ <- super.delete(value) } yield {
      vcache.delete(value)
    }

  override def purge: Task[Unit] =
    for {
      _ <- super.purge
      _ <- Task {
        vcache.clear()
      }
    } yield ()
}
