package lspace.provider.mem.store

import java.util.concurrent.ConcurrentHashMap

import lspace.provider.mem.MemGraph
import lspace.structure.store.Store
import monix.eval.Task
import monix.reactive.Observable

import scala.collection._
import scala.collection.JavaConverters._

object MemStore {
//  def apply[T <: Resource[_]](iri: String, graph: MemGraph): MemStore[T] =
//    new MemStore[T](iri, graph)
}
trait MemStore[G <: MemGraph] extends Store[G] {
  val graph: G

  protected[mem] lazy val data: concurrent.Map[Long, T2] =
    new ConcurrentHashMap[Long, T2](16, 0.9f, 32).asScala
//  protected[mem] lazy val data: concurrent.TrieMap[Long, T2] =
//    new concurrent.TrieMap[Long, T2]()

  def store(resource: T): Task[Unit] = Task { cache(resource) }
  def cache(resource: T): Unit = {
    data += resource.id -> resource.asInstanceOf[T2]
  }

  def store(resources: List[T]): Task[Unit] = Task { cache(resources) }
  def cache(resources: List[T]): Unit       = resources.foreach(cache)

  def hasId(id: Long): Task[Option[T2]] = Task { data.get(id) }
  def hasId(ids: List[Long]): Observable[T2] =
    Observable.fromIterable(ids).map(data.get).flatMap(Observable.fromIterable(_))

  def cached = new {
    def all(): Stream[T2]           = data.toStream.map(_._2)
    def hasId(id: Long): Option[T2] = data.get(id)
    def count: Long                 = data.size
  }

  def delete(resource: T): Task[Unit] = Task {
    data -= resource.id
  }
  def delete(resources: List[T]): Task[Unit] =
    for {
      _ <- Task.gather {
        resources.map(delete)
      }
    } yield ()

  def purge: Task[Unit] = Task {
    data.clear()
  }

  def all(): Observable[T2] = Observable.fromIterable(data).map(_._2)
  def count(): Task[Long]   = Task.delay(data.size.toLong)
}
