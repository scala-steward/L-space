package lspace.librarian.provider.mem.store

import java.util.concurrent.ConcurrentHashMap

import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure.store.Store

import scala.collection._
import scala.collection.JavaConverters._

object MemStore {
//  def apply[T <: Resource[_]](iri: String, graph: MemGraph): MemStore[T] =
//    new MemStore[T](iri, graph)
}
trait MemStore[G <: MemGraph] extends Store[G] {
  val graph: G

  protected[mem] lazy val data: concurrent.Map[Long, T2] =
    new ConcurrentHashMap[Long, T2]().asScala

  def store(resource: T): Unit = {
    data += resource.id -> resource.asInstanceOf[T2]
  }
  def store(resources: List[T]): Unit = {
    resources.foreach { resource =>
      data += resource.id -> resource.asInstanceOf[T2]
    }
  }

  def byId(id: Long): Option[T2]        = data.get(id)
  def byId(ids: List[Long]): Stream[T2] = ids.toStream.flatMap(data.get)

  def delete(resource: T): Unit = {
    data -= resource.id
  }
  def delete(resources: List[T]): Unit = {
    resources.foreach(delete)
  }

  def all(): Stream[T2] = data.toStream.map(_._2)
  def count(): Long     = all().size
}
