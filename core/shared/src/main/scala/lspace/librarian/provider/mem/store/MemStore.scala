package lspace.librarian.provider.mem.store

import lspace.librarian.provider.mem.{MemEdge, MemGraph, MemResource}
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default._
import lspace.librarian.structure.store.Store

import scala.collection.mutable

object MemStore {
//  def apply[T <: Resource[_]](iri: String, graph: MemGraph): MemStore[T] =
//    new MemStore[T](iri, graph)
}
trait MemStore[G <: MemGraph] extends Store[G] {
  val graph: G

  protected lazy val data: mutable.OpenHashMap[Long, T] =
    mutable.OpenHashMap[Long, T]()

  def store(resource: T): Unit = data += resource.id -> resource
  def store(resources: List[T]): Unit = resources.foreach { resource =>
    data += resource.id -> resource
  }

  def byId(id: Long): Option[T]        = data.get(id)
  def byId(ids: List[Long]): Stream[T] = ids.toStream.flatMap(data.get)

  def delete(resource: T): Unit        = data -= resource.id
  def delete(resources: List[T]): Unit = resources.foreach(delete)

  def all(): Stream[T] = data.toStream.map(_._2)
  def count(): Long    = all().size
}
