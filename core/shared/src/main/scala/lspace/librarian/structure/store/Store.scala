package lspace.librarian.structure.store

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import lspace.librarian.datatype._
import lspace.librarian.structure._
import lspace.types.vector.Point

object Store {}
trait Store[G <: Graph] {
  def iri: String
  val graph: G
  type T <: Resource[_]

  lazy val id: Long = iri.hashCode()

  def +(resource: T): Unit         = store(resource)
  def ++(resources: List[T]): Unit = store(resources)
  def store(resource: T): Unit
  def store(resources: List[T]): Unit

  def byId(id: Long): Option[T]
  def byId(ids: List[Long]): Stream[T]
  def byIri(iri: String): Stream[T]

  def -(resource: T): Unit = delete(resource)
  def delete(resource: T): Unit
  def delete(resources: List[T]): Unit

  def all(): Stream[T]
  def count(): Long
}
trait NodeStore[G <: Graph] extends Store[G] {
  type T = graph._Node
}
trait EdgeStore[G <: Graph] extends Store[G] {
  type T = graph._Edge[_, _]

  def byId(fromId: Option[Long] = None, key: Option[Property] = None, toId: Option[Long] = None): Stream[T]
  def byIri(fromIri: Option[String] = None, key: Option[Property] = None, toIri: Option[String] = None): Stream[T]
}
trait ValueStore[G <: Graph] extends Store[G] {
  type T = graph._Value[_]

  def byValue[V](value: V, dt: DataType[V]): Stream[T]
}
