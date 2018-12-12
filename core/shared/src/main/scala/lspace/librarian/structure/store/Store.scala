package lspace.librarian.structure.store

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import lspace.librarian.datatype._
import lspace.librarian.structure._
import lspace.types.vector.Point

object Store {}
trait Store[G <: Graph] {
  def iri: String
  val graph: G
  type T <: graph._Resource[_]
  type T2 <: graph._Resource[_]

  lazy val id: Long = iri.hashCode()

  def +(resource: T): Unit         = store(resource)
  def ++(resources: List[T]): Unit = store(resources)
  def store(resource: T): Unit
  def store(resources: List[T]): Unit

  def byId(id: Long): Option[T2]
  def byId(ids: List[Long]): Stream[T2]
  def byIri(iri: String): Stream[T2]

  def -(resource: T): Unit = delete(resource)
  def delete(resource: T): Unit
  def delete(resources: List[T]): Unit

  def all(): Stream[T2]
  def count(): Long
}
trait NodeStore[G <: Graph] extends Store[G] {
  type T  = graph.GNode
  type T2 = graph.GNode
}
trait EdgeStore[G <: Graph] extends Store[G] {
  type T  = graph.GEdge[_, _]
  type T2 = graph.GEdge[Any, Any]
}
trait ValueStore[G <: Graph] extends Store[G] {
  type T  = graph.GValue[_]
  type T2 = graph.GValue[Any]

  def byValue[V](value: V, dt: DataType[V]): Stream[graph.GValue[V]]
}
