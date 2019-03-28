package lspace.structure.store

import lspace.structure._
import monix.eval.Task
import monix.reactive.Observable

object Store {}
trait Store[G <: Graph] {
  def iri: String
  val graph: G
  type T <: graph._Resource[_]
  type T2 <: graph._Resource[_]

  lazy val id: Long = iri.hashCode()

  def +(resource: T): Task[Unit]         = store(resource)
  def ++(resources: List[T]): Task[Unit] = store(resources)
  def store(resource: T): Task[Unit]
  def store(resources: List[T]): Task[Unit]

  def hasId(id: Long): Task[Option[T2]]
  def hasId(ids: List[Long]): Observable[T2]
  def hasIri(iri: String): Observable[T2]
  def hasIri(iri: Set[String]): Observable[T2]

  def cached: {
    def all(): Stream[T2]
    def hasId(id: Long): Option[T2]
  }

  def -(resource: T): Task[Unit] = delete(resource)
  def delete(resource: T): Task[Unit]
  def delete(resources: List[T]): Task[Unit]

  def all(): Observable[T2]
  def count(): Task[Long]
}
