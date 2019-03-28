package lspace.provider.wrapped

import monix.reactive.subjects.Var
import lspace.structure._
import lspace.util.CacheStatus
import monix.eval.Task

trait WrappedResource[T] extends Resource[T] {
  //  def value: T = self.value

  def id: Long                   = self.id
  val graph: Graph               = self.graph
  override def iri: String       = self.iri
  override def iris: Set[String] = self.iris

  //  @transient override val status: Var[CacheStatus.CacheStatus] = self.status
  //  @transient override val memento: Var[Long] = self.memento

  override lazy val hashCode: Int = self.value.hashCode()

  override def keys: Set[Property] = self.keys

  def out(key: Property*): List[Any]                             = self.out(key: _*)
  def outMap(key: Property*): Map[Property, List[Any]]           = self.outMap(key: _*)
  def outE(key: Property*): List[Edge[T, Any]]                   = self.outE(key: _*)
  def outEMap(key: Property*): Map[Property, List[Edge[T, Any]]] = self.outEMap(key: _*)
  def in(key: Property*): List[Any]                              = self.in(key: _*)
  def inMap(key: Property*): Map[Property, List[Any]]            = self.inMap(key: _*)
  def inE(key: Property*): List[Edge[Any, T]]                    = self.inE(key: _*)
  def inEMap(key: Property*): Map[Property, List[Edge[Any, T]]]  = self.inEMap(key: _*)

  def removeIn[V >: T](edge: Edge[_, V]): Task[Unit]  = self.removeIn(edge)
  def removeOut[V >: T](edge: Edge[V, _]): Task[Unit] = self.removeOut(edge)
  def removeIn(key: Property): Task[Unit]             = self.removeIn(key)
  def removeOut(key: Property): Task[Unit]            = self.removeOut(key)

  protected def _remove(): Unit     = ??? //should never be hit
  override def remove(): Task[Unit] = self.remove()
}
