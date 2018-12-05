package lspace.librarian.provider.wrapped

import monix.reactive.subjects.Var
import lspace.librarian.structure._
import lspace.util.CacheStatus

trait WrappedResource[T] extends Resource[T] {
  //  def value: T = self.value

  def id: Long                   = self.id
  def graph: Graph               = self.graph
  override def iri: String       = self.iri
  override def iris: Set[String] = self.iris

  //  @transient override val status: Var[CacheStatus.CacheStatus] = self.status
  //  @transient override val memento: Var[Long] = self.memento

  override def hashCode(): Int = self.value.hashCode()

  def out(key: Property*): List[Any]                             = self.out(key: _*)
  def outMap(key: Property*): Map[Property, List[Any]]           = self.outMap(key: _*)
  def outE(key: Property*): List[Edge[T, Any]]                   = self.outE(key: _*)
  def outEMap(key: Property*): Map[Property, List[Edge[T, Any]]] = self.outEMap(key: _*)
  def in(key: Property*): List[Any]                              = self.in(key: _*)
  def inMap(key: Property*): Map[Property, List[Any]]            = self.inMap(key: _*)
  def inE(key: Property*): List[Edge[Any, T]]                    = self.inE(key: _*)
  def inEMap(key: Property*): Map[Property, List[Edge[Any, T]]]  = self.inEMap(key: _*)

//  def addOuts[V, R <: ClassType[_]](key: Property, values: List[(R, V)]): List[Edge[T, V]] =
//    self.addOuts(key, values)
//  def addIns[V](key: Property, values: List[(ClassType[V], V)]): List[Edge[V, T]] =
//    self.addIns(key, values)

  def removeIn(edge: Edge[_, T]): Unit  = self.removeIn(edge)
  def removeOut(edge: Edge[T, _]): Unit = self.removeOut(edge)
  def removeIn(key: Property): Unit     = self.removeIn(key)
  def removeOut(key: Property): Unit    = self.removeOut(key)

  protected def _remove(): Unit = ??? //should never be hit
  override def remove(): Unit   = self.remove()
}
