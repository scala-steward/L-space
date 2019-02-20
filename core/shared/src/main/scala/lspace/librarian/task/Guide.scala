package lspace.librarian.task

import java.time.Instant

import lspace.librarian.logic.Assistent
import lspace.librarian.traversal.{Librarian, Segment, TraversalPath}
import lspace.structure.{Graph, Resource}
import monix.reactive.Observable

trait Guide {
  def assistent: Assistent

  def toValue(v: Any): Any = v match {
    case librarian: Librarian[Any] => toValue(librarian.get)
    case resource: Resource[Any]   => resource.value
    case it: Map[Any, Any]         => it.map(t => toValue(t._1) -> toValue(t._2))
    case it: Iterable[Any]         => it.map(toValue)
    case (v1, v2)                  => (toValue(v1), toValue(v2))
    case (v1, v2, v3)              => (toValue(v1), toValue(v2), toValue(v3))
    case (v1, v2, v3, v4)          => (toValue(v1), toValue(v2), toValue(v3), toValue(v4))
    case value                     => value
  }

  private case class SimpleLibrarian[+T](val get: T,
                                         val path: TraversalPath = TraversalPath(),
                                         val loops: Int = 0,
                                         val mit: Option[Instant] = None,
                                         val permissions: List[String] = List())
      extends Librarian[T] {
    def apply[V](get: V,
                 path: TraversalPath = TraversalPath(),
                 loops: Int = 0,
                 mit: Option[Instant] = None,
                 permissions: List[String] = List()): Librarian[V] =
      new SimpleLibrarian[V](get, path, loops, mit, permissions)
  }

  def createLibrarian[T](get: T,
                         path: TraversalPath = TraversalPath(),
                         loops: Int = 0,
                         mit: Option[Instant] = None,
                         permissions: List[String] = List()): Librarian[T] =
    new SimpleLibrarian[T](get, path, loops, mit, permissions)

  def buildTraveralObservable[Out](segments: List[Segment[_]]): Graph => Observable[Out]
}
