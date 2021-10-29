package lspace.librarian.task

import java.time.Instant

import lspace.librarian.traversal.{Librarian, TraversalPath}

case class SimpleLibrarian[+T](get: T)(
  val path: TraversalPath = TraversalPath(),
  val loops: Int = 0,
  val mit: Option[Instant] = None,
  val permissions: List[String] = List()
) extends Librarian[T] {
  def apply[V](
    get: V,
    path: TraversalPath = TraversalPath(),
    loops: Int = 0,
    mit: Option[Instant] = None,
    permissions: List[String] = List()
  ): Librarian[V] =
    SimpleLibrarian[V](get)(path, loops, mit, permissions)
}
