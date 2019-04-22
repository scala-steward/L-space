package lspace.librarian.traversal

import java.time.Instant

import lspace.structure.Graph

object Librarian {
//  def apply[T](_get: T) = new Traverser(_get)
}

/**
  * TODO: create different type of librarians/traversers, e.g. no need to keep path if the traversal does not have a path-step
//  * @param get current object being traversed
//  * @param path stores the traversal path
//  * @param loops counter for loop-detection
//  * @param mit moment-in-time, for time-aware traversals (@deleted < mit or @created > mit are out-of-scope)
  * @tparam T
  */
trait Librarian[+T] {
  def get: T
  def path: TraversalPath
  def loops: Int
  def mit: Option[Instant]
  def permissions: List[String]

  def apply[V](get: V,
               path: TraversalPath = TraversalPath(),
               loops: Int = 0,
               mit: Option[Instant] = None,
               permissions: List[String] = List[String]()): Librarian[V]

  //TODO: labeled-path
  def copy[V](get: V = get,
              path: TraversalPath = path,
              loops: Int = loops,
              mit: Option[Instant] = mit,
              permissions: List[String] = permissions): Librarian[V] =
    apply[V](get, path, loops, mit, permissions)
}
