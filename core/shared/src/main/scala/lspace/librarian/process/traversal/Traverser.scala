package lspace.librarian.process.traversal

import java.time.Instant

object Traverser {
//  def apply[T](_get: T) = new Traverser(_get)
}

/**
  * TODO: create different type of traversers, e.g. no need to keep path if the traversal does not have a path-step
  * @param get current object being traversed
  * @param path stores the traversal path
  * @param loops counter for loop-detection
  * @param mit moment-in-time, for time-aware traversals (@deleted < mit or @created > mit are out-of-scope)
  * @tparam T
  */
trait Traverser[+T] {
  def get: T
  def path: TraversalPath
  def loops: Int
  def mit: Option[Instant]
  def permissions: List[String]

  def apply[V](get: V,
               path: TraversalPath = TraversalPath(),
               loops: Int = 0,
               mit: Option[Instant] = None,
               permissions: List[String] = List()): Traverser[V]

  //TODO: labeled-path
  def copy[V](get: V = get,
              path: TraversalPath = path,
              loops: Int = loops,
              mit: Option[Instant] = mit,
              permissions: List[String] = permissions): Traverser[V] = apply(get, path, loops, mit, permissions)
}
