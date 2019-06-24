package lspace.librarian.traversal

import java.time.Instant

import lspace.structure.Graph

object Librarian {
//  def apply[T](_get: T) = new Traverser(_get)
}

/**
  * TODO: create different type of librarians/traversers, e.g. no need to keep path if the traversal does not have a path-step
  */
trait Librarian[+T] {

  /**
    * current object being traversed
    * @return
    */
  def get: T

  /**
    * stores the traversal path
    * @return
    */
  def path: TraversalPath

  /**
    * counter for loop-detection
    * @return
    */
  def loops: Int

  /**
    * moment-in-time, for time-aware traversals (@deleted < mit or @created > mit are out-of-scope)
    * @return
    */
  def mit: Option[Instant]

  /**
    * Permissions of the Librarian
    * @return
    */
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
