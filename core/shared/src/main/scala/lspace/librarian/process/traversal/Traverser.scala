package lspace.librarian.process.traversal

import java.time.Instant

object Traverser {
  def apply[T](_get: T) = new Traverser(_get)
}

/**
  * TODO: create different type of traversers, e.g. no need to keep path if the traversal does not have a path-step
  * @param get current object being traversed
  * @param path stores the traversal path
  * @param loops counter for loop-detection
  * @param mit moment-in-time, for time-aware traversals (@deleted < mit or @created > mit are out-of-scope)
  * @tparam T
  */
case class Traverser[+T](get: T,
                         path: TraversalPath = TraversalPath(),
                         loops: Int = 0,
                         mit: Option[Instant] = None,
                         permissions: List[String] = List()) {
  //TODO: labeled-path
}
