package lspace

// import eu.timepit.refined._
// import eu.timepit.refined.api.Refined
// import eu.timepit.refined.string.Uri

/** @tparam R
  *   resource self-type
  */
sealed trait Resource[+R] extends Matchable derives CanEqual

open class Node extends Resource[Node]

/** @tparam S
  *   outV-type, edge-start
  * @tparam E
  *   inV-type, edge-end
  */
open class Edge[+S, +E] extends Resource[Edge[S, E]]

/** @tparam V
  *   value-type
  */
open class Value[+V](v: V) extends Resource[Value[V]]

open class Graph extends Resource[Graph]

final case class Traversal[+ST <: ClassType[?], +ET <: ClassType[?], Steps](steps: Steps)
// (
//   val st: ST,
//   val et: ET
// )

object Traversal:

  def apply(): Traversal[ResourceType[Any], ResourceType[Any], EmptyTuple] =
    Traversal(EmptyTuple) // (ResourceType[Any](), ResourceType[Any]())

  def empty[ST <: ClassType[?], ET <: ClassType[?]]: Traversal[ST, ET, EmptyTuple] =
    Traversal(EmptyTuple)

  type StartType[traversal] <: ClassType[?] = traversal match {
    case Traversal[st, et, steps] => st
  }
  type EndType[traversal] <: ClassType[?] = traversal match {
    case Traversal[st, et, steps] => et
  }
//NonEmptyTuple
end Traversal

type AnyTraversal[X] = X match {
  case Traversal[s, e, steps] => Traversal[s, e, steps]
}

type Traversals[X] <: Tuple = X match {
  case Traversal[s, e, steps] *: EmptyTuple => Traversal[s, e, steps] *: EmptyTuple
  case Traversal[s, e, steps] *: traversals => Traversal[s, e, steps] *: Traversals[traversals]
  case Traversal[s, e, steps]               => Traversal[s, e, steps] *: EmptyTuple
}

implicit def Traversals[X](traverals: X): Traversals[X] = traverals match {
  case (traversal: Traversal[s, e, steps]) *: EmptyTuple => (traversal *: EmptyTuple).asInstanceOf[Traversals[X]]
  case (traversal: Traversal[s, e, steps]) *: traversals =>
    (traversal *: Traversals(traversals)).asInstanceOf[Traversals[X]]
  case traversal: Traversal[s, e, steps] => (traversal *: EmptyTuple).asInstanceOf[Traversals[X]]
}

def TraversalsApply[ET <: ClassType[?], X](in: Traversal[ET, ET, EmptyTuple], traverals: X): Traversals[X] =
  traverals match {
    case (traversal: Traversal[s, e, steps]) *: EmptyTuple => (traversal *: EmptyTuple).asInstanceOf[Traversals[X]]
    case (traversal: Traversal[s, e, steps]) *: traversals =>
      (traversal *: Traversals(traversals)).asInstanceOf[Traversals[X]]
    case traversal: Traversal[s, e, steps] => (traversal *: EmptyTuple).asInstanceOf[Traversals[X]]
  }
