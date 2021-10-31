package lspace

// import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.Uri

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

// enum ClassType[T]:
//   case ResourceType[T]() extends ClassType[Resource[T]]
//   case ValueType[T]() extends ClassType[Value[T]]
//   case EdgeType[S, E]() extends ClassType[Edge[S, E]]
//   case NodeType() extends ClassType[Node]
sealed trait ClassType[+C]
case class ResourceType[+R]() extends ClassType[Resource[R]]
case class NodeType()         extends ClassType[Node]
case class EdgeType[+S, +E]() extends ClassType[Edge[S, E]]
case class ValueType[+V]()    extends ClassType[Value[V]]

final case class Traversal[+ST <: ClassType[Any], +ET <: ClassType[Any], Steps](steps: Steps)(
  val st: ST,
  val et: ET
)

object Traversal:

  def apply(): Traversal[ResourceType[Any], ResourceType[Any], EmptyTuple] =
    Traversal(EmptyTuple)(ResourceType[Any](), ResourceType[Any]())

  extension [ST <: ClassType[Any], ET <: ClassType[Any], Steps <: Tuple](
    traversal: Traversal[ST, ET, Steps]
  )
    // def and: Traversal[ST, ET, StepsLeaf[Step.And *: Steps]] =
    //   Traversal(Step.And() *: traversal.steps)(traversal.st, traversal.et)
    def has[p <: String](predicate: p): Traversal[ST, ET, Tuple.Concat[Steps, Tuple1[Has[predicate.type]]]] =
      Traversal(traversal.steps ++ Tuple1(Has(predicate)))(traversal.st, traversal.et)
    def out[p <: String](
      predicate: p
    ): Traversal[ST, ResourceType[Any], Tuple.Concat[Steps, Tuple1[Out[predicate.type]]]] =
      Traversal(traversal.steps ++ Tuple1(Out(predicate)))(traversal.st, ResourceType[Any]())

  val t: Traversal[ResourceType[Any], ResourceType[Any], (Out["name"], Out["has:name"], Out["has"])] =
    apply().out("name").out("has:name").out("has")

//NonEmptyTuple
end Traversal

// enum Step[T]:
//   case And()
//   case Has[T](t: T) extends Step[T]
sealed trait Step[+T]

object Has:
  def apply[p <: String](predicate: p): Has[predicate.type] = Has(predicate)
end Has
case class Has[P <: String] private (p: P) extends Step[Has[P]]

object Out:
  def apply[p <: String](predicate: p): Out[predicate.type] = Out(predicate)

  type EndType[ET, X] = X match
    case _ => X
end Out
case class Out[predicate <: String] private (predicate: predicate) extends Step[predicate.type]

