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

final case class Traversal[+ST <: ClassType[Any], +ET <: ClassType[Any], Steps](steps: Steps)(
  val st: ST,
  val et: ET
)

object Traversal:

  def apply(): Traversal[ResourceType[Any], ResourceType[Any], EmptyTuple] =
    Traversal(EmptyTuple)(ResourceType[Any](), ResourceType[Any]())

  val t: Traversal[ResourceType[Any], ResourceType[Any], (Out["name"], Out["has:name"], Out["has"])] =
    apply().out("name").out("has:name").out("has")

//NonEmptyTuple
end Traversal
