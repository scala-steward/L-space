package lspace.graphql

import lspace._
import lspace.librarian.traversal.UntypedTraversal
import lspace.librarian.traversal.step.Project
import shapeless.{HList, HNil}

import scala.collection.immutable.ListMap

object Query {}
case class Query(
  projections: List[Projection] = List(), // HLIST?
  limit: Option[Int] = None,
  offset: Option[Int] = None,
  parameters: ListMap[Property, Any] = ListMap()
) //add direction to parameter property (in/out)
    extends GraphQL {
  def toTraversal: Traversal[ClassType[Any], ClassType[Any], _ <: HList] = {
    val traversal = projections
      .map(_.toTraversal) match {
      case Nil               => g.untyped
      case projection :: Nil => UntypedTraversal(Vector(Project[HList](projection :: HNil)))
      case projections =>
        UntypedTraversal(Vector(Project[HList](projections.foldLeft[HList](HNil) { case (r, t) =>
          t :: r
        })))
    }
    val startFilter =
      parameters.foldLeft[lspace.librarian.traversal.Traversal[ClassType[Any], ClassType[Any], HList]](g) {
        case (t, (property, value)) => t.has(property, P.eqv(value))
      }
    val endFilter = (limit, offset) match {
      case (Some(limit), Some(offset)) =>
        if (limit <= 0) throw new IllegalArgumentException(s"limit <= 0, found limit = $limit")
        else if (offset < 0) throw new IllegalArgumentException(s"offset < 0, found offset $offset")
        else g.range(offset, offset + limit)
      case (Some(limit), None) =>
        if (limit <= 0) throw new IllegalArgumentException(s"limit <= 0, found limit = $limit")
        else g.limit(limit)
      case (None, Some(offset)) =>
        if (offset < 0) throw new IllegalArgumentException(s"offset < 0, found offset $offset")
        else g.skip(offset)
      case (None, None) => g
    }
    (startFilter.untyped ++ traversal ++ endFilter.untyped).toTyped
  }
}
