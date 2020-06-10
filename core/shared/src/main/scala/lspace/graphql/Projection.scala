package lspace.graphql

import lspace.librarian.traversal.UntypedTraversal
import lspace.librarian.traversal.step.Project
import lspace.{g, ClassType, P, Property, Traversal}
import shapeless.{HList, HNil}

import scala.collection.immutable.ListMap

case class Projection(name: String,
                      property: Property,
                      alias: String,
                      reverse: Boolean = false,
                      limit: Option[Int] = None,
                      offset: Option[Int] = None,
                      parameters: ListMap[Property, Any] = ListMap(), //add direction to parameter property (in/out)
                      projections: List[Projection] = List()) {

  lazy val toTraversal: Traversal[ClassType[Any], ClassType[Any], _ <: HList] = {
    val traversal =
      if (reverse) g.in(property)
      else g.out(property)

    val projection = projections
      .map(_.toTraversal) match {
      case Nil               => g.untyped
      case projection :: Nil => UntypedTraversal(Vector(Project[HList](projection :: HNil)))
      case projections =>
        UntypedTraversal(Vector(Project[HList](projections.foldLeft[HList](HNil) {
          case (r, t) => t :: r
        })))
    }

    val startFilter = parameters.foldLeft(g: Traversal[ClassType[Any], ClassType[Any], HList]) {
      case (t, (property, value)) => g.has(property, P.eqv(value))
    }
    val endFilter = (limit, offset) match {
      case (Some(limit), Some(offset)) if limit > 0 && offset >= 0 => g.range(offset, offset + limit)
      case (Some(limit), None) if limit > 0                        => g.limit(limit)
      case (None, Some(offset)) if offset >= 0                     => g.skip(offset)
      case (None, None)                                            => g
    }
    (startFilter.untyped ++ traversal.untyped ++ projection ++ endFilter.untyped).toTyped
  }
}
