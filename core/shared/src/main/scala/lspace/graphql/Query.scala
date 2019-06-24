package lspace.graphql

import lspace._
import lspace.librarian.traversal.UntypedTraversal
import lspace.librarian.traversal.step.Project
import shapeless.{HList, HNil}

import scala.collection.immutable.ListMap

case class Query(projections: List[Projection]) extends GraphQL {
  def toTraversal: Traversal[ClassType[Any], ClassType[Any], _ <: HList] = {
    projections
      .map { projection =>
        projection.query match {
          case Some(query) =>
            //          (projection.toTraversal.untyped ++ UntypedTraversal(
            //            Vector(Project[HList](query.projections.foldLeft[HList](HNil) {
            //              case (r, t) => t.toTraversal :: r
            //            })))).toTyped.retype()
            projection.toTraversal.untyped ++ query.toTraversal.untyped
          case None =>
            projection.toTraversal.untyped
          //.retype() //causes unnecessary retyping for nested queries, only retype most parent traversal
        }
      } match {
      case Nil               => g
      case projection :: Nil => g ++ UntypedTraversal(Vector(Project[HList](projection.toTyped :: HNil))).toTyped
      case projections =>
        g ++ UntypedTraversal(Vector(Project[HList](projections.foldLeft[HList](HNil) {
          case (r, t) => t.toTyped :: r
        }))).toTyped
    }
  }
}
