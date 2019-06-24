package lspace.graphql

import lspace.{g, ClassType, Property, Traversal}
import shapeless.HList

import scala.collection.immutable.ListMap

case class Projection(property: Property,
                      reverse: Boolean = false,
                      parameters: ListMap[String, Any] = ListMap(),
                      query: Option[Query] = None) {
  lazy val toTraversal: Traversal[ClassType[Any], ClassType[Any], _ <: HList] =
    if (reverse) g.in(property)
    else g.out(property)
}
