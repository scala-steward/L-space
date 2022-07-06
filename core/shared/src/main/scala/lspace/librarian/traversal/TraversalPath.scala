package lspace.librarian.traversal

import lspace.structure.IriResource

import scala.collection.immutable.ListMap

//TODO: labeled-path
case class TraversalPath(
  resources: List[IriResource] = List[IriResource](),
  labeled: ListMap[String, Any] = ListMap[String, Any]()
)
