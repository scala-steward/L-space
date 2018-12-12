package lspace.librarian.process.traversal

import lspace.librarian.structure.Resource

import scala.collection.immutable.ListMap

//TODO: labeled-path
case class TraversalPath(resources: List[Resource[_]] = List[Resource[_]](),
                         labeled: ListMap[String, Any] = ListMap[String, Any]())
