package lspace.librarian.structure

import scala.collection.mutable.ListBuffer

object Transaction {}

trait Transaction extends Graph {
  def parent: Graph

  val addedResources: ListBuffer[Resource[_]] = ListBuffer[Resource[_]]()

  def commit(): Unit
  def isOpen: Boolean
  def rollback(): Unit
}
