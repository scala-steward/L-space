package lspace.parse

import lspace.librarian.structure.{Edge, Property, Resource}

trait ToLink {
  def link(resource: Resource[_]): Edge[Any, Any]
}

case class EdgeTo(key: Property, to: Resource[_]) extends ToLink {
  def link(from: Resource[_]): Edge[Any, Any] = from --- key --> to
}
case class EdgeFrom(key: Property, to: Resource[_]) extends ToLink {
  def link(from: Resource[_]): Edge[Any, Any] = to --- key --> from
}
