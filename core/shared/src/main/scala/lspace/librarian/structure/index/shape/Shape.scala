package lspace.librarian.structure.index.shape

import lspace.librarian.structure._

object Shape {

  def apply(resource1: Resource[_], edge: Edge[_, _]*): Shape =
    new Shape(resource1, edge.toVector)
}

case class Shape private (origin: Resource[_], edges: Vector[Edge[_, _]]) {
  override def toString: String = "Shape(" + origin.prettyPrint :: edges.toList.map(_.prettyPrint) mkString (", ") + ")"
}
