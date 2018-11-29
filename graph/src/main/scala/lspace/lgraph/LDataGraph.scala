package lspace.lgraph

import lspace.lgraph.index.LIndex
import lspace.librarian.structure._
import lspace.librarian.structure.index.Index

trait LDataGraph extends LGraph with DataGraph {
  protected def `@idIndex`: Index   = LIndex(Vector(Set(Property.default.`@id`)), thisgraph)
  protected def `@typeIndex`: Index = LIndex(Vector(Set(Property.default.`@type`)), thisgraph)
}
