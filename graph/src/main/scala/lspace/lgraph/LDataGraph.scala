package lspace.lgraph

import lspace.lgraph.index.LIndex
import lspace.librarian.structure._
import lspace.librarian.structure.index.Index

trait LDataGraph extends LGraph with DataGraph {
  protected def `@idIndex`: Index =
    LIndex(__[Any, Any].has(Property.default.`@id`).has(Property.default.`@ids`).untyped)
  protected def `@typeIndex`: Index = LIndex(__[Any, Any].has(Property.default.`@type`).untyped)
}
