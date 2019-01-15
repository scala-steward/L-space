package lspace.librarian.provider.mem

import lspace.librarian.provider.mem.index.MemIndex
import lspace.librarian.structure._
import lspace.librarian.structure.index.Index
import lspace.librarian.structure.index.shape.Shape

trait MemDataGraph extends MemGraph with DataGraph {
  protected[mem] lazy val `@idIndex`: Index = MemIndex(
    __[Any, Any].has(Property.default.`@id`).has(Property.default.`@ids`).untyped)
  protected[mem] lazy val `@typeIndex`: Index = MemIndex(__[Any, Any].has(Property.default.`@type`).untyped)
}
