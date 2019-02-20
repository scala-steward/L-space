package lspace.provider.mem

import lspace.provider.mem.index.MemIndex
import lspace.structure._
import lspace.structure.index.Index
import lspace.structure.index.shape.Shape

trait MemDataGraph extends MemGraph with DataGraph {
  protected[mem] lazy val `@idIndex`: Index = MemIndex(
    __[Any, Any].has(Property.default.`@id`).has(Property.default.`@ids`).untyped)
  protected[mem] lazy val `@typeIndex`: Index = MemIndex(__[Any, Any].has(Property.default.`@type`).untyped)
}
