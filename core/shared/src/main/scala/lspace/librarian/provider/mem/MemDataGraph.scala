package lspace.librarian.provider.mem

import lspace.librarian.provider.mem.index.MemIndex
import lspace.librarian.structure._
import lspace.librarian.structure.index.Index

trait MemDataGraph extends MemGraph with DataGraph {
  protected[mem] lazy val `@idIndex`: Index   = MemIndex(Vector(Set(Property.default.`@id`)), thisgraph)
  protected[mem] lazy val `@typeIndex`: Index = MemIndex(Vector(Set(Property.default.`@type`)), thisgraph)
}
