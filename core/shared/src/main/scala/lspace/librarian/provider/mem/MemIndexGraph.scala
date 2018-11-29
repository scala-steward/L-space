package lspace.librarian.provider.mem

import lspace.librarian.provider.mem.index.MemIndex
import lspace.librarian.structure.index.Index
import lspace.librarian.structure.{ClassType, IndexGraph, Property}

trait MemIndexGraph extends MemGraph with IndexGraph {
  def graph: MemGraph

  protected[mem] lazy val `@typeIndex`: Index = MemIndex(Vector(Set(Property.default.`@type`)), thisgraph)
//  protected val `@patternIndex`: Index = createIndex(Vector(Set()))

  protected def createIndex(pattern: Vector[Set[_ <: ClassType[_]]]): Index = MemIndex(pattern, thisgraph)

//  override def _storeEdge[S, E](edge: _Edge[S, E]): Unit = super._storeEdge(edge)
}
