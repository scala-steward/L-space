package lspace.lgraph

import lspace.lgraph.index.{IndexManager, LIndex}
import lspace.librarian.structure.index.Index
import lspace.librarian.structure.{ClassType, IndexGraph}

trait LIndexGraph extends LGraph with IndexGraph {
  def graph: LGraph
  def indexManager: IndexManager[this.type]
//  protected val `@patternIndex`: Index = createIndex(Vector(Set()))

  protected def createIndex(pattern: Vector[Set[_ <: ClassType[_]]]): Index = LIndex(pattern, thisgraph)

//  override def _storeEdge[S, E](edge: _Edge[S, E]): Unit = super._storeEdge(edge)
}
