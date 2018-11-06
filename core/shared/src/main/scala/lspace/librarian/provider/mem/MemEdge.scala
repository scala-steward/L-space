package lspace.librarian.provider.mem

import lspace.librarian.structure._

object MemEdge {
  protected[provider] def apply[S, E](_outV: MemResource[S], _key: Property, _inV: MemResource[E])(
      implicit _graph: MemGraph): MemEdge[S, E] = new MemEdge[S, E] {
    val key: Property            = _key
    implicit val graph: MemGraph = _graph

    val outV: MemResource[S] = _outV
    val inV: MemResource[E]  = _inV
  }
}

trait MemEdge[S, E] extends MemResource[Edge[S, E]] with Edge[S, E] {

  def inV: MemResource[E]
  override def to: MemResource[E] = inV

  def outV: MemResource[S]
  override def from: MemResource[S] = outV

  override def remove(): Unit = {
    super.remove()
    inV.removeInE(this)
    outV.removeOutE(this)
    graph.edgeCache -= id
  }
}
