package lspace.librarian.provider.mem

import lspace.librarian.structure._

object MemEdge {
//  protected[provider] def apply[S, E](_outV: MemResource[S], _key: Property, _inV: MemResource[E])(
//      implicit _graph: MemGraph): MemEdge[S, E] = new MemEdge[S, E] {
//    val key: Property            = _key
//    implicit val graph: MemGraph = _graph
//
//    val outV: MemResource[S] = _outV
//    val inV: MemResource[E]  = _inV
//
//    val id: Long = graph.idGenerator.next
//  }

  protected[provider] def apply[S, E](_outV: MemResource[S], _key: Property, _inV: MemResource[E], _id: Long)(
      implicit _graph: MemGraph): MemEdge[S, E] = new MemEdge[S, E] {
    val key: Property         = _key
    implicit val graph: Graph = _graph

    val outV: Resource[S] = _outV
    val inV: Resource[E]  = _inV

    val id: Long = _id
  }
}

trait MemEdge[S, E] extends MemResource[Edge[S, E]] with Edge[S, E] {

  def inV: Resource[E]
  override def to: Resource[E] = inV

  def outV: Resource[S]
  override def from: Resource[S] = outV

  override def remove(): Unit = {
    super.remove()
    inV.removeIn(this)
    outV.removeOut(this)
  }
}
