package lspace.lgraph

import lspace.librarian.structure.{Edge, Property, Resource}

object LEdge {
  def apply[S, E](graph: LGraph)(id: Long,
                                 from: graph._Resource[S],
                                 key: Property,
                                 to: graph._Resource[E]): graph._Edge[S, E] with LEdge[S, E] = {
    val _id    = id
    val _from  = from
    val _key   = key
    val _to    = to
    val _graph = graph
    new graph._Edge[S, E] with LEdge[S, E] {
      val id: Long                   = _id
      override val from: Resource[S] = _from
      val key: Property              = _key
      override val to: Resource[E]   = _to
      val graph: LGraph              = _graph
    }
  }
}

trait LEdge[S, E] extends LResource[Edge[S, E]] with Edge[S, E] {
  def to: Resource[E]
  def inV: Resource[E] = to

  def from: Resource[S]
  def outV: Resource[S] = from
}
