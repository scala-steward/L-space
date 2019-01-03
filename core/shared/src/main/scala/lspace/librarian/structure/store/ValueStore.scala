package lspace.librarian.structure.store

import lspace.librarian.structure.{DataType, Graph}

trait ValueStore[G <: Graph] extends Store[G] {
  type T  = graph.GValue[_]
  type T2 = graph.GValue[Any]

  def byValue[V](value: V, dt: DataType[V]): Stream[graph.GValue[V]]
}
