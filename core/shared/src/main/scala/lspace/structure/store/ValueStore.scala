package lspace.structure.store

import lspace.datatype.DataType
import lspace.structure.Graph
import monix.reactive.Observable

trait ValueStore[G <: Graph] extends Store[G] {
  type T  = graph._Value[_]
  type T2 = graph.GValue[Any]

  def byValue[V](value: V, dt: DataType[V]): Observable[graph._Value[V]]
}
