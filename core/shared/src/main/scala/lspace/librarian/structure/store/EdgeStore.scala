package lspace.librarian.structure.store

import lspace.librarian.structure.Graph

trait EdgeStore[G <: Graph] extends Store[G] {
  type T  = graph.GEdge[_, _]
  type T2 = graph.GEdge[Any, Any]
}
