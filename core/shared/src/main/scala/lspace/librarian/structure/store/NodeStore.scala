package lspace.librarian.structure.store

import lspace.librarian.structure.Graph

trait NodeStore[G <: Graph] extends Store[G] {
  type T  = graph.GNode
  type T2 = graph.GNode
}
