package lspace.structure.store

import lspace.structure.Graph

trait NodeStore[G <: Graph] extends Store[G] {
  type T  = graph._Node
  type T2 = graph.GNode
}
