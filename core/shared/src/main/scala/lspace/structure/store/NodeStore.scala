package lspace.structure.store

import lspace.structure.Graph

object NodeStore {
  type Aux[G <: Graph, T0, T20] = NodeStore[G] {
    type T  = T0
    type T2 = T20
  }
}
trait NodeStore[G <: Graph] extends Store[G] {
  type T  = graph._Node
  type T2 = graph.GNode
}
