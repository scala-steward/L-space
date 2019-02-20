package lspace.structure.store

import lspace.structure.Graph

trait NodeStore[G <: Graph] extends Store[G] {
  type T  = graph.GNode
  type T2 = graph.GNode
}
