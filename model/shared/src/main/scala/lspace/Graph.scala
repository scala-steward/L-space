package lspace

trait Graph extends Resource[Graph]

opaque type LGraph[name] <: Graph = Graph

object LGraph:
  def apply[name <: String](graph: Graph): LGraph[name] = graph

extension [name](graph: LGraph[name])
  def toGraph: Graph  = graph
  def toGraph2: Graph = graph
