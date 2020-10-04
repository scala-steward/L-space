package lspace.lgraph

import lspace.structure.NameSpaceGraph

trait LNSGraph extends LDataGraph with NameSpaceGraph {
  def index: LIndexGraph
  def graph: LGraph
}
