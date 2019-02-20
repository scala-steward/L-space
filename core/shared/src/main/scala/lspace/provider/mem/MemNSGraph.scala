package lspace.provider.mem

import lspace.structure.NameSpaceGraph

trait MemNSGraph extends MemDataGraph with NameSpaceGraph {
  def graph: MemGraph

}
