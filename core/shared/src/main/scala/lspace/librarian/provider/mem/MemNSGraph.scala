package lspace.librarian.provider.mem

import lspace.librarian.structure.NameSpaceGraph

trait MemNSGraph extends MemDataGraph with NameSpaceGraph {
  def graph: MemGraph

}
