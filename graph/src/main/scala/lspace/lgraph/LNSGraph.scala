package lspace.lgraph

import lspace.librarian.structure.NameSpaceGraph

trait LNSGraph extends LDataGraph with NameSpaceGraph {
  def graph: LGraph

}
