package lspace.lgraph.index

import lspace.lgraph.LGraph
import lspace.librarian.traversal.UntypedTraversal
import lspace.structure.index.IndexSpec

trait LGraphIndexSpec extends IndexSpec {
  val graph: LGraph
  def createIndex(traversal: UntypedTraversal): LIndex
}
