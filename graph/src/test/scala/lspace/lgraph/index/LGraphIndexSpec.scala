package lspace.lgraph.index

import lspace.lgraph.LGraph
import lspace.librarian.process.traversal.UntypedTraversal
import lspace.librarian.structure.index.IndexSpec

trait LGraphIndexSpec extends IndexSpec {
  val graph: LGraph
  def createIndex(traversal: UntypedTraversal): LIndex
}
