package lspace.provider.mem.index

import lspace.librarian.traversal.UntypedTraversal
import lspace.provider.mem.MemGraph
import lspace.structure.index.IndexSpec

class MemIndexSpec extends IndexSpec {
  val graph: MemGraph                                    = MemGraph("MemIndexSpec")
  def createIndex(traversal: UntypedTraversal): MemIndex = MemIndex(traversal)
}
