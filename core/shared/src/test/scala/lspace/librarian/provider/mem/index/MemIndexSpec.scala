package lspace.librarian.provider.mem.index

import lspace.librarian.process.traversal.UntypedTraversal
import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure.index.IndexSpec

class MemIndexSpec extends IndexSpec {
  val graph: MemGraph                                    = MemGraph("MemIndexSpec")
  def createIndex(traversal: UntypedTraversal): MemIndex = MemIndex(traversal)
}
