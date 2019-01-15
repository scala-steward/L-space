package lspace.lgraph.provider.elasticsearch

import lspace.lgraph.LGraph
import lspace.lgraph.index.{LGraphIndexSpec, LIndex}
import lspace.lgraph.provider.mem.MemStoreProvider
import lspace.librarian.process.traversal.UntypedTraversal

class ESIndexSpec extends LGraphIndexSpec {

  val store = MemStoreProvider("ESIndexSpec")
  val graph: LGraph =
    LGraph(store, new ESIndexProvider)
  def createIndex(traversal: UntypedTraversal): LIndex = LIndex(traversal)
}
