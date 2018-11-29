package lspace.lgraph

import lspace.librarian.provider.mem.{MemGraph, MemIndexGraph}
import lspace.librarian.provider.transaction.Transaction

object LTransaction {
  def apply(parent: LGraph): LTransaction = new LTransaction(parent)
}

class LTransaction(val parent: LGraph) extends Transaction {
  val iri: String  = parent.iri + "/" + java.time.Instant.now() + "/" + (Math.random() * 100000 toInt)
  private val self = this
  private val _iri = iri

  val index: MemIndexGraph = new MemIndexGraph {
    def iri: String = _iri + ".index"

    val graph: MemGraph      = self
    val index: MemIndexGraph = this
  }

  override def commit(): Unit = {
    if (isOpen) {
      super.commit()
      parent.storeManager.storeNodes(nodes.added.toList)
      parent.storeManager.storeValues(values.added.toList)
      parent.storeManager.storeEdges(edges.added.toList)
      close()
    } else close()
//    parent.nodeStore.store(nodes.added)
//    nodes.added.map(node => parent.nodes.create(node.id)(node.labels: _*))
//    values.added
//      .map(_.asInstanceOf[LValue[Any]])
//      .map(value => parent.values.create(value.id)(value.value)(value.label))
//    edges.added.map(edge => parent.edges.create(edge.id, edge.from.id, edge.key, edge.to.id))
  }

  /**
    * clears the transaction's MemGraph
    */
  override def rollback(): Unit = open = false //return claimed id's?
}
