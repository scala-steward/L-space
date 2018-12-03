package lspace.librarian.provider.mem

import lspace.librarian.provider.transaction.Transaction
import monix.eval.Task

object MemTransaction {
  def apply(parent: MemGraph): MemTransaction = new MemTransaction(parent)
}

class MemTransaction(val parent: MemGraph) extends Transaction {
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
      nodes.added.map(node => parent.nodes.create(node.id)(node.labels: _*))
      values.added
        .map(_.asInstanceOf[MemValue[Any]])
        .map(value => parent.values.create(value.id)(value.value)(value.label))
      edges.added.map(edge => parent.edges.create(edge.id, edge.from.id, edge.key, edge.to.id))
      edges.deleted.flatMap(parent.edges.hasId).foreach(_.remove())
      nodes.deleted.flatMap(parent.nodes.hasId).foreach(_.remove())
      values.deleted.flatMap(parent.values.hasId).foreach(_.remove())
    }
  }

  /**
    * clears the transaction's MemGraph
    */
  override def rollback(): Unit = open = false //return claimed id's?
}
