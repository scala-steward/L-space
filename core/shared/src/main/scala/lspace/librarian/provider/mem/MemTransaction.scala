package lspace.librarian.provider.mem

import lspace.librarian.provider.transaction.Transaction
import monix.eval.Task

object MemTransaction {
  def apply(parent: MemGraph): MemTransaction = new MemTransaction(parent)
}

class MemTransaction(override val parent: MemGraph) extends Transaction(parent) {
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
      values.added.toList.map(value => parent.newValue(value.id, value.value, value.label))
      nodes.added.toList.map(_._2).map { node =>
        val newNode = parent.newNode(node.id)
        node.labels.foreach(newNode._addLabel)
        newNode
      }
      edges.added.toList.map(edge => parent.newEdge(edge.id, edge.from.id, edge.key, edge.to.id))

      edges.deleted.values.foreach(_.remove())
      nodes.deleted.values.foreach(_.remove())
      values.deleted.values.foreach(_.remove())
    }
  }

  /**
    * clears the transaction's MemGraph
    */
  override def rollback(): Unit = open = false //return claimed id's?
}
