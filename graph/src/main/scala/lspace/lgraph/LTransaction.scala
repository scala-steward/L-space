package lspace.lgraph

import lspace.librarian.provider.mem.{MemGraph, MemIndexGraph}
import lspace.librarian.provider.transaction.Transaction
import monix.eval.Task

import scala.concurrent.TimeoutException
import scala.concurrent.duration._

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
      Task
        .sequence(Seq(
          parent.storeManager.storeNodes(nodes.added.toList),
          parent.storeManager.storeValues(values.added.toList),
          parent.storeManager.storeEdges(edges.added.toList),
          parent.storeManager.deleteEdges(edges.deleted.toList.flatMap(id =>
            parent.edgeStore.cachedById(id).orElse(parent.storeManager.edgeById(id)))),
          parent.storeManager.deleteNodes(nodes.deleted.toList.flatMap(id =>
            parent.nodeStore.cachedById(id).orElse(parent.storeManager.nodeById(id)))),
          parent.storeManager.deleteValues(values.deleted.toList.flatMap(id =>
            parent.valueStore.cachedById(id).orElse(parent.storeManager.valueById(id)))),
          Task {
            val edgesToUnCache  = edges.deleted.toList.flatMap(id => parent.edgeStore.cachedById(id))
            val nodesToUnCache  = nodes.deleted.toList.flatMap(id => parent.nodeStore.cachedById(id))
            val valuesToUnCache = values.deleted.toList.flatMap(id => parent.valueStore.cachedById(id))

            nodesToUnCache.foreach(parent.nodeStore.uncacheByIri)
            edgesToUnCache.foreach(parent.edgeStore.uncacheByIri)
            valuesToUnCache.foreach(parent.valueStore.uncacheByIri)
            nodesToUnCache.foreach(parent.nodeStore.uncache)
            edgesToUnCache.foreach(parent.edgeStore.uncache)
            valuesToUnCache.foreach(parent.valueStore.uncache)
          }
        ))
        .onErrorHandleWith {
          case _: TimeoutException => Task.now("recovered")
          case other               => Task.raiseError(other)
        }
        .doOnFinish {
          case None =>
            close()
            Task.unit
          case Some(ex) =>
            close()
            Task.unit
        }
        .runSyncUnsafe(300 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
    } else {
      close()
    }

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

  override def _deleteNode(node: _Node): Unit = {
    //1st prepare statements to remove objects from store/index (or remove first and then cache?)
    super._deleteNode(node)
  }
  override def _deleteEdge(edge: _Edge[_, _]): Unit = super._deleteEdge(edge)
  override def _deleteValue(value: _Value[_]): Unit = super._deleteValue(value)
}
