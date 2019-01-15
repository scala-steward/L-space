package lspace.lgraph

import lspace.librarian.provider.mem.{MemGraph, MemIndexGraph}
import lspace.librarian.provider.transaction.Transaction
import monix.eval.Task

import scala.concurrent.TimeoutException
import scala.concurrent.duration._

object LTransaction {
  def apply(parent: LGraph): LTransaction = new LTransaction(parent)
}

class LTransaction(override val parent: LGraph) extends Transaction(parent) {
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

      val start = java.time.Instant.now().toEpochMilli

      parent.edgeStore.markDeleted(edges.deleted.keySet.toSet)
      parent.valueStore.markDeleted(edges.deleted.keySet.toSet)
      parent.nodeStore.markDeleted(edges.deleted.keySet.toSet)

      val addedValues = values.added.toList.map(value => parent.newValue(value.id, value.value, value.label))
      parent.valueStore.cache(addedValues.asInstanceOf[List[parent.valueStore.T]])
      val addedNodes = nodes.added.toList.map(_._2).map { node =>
        val newNode = parent.newNode(node.id)
        node.labels.foreach(newNode._addLabel)
        newNode
      }
      parent.nodeStore.cache(addedNodes.asInstanceOf[List[parent.nodeStore.T]])
      val addedEdges = edges.added.toList.map(edge => parent.newEdge(edge.id, edge.from.id, edge.key, edge.to.id))
      parent.edgeStore.cache(addedEdges.asInstanceOf[List[parent.edgeStore.T]])

//      addedValues.asInstanceOf[List[parent.GValue[_]]].foreach(parent.valueStore.cache)
//      addedNodes.foreach(parent.nodeStore.cache)
//      addedEdges.asInstanceOf[List[parent.GEdge[_, _]]].foreach(parent.edgeStore.cache)

      val removedEdges  = edges.deleted.values.toList
      val removedNodes  = nodes.deleted.values.toList
      val removedValues = values.deleted.values.toList

      val iEnd = java.time.Instant.now().toEpochMilli
      println(
        s"update cache took ${iEnd - start} millis, added #${addedNodes.size} nodes - #${addedEdges.size} edges - #${addedValues.size}")
      Task
        .sequence(Seq(
          parent.storeManager.storeValues(addedValues),
          parent.storeManager.storeNodes(addedNodes),
          parent.storeManager.storeEdges(addedEdges),
          parent.storeManager.deleteEdges(removedEdges),
          parent.storeManager.deleteNodes(removedNodes),
          parent.storeManager.deleteValues(removedValues),
          Task {
            removedNodes.foreach(parent.nodeStore.uncacheByIri)
            removedValues.foreach(parent.valueStore.uncacheByIri)
            removedEdges.foreach(parent.edgeStore.uncacheByIri)

            removedEdges.foreach(parent.edgeStore.uncacheById)
            removedNodes.foreach(parent.nodeStore.uncacheById)
            removedValues.foreach(parent.valueStore.uncacheById)
          }
        ))
        .onErrorHandleWith {
          case _: TimeoutException => Task.now("recovered")
          case other               => Task.raiseError(other)
        }
        .doOnFinish {
          case None =>
            println(s"update graph took ${java.time.Instant.now().toEpochMilli - iEnd} millis")
            close()
            Task.unit
          case Some(ex) =>
            println(s"update graph failed and took ${java.time.Instant.now().toEpochMilli - iEnd} millis")
            close()
            Task.unit
        }
        .runSyncUnsafe(3000 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
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

  override def deleteNode(node: GNode): Unit = {
    //1st prepare statements to remove objects from store/index (or remove first and then cache?)
    super.deleteNode(node)
  }
  override def deleteEdge(edge: GEdge[_, _]): Unit = super.deleteEdge(edge)
  override def deleteValue(value: GValue[_]): Unit = super.deleteValue(value)
}
