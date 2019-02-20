package lspace.lgraph

import lspace.datatype.{CollectionType, DataType}
import lspace.provider.mem.{MemGraph, MemIndexGraph}
import lspace.provider.transaction.Transaction
import lspace.structure._
import monix.eval.Task

import scala.collection.immutable.ListSet
import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scribe._

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

      val (collections, others) = values.added.partition(_.label.isInstanceOf[CollectionType[_]])
      val addedOthers           = others.toList.map(value => parent.newValue(value.id, value.value, value.label))
      parent.valueStore.cache(addedOthers.asInstanceOf[List[parent.valueStore.T]])
      val addedNodes = nodes.added.toList.map(_._2).map { node =>
        val newNode = parent.newNode(node.id)
        node.labels.foreach(newNode._addLabel)
        newNode
      }
      parent.nodeStore.cache(addedNodes.asInstanceOf[List[parent.nodeStore.T]])

      def dereferenceValue(t: Any): Any = t match {
        case v: Vector[_]  => v.map(dereferenceValue)
        case v: ListSet[_] => v.map(dereferenceValue)
        case v: List[_]    => v.map(dereferenceValue)
        case v: Set[_]     => v.map(dereferenceValue)
        case v: Map[_, _]  => v.map { case (key, value) => (dereferenceValue(key), dereferenceValue(value)) }
        case (v1, v2)      => (dereferenceValue(v1), dereferenceValue(v2))
        case (v1, v2, v3)  => (dereferenceValue(v1), dereferenceValue(v2), dereferenceValue(v3))
        case (v1, v2, v3, v4) =>
          (dereferenceValue(v1), dereferenceValue(v2), dereferenceValue(v3), dereferenceValue(v4))
        case (v1, v2, v3, v4, v5) =>
          (dereferenceValue(v1), dereferenceValue(v2), dereferenceValue(v3), dereferenceValue(v4), dereferenceValue(v5))
//        case v: Ontology     => nodes.upsert(ns.ontologies.store(v))
//        case v: Property     => nodes.upsert(ns.properties.store(v))
//        case v: DataType[_]  => nodes.upsert(ns.datatypes.store(v))
        case v: _TNode       => v.self
        case v: Node         => addedNodes.find(_.id == v.id).getOrElse(throw new Exception("dereferencing node failed"))
        case v: _TEdge[_, _] => v.self
        case v: Edge[_, _]   => throw new Exception("dereferencing edge failed")
        case v: _TValue[_]   => v.self
        case v: Value[_]     => addedOthers.find(_.id == v.id).getOrElse(throw new Exception("dereferencing value failed"))
        case _               => t
      }
      val addedCollections =
        collections.toList.map(value => parent.newValue(value.id, dereferenceValue(value.value), value.label))
      parent.valueStore.cache(addedCollections.asInstanceOf[List[parent.valueStore.T]])
      val addedValues = addedOthers ++ addedCollections

      val addedEdges = edges.added.toList.map(edge => parent.newEdge(edge.id, edge.from.id, edge.key, edge.to.id))
      parent.edgeStore.cache(addedEdges.asInstanceOf[List[parent.edgeStore.T]])

//      addedValues.asInstanceOf[List[parent.GValue[_]]].foreach(parent.valueStore.cache)
//      addedNodes.foreach(parent.nodeStore.cache)
//      addedEdges.asInstanceOf[List[parent.GEdge[_, _]]].foreach(parent.edgeStore.cache)

      val removedEdges  = edges.deleted.values.toList
      val removedNodes  = nodes.deleted.values.toList
      val removedValues = values.deleted.values.toList

      val iEnd = java.time.Instant.now().toEpochMilli
      scribe.info(
        s"update cache took ${iEnd - start} millis, added #${addedNodes.size} nodes - #${addedEdges.size} edges - #${addedValues.size}")
      Task
        .sequence(Seq(
          Task {
            removedNodes.foreach(parent.nodeStore.uncache)
            removedValues.foreach(parent.valueStore.uncache)
            removedEdges.foreach(parent.edgeStore.uncache)
          },
          parent.storeManager.deleteEdges(removedEdges),
          parent.storeManager.deleteNodes(removedNodes),
          parent.storeManager.deleteValues(removedValues),
          parent.storeManager.storeValues(addedValues),
          parent.storeManager.storeNodes(addedNodes),
          parent.storeManager.storeEdges(addedEdges)
        ))
        .onErrorHandleWith {
          case _: TimeoutException => Task.now("recovered")
          case other               => Task.raiseError(other)
        }
        .doOnFinish {
          case None =>
            scribe.info(s"update graph took ${java.time.Instant.now().toEpochMilli - iEnd} millis")
            close()
            Task.unit
          case Some(ex) =>
            scribe.error(ex.getMessage)
            scribe.info(s"update graph failed and took ${java.time.Instant.now().toEpochMilli - iEnd} millis")
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
