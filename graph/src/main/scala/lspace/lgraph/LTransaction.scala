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
  val iri: String  = parent.iri + "/" + java.time.Instant.now() + "/" + (Math.random() * 100000).toInt
  private val self = this
  private val _iri = iri

  val index: MemIndexGraph = new MemIndexGraph {
    def iri: String = _iri + ".index"

    val graph: MemGraph      = self
    val index: MemIndexGraph = this
  }

  override def commit(): Task[Unit] = {
    if (isOpen) {
      val start = java.time.Instant.now().toEpochMilli

      (for {
        _ <- super.commit()
        _ = {
          parent.edgeStore.markDeleted(edges.deleted.keySet.toSet)
          parent.valueStore.markDeleted(edges.deleted.keySet.toSet)
          parent.nodeStore.markDeleted(edges.deleted.keySet.toSet)
        }
        (collections, others) = values.added.toList.partition(_.label.isInstanceOf[CollectionType[_]])
        newOtherValues <- Task {
          //          println(s"committing others ${others.map(_.prettyPrint)}")
          others.map(value => parent.newValue(value.id, value.value, value.label))
        }
        //        _ <- Task.defer { parent.values().toListL.map(r => println(r.map(_.prettyPrint))) }
        newNodes <- Task.parSequence {
          nodes.added.toList.map(_._2).map { node =>
            for {
              newNode <- Task { parent.newNode(node.id) }
              _ = node.labels.foreach(newNode._cacheLabel)
            } yield newNode
          }
        }
        _ = parent.nodeStore.cache(newNodes.asInstanceOf[List[parent.nodeStore.T]])
        newCollections <- Task {
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
              (dereferenceValue(v1),
               dereferenceValue(v2),
               dereferenceValue(v3),
               dereferenceValue(v4),
               dereferenceValue(v5))
            //        case v: Ontology     => nodes.upsert(parent.ns.ontologies.store(v)) //irrelevant, value is already dereferenced
            //        case v: Property     => nodes.upsert(parent.ns.properties.store(v))
            //        case v: DataType[_]  => nodes.upsert(parent.ns.datatypes.store(v))
            case v: _TNode       => v.self
            case v: Node         => newNodes.find(_.id == v.id).getOrElse(throw new Exception("dereferencing node failed"))
            case v: _TEdge[_, _] => v.self
            case v: Edge[_, _]   => throw new Exception("dereferencing edge failed")
            case v: _TValue[_]   => v.self
            case v: Value[_] =>
              newOtherValues.find(_.id == v.id).getOrElse(throw new Exception("dereferencing value failed"))
            case _ => t
          }
          collections.map(value => parent.newValue(value.id, dereferenceValue(value.value), value.label))
        }
        _         = parent.valueStore.cache(newCollections.asInstanceOf[List[parent.valueStore.T]])
        newValues = newOtherValues ++ newCollections
        newEdges = edges.added.toList.map(edge =>
          parent.newEdge(
            edge.id,
            edge.from match {
              case r: _TNode       => r.self.asInstanceOf[parent._Resource[Any]]
              case r: _TEdge[_, _] => r.self.asInstanceOf[parent._Resource[Any]]
              case r: _TValue[_]   => r.self.asInstanceOf[parent._Resource[Any]]
              case r =>
                parent.resources.cached
                  .hasId(r.id)
                  .get
                  .asInstanceOf[parent._Resource[Any]]
            },
            edge.key,
            edge.to match {
              case r: _TNode       => r.self.asInstanceOf[parent._Resource[Any]]
              case r: _TEdge[_, _] => r.self.asInstanceOf[parent._Resource[Any]]
              case r: _TValue[_]   => r.self.asInstanceOf[parent._Resource[Any]]
              case r =>
                parent.resources.cached
                  .hasId(r.id)
                  .get
                  .asInstanceOf[parent._Resource[Any]]
            }
        ))
        _             = parent.edgeStore.cache(newEdges.asInstanceOf[List[parent.edgeStore.T]])
        removedEdges  = edges.deleted.values.toList
        removedNodes  = nodes.deleted.values.toList
        removedValues = values.deleted.values.toList
        _ <- Task
          .sequence(
            Seq(
              Task {
                removedNodes.foreach(parent.nodeStore.uncache)
                removedValues.foreach(parent.valueStore.uncache)
                removedEdges.foreach(parent.edgeStore.uncache)
              },
              parent.storeManager.deleteEdges(removedEdges),
              parent.storeManager.deleteNodes(removedNodes),
              parent.storeManager.deleteValues(removedValues),
              parent.storeManager.storeValues(newValues),
              parent.storeManager.storeNodes(newNodes),
              parent.storeManager.storeEdges(newEdges)
            ))
      } yield {
        val iEnd = java.time.Instant.now().toEpochMilli
        scribe.info(
          s"update cache took ${iEnd - start} millis, added #${newNodes.size} nodes - #${newEdges.size} edges - #${newValues.size}")
      }).onErrorHandleWith {
          case _: TimeoutException => Task.now("recovered")
          case other               => Task.raiseError(other)
        }
        .doOnFinish {
          case None =>
            val iEnd = java.time.Instant.now().toEpochMilli
            scribe.info(s"update graph took ${java.time.Instant.now().toEpochMilli - iEnd} millis")
            close()
            Task.unit
          case Some(ex) =>
            val iEnd = java.time.Instant.now().toEpochMilli
            scribe.error(ex.getMessage)
            scribe.info(s"update graph failed and took ${java.time.Instant.now().toEpochMilli - iEnd} millis")
            close()
            Task.unit
        }
        .map(f => ())
//      addedValues.asInstanceOf[List[parent._Value[_]]].foreach(parent.valueStore.cache)
//      addedNodes.foreach(parent.nodeStore.cache)
//      addedEdges.asInstanceOf[List[parent._Edge[_, _]]].foreach(parent.edgeStore.cache)

//        .runSyncUnsafe(3000 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
    } else {
      Task.unit
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
  override def rollback(): Task[Unit] = Task.now { open = false } //return claimed id's?

  override protected[lspace] def deleteNode(node: _Node): Task[Unit] =
    //1st prepare statements to remove objects from store/index (or remove first and then cache?)
    super.deleteNode(node)
  override protected[lspace] def deleteEdge(edge: _Edge[_, _]): Task[Unit] = super.deleteEdge(edge)
  override protected[lspace] def deleteValue(value: _Value[_]): Task[Unit] = super.deleteValue(value)
}
