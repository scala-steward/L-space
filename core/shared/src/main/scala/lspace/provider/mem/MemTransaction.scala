package lspace.provider.mem

import lspace.datatype.{CollectionType, DataType}
import lspace.provider.transaction.Transaction
import lspace.structure._
import monix.eval.Task

import scala.collection.immutable.ListSet

object MemTransaction {
  def apply(parent: MemGraph): MemTransaction = new MemTransaction(parent)
}

class MemTransaction(override val parent: MemGraph) extends Transaction(parent) {
  val iri: String  = parent.iri + "/" + java.time.Instant.now() + "/" + (Math.random() * 100000).toInt
  private val self = this
  private val _iri = iri

  val index: MemIndexGraph = new MemIndexGraph {
    def iri: String = _iri + ".index"

    val graph: MemGraph      = self
    val index: MemIndexGraph = this
  }
  override def commit(): Task[Unit] =
    if (isOpen) {
//      println(s"commit ${nodes.added.size} nodes")
//      println(s"commit ${edges.added.size} edges")
//      println(s"commit ${values.added.size} values")
//      println(nodes.added.map(_._2.prettyPrint))
//      println(edges.added.map(_.prettyPrint))
//      println(values.added.map(_.prettyPrint))
      for {
        _ <- super.commit()
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
//        _ <- Task.defer { parent.nodes().toListL.map(r => println(r.map(_.prettyPrint))) }
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
        newValues = newOtherValues ++ newCollections
        _ = edges.added.toList.map { edge =>
//            println(s"committing ${edge.prettyPrint}")
          parent.newEdge[Any, Any](
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
          )
        }
//        _ <- Task.defer { parent.edges().toListL.map(r => println(r.map(_.prettyPrint))) }

        _ <- Task.sequence(edges.deleted.values.map(_.remove()))
        _ <- Task.sequence(nodes.deleted.values.map(_.remove()))
        _ <- Task.sequence(values.deleted.values.map(_.remove()))

      } yield ()
    } else Task.unit

  /**
    * clears the transaction's MemGraph
    */
  override def rollback(): Task[Unit] = Task { open = false } //return claimed id's?
}
