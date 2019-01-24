package lspace.librarian.provider.mem

import lspace.librarian.datatype.{CollectionType, DataType}
import lspace.librarian.provider.transaction.Transaction
import lspace.librarian.structure._

import scala.collection.immutable.ListSet

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
      val (collections, others) = values.added.partition(_.label.isInstanceOf[CollectionType[_]])
      val newValues             = others.toList.map(value => parent.newValue(value.id, value.value, value.label))
      val newNodes = nodes.added.toList.map(_._2).map { node =>
        val newNode = parent.newNode(node.id)
        node.labels.foreach(newNode._addLabel)
        newNode
      }

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
        case v: Node         => newNodes.find(_.id == v.id).getOrElse(throw new Exception("dereferencing node failed"))
        case v: _TEdge[_, _] => v.self
        case v: Edge[_, _]   => throw new Exception("dereferencing edge failed")
        case v: _TValue[_]   => v.self
        case v: Value[_]     => newValues.find(_.id == v.id).getOrElse(throw new Exception("dereferencing value failed"))
        case _               => t
      }
      collections.toList.map(value => parent.newValue(value.id, dereferenceValue(value.value), value.label))

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
