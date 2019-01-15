package lspace.lgraph.provider.mem

import lspace.lgraph._
import lspace.lgraph.store.StoreManager
import lspace.librarian.datatype._
import lspace.librarian.structure.Property
import monix.eval.Task

object MemStoreManager {
  def apply[G <: LGraph](graph: G): MemStoreManager[G] = new MemStoreManager[G](graph)
}
class MemStoreManager[G <: LGraph](override val graph: G) extends StoreManager(graph) {

  override def nodeById(id: Long): Option[graph._Node with LNode] = None

  override def nodesById(ids: List[Long]): Stream[graph._Node with LNode] = Stream()

  override def nodeByIri(iri: String): Stream[graph._Node with LNode] = Stream()

  override def nodesByIri(iri: List[String]): Stream[graph._Node with LNode] = Stream()

  override def edgeById(id: Long): Option[graph._Edge[Any, Any] with LEdge[Any, Any]] = None

  override def edgesById(ids: List[Long]): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgesByFromId(fromId: Long): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgesByFromIdAndKey(fromId: Long, key: Property): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] =
    Stream()

  override def edgesByToId(toId: Long): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgesByToIdAndKey(toId: Long, key: Property): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] =
    Stream()

  override def edgesByFromIdAndToId(fromId: Long, toId: Long): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] =
    Stream()

  override def edgesByFromIdAndKeyAndToId(fromId: Long,
                                          key: Property,
                                          toId: Long): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgeByIri(iri: String): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def edgesByIri(iri: List[String]): Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def valueById(id: Long): Option[graph._Value[Any] with LValue[Any]] = None

  override def valuesById(ids: List[Long]): Stream[graph._Value[Any] with LValue[Any]] = Stream()

  override def valueByIri(iri: String): Stream[graph._Value[Any] with LValue[Any]] = Stream()

  override def valuesByIri(iri: List[String]): Stream[graph._Value[Any] with LValue[Any]] = Stream()

  override def valueByValue[T](value: T, dt: DataType[T]): Stream[graph._Value[T] with LValue[T]] = Stream()

  override def valuesByValue[T](values: List[(T, DataType[T])]): Stream[graph._Value[T] with LValue[T]] = Stream()

  override def storeNodes(nodes: List[graph._Node with LNode]): Task[_] = Task {}

  override def storeEdges(
      edges: List[(graph._Edge[_$1, _$2] with LEdge[_$1, _$2]) forSome { type _$1; type _$2 }]): Task[_] =
    Task {}

  override def storeValues(values: List[(graph._Value[_$1] with LValue[_$1]) forSome { type _$1 }]): Task[_] =
    Task {}

  override def deleteNodes(nodes: List[graph._Node with LNode]): Task[_] =
    Task {}

  override def deleteEdges(
      edges: List[(graph._Edge[_$1, _$2] with LEdge[_$1, _$2]) forSome { type _$1; type _$2 }]): Task[_] =
    Task {}

  override def deleteValues(values: List[(graph._Value[_$1] with LValue[_$1]) forSome { type _$1 }]): Task[_] =
    Task {}

  override def nodes: Stream[graph._Node with LNode] = Stream()

  override def edges: Stream[graph._Edge[Any, Any] with LEdge[Any, Any]] = Stream()

  override def values: Stream[graph._Value[Any] with LValue[Any]] = Stream()

  override def nodeCount(): Long = graph.nodes().size

  override def edgeCount(): Long = graph.edges().size

  override def valueCount(): Long = graph.values().size

  /**
    * finishes write-queue(s) and closes connection
    */
  override def close(): Unit = {}
}
