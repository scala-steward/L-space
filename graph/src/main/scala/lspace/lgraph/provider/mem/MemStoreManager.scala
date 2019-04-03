package lspace.lgraph.provider.mem

import lspace.lgraph._
import lspace.lgraph.store.StoreManager
import lspace.datatype._
import lspace.structure.Property
import monix.eval.Task
import monix.execution.CancelableFuture
import monix.reactive.Observable

object MemStoreManager {
  def apply[G <: LGraph](graph: G): MemStoreManager[G] = new MemStoreManager[G](graph)
}

/**
  * This manager does not store or retrieve any resources. It is a stub to storageless LGraphs
  * @param graph
  * @tparam G
  */
class MemStoreManager[G <: LGraph](override val graph: G) extends StoreManager(graph) {

  override def nodeById(id: Long): Task[Option[graph._Node with LNode]] = Task.now(None)

  override def nodesById(ids: List[Long]): Observable[graph._Node with LNode] = Observable()

//  override def nodeByIri(iri: String): Observable[graph._Node with LNode] = Observable()

//  override def nodesByIri(iri: List[String]): Observable[graph._Node with LNode] = Observable()

  override def edgeById(id: Long): Task[Option[graph._Edge[Any, Any] with LEdge[Any, Any]]] = Task.now(None)

  override def edgesById(ids: List[Long]): Observable[graph._Edge[Any, Any] with LEdge[Any, Any]] = Observable()

  override def edgesByFromId(fromId: Long): Observable[graph._Edge[Any, Any] with LEdge[Any, Any]] = Observable()

  override def edgesByFromIdAndKey(fromId: Long,
                                   key: Property): Observable[graph._Edge[Any, Any] with LEdge[Any, Any]] =
    Observable()

  override def edgesByToId(toId: Long): Observable[graph._Edge[Any, Any] with LEdge[Any, Any]] = Observable()

  override def edgesByToIdAndKey(toId: Long, key: Property): Observable[graph._Edge[Any, Any] with LEdge[Any, Any]] =
    Observable()

  override def edgesByFromIdAndToId(fromId: Long, toId: Long): Observable[graph._Edge[Any, Any] with LEdge[Any, Any]] =
    Observable()

  override def edgesByFromIdAndKeyAndToId(fromId: Long,
                                          key: Property,
                                          toId: Long): Observable[graph._Edge[Any, Any] with LEdge[Any, Any]] =
    Observable()

//  override def edgeByIri(iri: String): Observable[graph._Edge[Any, Any] with LEdge[Any, Any]] = Observable()

//  override def edgesByIri(iri: List[String]): Observable[graph._Edge[Any, Any] with LEdge[Any, Any]] = Observable()

  override def valueById(id: Long): Task[Option[graph._Value[Any] with LValue[Any]]] = Task.now(None)

  override def valuesById(ids: List[Long]): Observable[graph._Value[Any] with LValue[Any]] = Observable()

//  override def valueByIri(iri: String): Observable[graph._Value[Any] with LValue[Any]] = Observable()

//  override def valuesByIri(iri: List[String]): Observable[graph._Value[Any] with LValue[Any]] = Observable()

  override def valueByValue[T](value: T, dt: DataType[T]): Observable[graph._Value[T] with LValue[T]] = Observable()

  override def valuesByValue[T](values: List[(T, DataType[T])]): Observable[graph._Value[T] with LValue[T]] =
    Observable()

  override def storeNodes(nodes: List[graph._Node]): Task[_] = Task {}

  override def storeEdges(edges: List[graph._Edge[_, _]]): Task[_] =
    Task {}

  override def storeValues(values: List[graph._Value[_]]): Task[_] =
    Task {}

  override def deleteNodes(nodes: List[graph._Node]): Task[_] =
    Task {}

  override def deleteEdges(edges: List[graph._Edge[_, _]]): Task[_] =
    Task {}

  override def deleteValues(values: List[graph._Value[_]]): Task[_] =
    Task {}

  override def nodes: Observable[graph._Node with LNode] = Observable()

  override def edges: Observable[graph._Edge[Any, Any] with LEdge[Any, Any]] = Observable()

  override def values: Observable[graph._Value[Any] with LValue[Any]] = Observable()

  override def nodeCount(): Task[Long] = graph.nodes().countL

  override def edgeCount(): Task[Long] = graph.edges().countL

  override def valueCount(): Task[Long] = graph.values().countL

  lazy val init: Task[Unit] = Task.unit

  def persist: Task[Unit] = Task.unit

  def purge: Task[Unit] = Task.unit

  /**
    * finishes write-queue(s) and closes connection
    */
  override def close(): Task[Unit] = Task.unit
}
