package lspace.lgraph.store

import lspace.lgraph.{LGraph, LGraphIdProvider}
import lspace.datatype.DataType
import lspace.structure._
import lspace.structure.store.{EdgeStore, NodeStore, ValueStore}
import lspace.structure.util.ClassTypeable
import monix.eval.Task
import monix.reactive.Observable

abstract class StoreManager[G <: LGraph](val graph: G) {

  def nodeStore: NodeStore[G]   = graph.nodeStore.asInstanceOf[NodeStore[G]]
  def edgeStore: EdgeStore[G]   = graph.edgeStore.asInstanceOf[EdgeStore[G]]
  def valueStore: ValueStore[G] = graph.valueStore.asInstanceOf[ValueStore[G]]

  def nodeById(id: Long): Task[Option[graph.GNode]]
  def nodesById(ids: List[Long]): Observable[graph.GNode]
//  def nodeByIri(iri: String): Observable[graph.GNode]
//  def nodesByIri(iri: List[String]): Observable[graph.GNode]
  def edgeById(id: Long): Task[Option[graph.GEdge[Any, Any]]]
  def edgesById(ids: List[Long]): Observable[graph.GEdge[Any, Any]]
  def edgesByFromId(fromId: Long): Observable[graph.GEdge[Any, Any]]
  def edgesByFromIdAndKey(fromId: Long, key: Property): Observable[graph.GEdge[Any, Any]]
  def edgesByToId(toId: Long): Observable[graph.GEdge[Any, Any]]
  def edgesByToIdAndKey(toId: Long, key: Property): Observable[graph.GEdge[Any, Any]]
  def edgesByFromIdAndToId(fromId: Long, toId: Long): Observable[graph.GEdge[Any, Any]]
  def edgesByFromIdAndKeyAndToId(fromId: Long, key: Property, toId: Long): Observable[graph.GEdge[Any, Any]]
//  def edgesByKey(key: Property): Stream[graph.GEdge[_, _]]
//  def edgeByIri(iri: String): Observable[graph.GEdge[Any, Any]]
//  def edgesByIri(iri: List[String]): Observable[graph.GEdge[Any, Any]]
  def valueById(id: Long): Task[Option[graph.GValue[Any]]]
  def valuesById(ids: List[Long]): Observable[graph.GValue[Any]]
//  def valueByIri(iri: String): Observable[graph.GValue[Any]]
//  def valuesByIri(iri: List[String]): Observable[graph.GValue[Any]]
  def valueByValue[T](value: T, dt: DataType[T]): Observable[graph.GValue[T]]
//  def valueByValue[T](value: T)(
//      implicit dt: ClassTypeable[T]): Observable[graph.GValue[T]] =
//    valueByValue[T](value, dt.ct.asInstanceOf[DataType[T]])
  def valuesByValue[T](values: List[(T, DataType[T])]): Observable[graph.GValue[T]]

  def storeNodes(nodes: List[graph._Node]): Task[_]
  def storeEdges(edges: List[graph._Edge[_, _]]): Task[_]
  def storeValues(values: List[graph._Value[_]]): Task[_]

  def deleteNodes(nodes: List[graph._Node]): Task[_]
  def deleteEdges(edges: List[graph._Edge[_, _]]): Task[_]
  def deleteValues(values: List[graph._Value[_]]): Task[_]

  def nodes: Observable[graph.GNode]
  def edges: Observable[graph.GEdge[Any, Any]]
  def values: Observable[graph.GValue[Any]]

  def nodeCount(): Task[Long]
  def edgeCount(): Task[Long]
  def valueCount(): Task[Long]

  def init: Task[Unit]

  def persist: Task[Unit]

  def purge: Task[Unit]

  /**
    * finishes write-queue(s) and closes connection
    */
  def close(): Task[Unit]

}
