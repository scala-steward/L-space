package lspace.lgraph.store

import lspace.lgraph.{LGraph, LGraphIdProvider}
import lspace.librarian.structure._
import monix.eval.Task

abstract class StoreManager[G <: LGraph](val graph: G) {

  def nodeStore  = graph.nodeStore
  def edgeStore  = graph.edgeStore
  def valueStore = graph.valueStore

  def nodeById(id: Long): Option[graph.GNode]
  def nodesById(ids: List[Long]): Stream[graph.GNode]
  def nodeByIri(iri: String): Stream[graph.GNode]
  def nodesByIri(iri: List[String]): Stream[graph.GNode]
  def edgeById(id: Long): Option[graph.GEdge[Any, Any]]
  def edgesById(ids: List[Long]): Stream[graph.GEdge[Any, Any]]
  def edgesByFromId(fromId: Long): Stream[graph.GEdge[Any, Any]]
  def edgesByFromIdAndKey(fromId: Long, key: Property): Stream[graph.GEdge[Any, Any]]
  def edgesByToId(toId: Long): Stream[graph.GEdge[Any, Any]]
  def edgesByToIdAndKey(toId: Long, key: Property): Stream[graph.GEdge[Any, Any]]
  def edgesByFromIdAndToId(fromId: Long, toId: Long): Stream[graph.GEdge[Any, Any]]
  def edgesByFromIdAndKeyAndToId(fromId: Long, key: Property, toId: Long): Stream[graph.GEdge[Any, Any]]
//  def edgesByKey(key: Property): Stream[graph._Edge[_, _]]
  def edgeByIri(iri: String): Stream[graph.GEdge[Any, Any]]
  def edgesByIri(iri: List[String]): Stream[graph.GEdge[Any, Any]]
  def valueById(id: Long): Option[graph.GValue[Any]]
  def valuesById(ids: List[Long]): Stream[graph.GValue[Any]]
  def valueByIri(iri: String): Stream[graph.GValue[Any]]
  def valuesByIri(iri: List[String]): Stream[graph.GValue[Any]]
  def valueByValue[T](value: T, dt: DataType[T]): Stream[graph.GValue[T]]
  def valuesByValue[T](values: List[(T, DataType[T])]): Stream[graph.GValue[T]]

  def storeNodes(nodes: List[graph.GNode]): Task[_]
  def storeEdges(edges: List[graph.GEdge[_, _]]): Task[_]
  def storeValues(values: List[graph.GValue[_]]): Task[_]

  def deleteNodes(nodes: List[graph.GNode]): Task[_]
  def deleteEdges(edges: List[graph.GEdge[_, _]]): Task[_]
  def deleteValues(values: List[graph.GValue[_]]): Task[_]

  def nodes: Stream[graph.GNode]
  def edges: Stream[graph.GEdge[Any, Any]]
  def values: Stream[graph.GValue[Any]]

  def nodeCount(): Long
  def edgeCount(): Long
  def valueCount(): Long

  /**
    * finishes write-queue(s) and closes connection
    */
  def close(): Unit

}
