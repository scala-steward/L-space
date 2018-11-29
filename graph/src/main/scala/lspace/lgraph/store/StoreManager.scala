package lspace.lgraph.store

import lspace.lgraph.{LGraph, LGraphIdProvider}
import lspace.librarian.structure._

abstract class StoreManager[G <: LGraph](val graph: G) {

  def nodeStore  = graph.nodeStore
  def edgeStore  = graph.edgeStore
  def valueStore = graph.valueStore

  def nodeById(id: Long): Option[graph._Node]
  def nodesById(ids: List[Long]): Stream[graph._Node]
  def nodeByIri(iri: String): Stream[graph._Node]
  def nodesByIri(iri: List[String]): Stream[graph._Node]
  def edgeById(id: Long): Option[graph._Edge[_, _]]
  def edgesById(ids: List[Long]): Stream[graph._Edge[_, _]]
  def edgesByFromId(fromId: Long): Stream[graph._Edge[_, _]]
  def edgesByFromIdAndKey(fromId: Long, key: Property): Stream[graph._Edge[_, _]]
  def edgesByToId(toId: Long): Stream[graph._Edge[_, _]]
  def edgesByToIdAndKey(toId: Long, key: Property): Stream[graph._Edge[_, _]]
  def edgesByFromIdAndToId(fromId: Long, toId: Long): Stream[graph._Edge[_, _]]
  def edgesByFromIdAndKeyAndToId(fromId: Long, key: Property, toId: Long): Stream[graph._Edge[_, _]]
//  def edgesByKey(key: Property): Stream[graph._Edge[_, _]]
  def edgeByIri(iri: String): Stream[graph._Edge[_, _]]
  def edgesByIri(iri: List[String]): Stream[graph._Edge[_, _]]
  def valueById(id: Long): Option[graph._Value[_]]
  def valuesById(ids: List[Long]): Stream[graph._Value[_]]
  def valueByIri(iri: String): Stream[graph._Value[_]]
  def valuesByIri(iri: List[String]): Stream[graph._Value[_]]
  def valueByValue[T](value: T, dt: DataType[T]): Stream[graph._Value[T]]
  def valuesByValue[T](values: List[(T, DataType[T])]): Stream[graph._Value[T]]

  def storeNodes(nodes: List[Node]): Unit
  def storeEdges(edges: List[Edge[_, _]]): Unit
  def storeValues(values: List[Value[_]]): Unit

  def nodes: Stream[graph._Node]
  def edges: Stream[graph._Edge[_, _]]
  def values: Stream[graph._Value[_]]

  /**
    * finishes write-queue(s) and closes connection
    */
  def close(): Unit

}
