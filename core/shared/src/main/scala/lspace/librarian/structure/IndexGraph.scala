package lspace.librarian.structure

import lspace.librarian.process.traversal.P
import lspace.librarian.structure.index.Index
import lspace.librarian.structure.util.IdProvider

import scala.collection.mutable

trait IndexGraph extends Graph {
  def graph: Graph
  def ns: NameSpaceGraph = graph.ns

  lazy val idProvider: IdProvider = graph.idProvider

//  protected def `@patternIndex`: Index
  protected def `@typeIndex`: Index

  def init(): Unit = {}

  protected val indexes: mutable.HashMap[Vector[Set[_ <: ClassType[_]]], Index] =
    new mutable.HashMap[Vector[Set[_ <: ClassType[_]]], Index]()

  def getIndex(pattern: Vector[Set[_ <: ClassType[_]]]): Option[Index] = indexes.get(pattern)
  def getIndex(pattern: Set[_ <: ClassType[_]]): Option[Index]         = getIndex(Vector(pattern))
  protected def createIndex(pattern: Vector[Set[_ <: ClassType[_]]]): Index
  def getOrCreateIndex(pattern: Vector[Set[_ <: ClassType[_]]]): Index = {
    //TODO: log when existing index is returned and no new index is created
    indexes.getOrElse(pattern, createIndex(pattern))
  }
  def getOrCreateIndex(pattern: Set[_ <: ClassType[_]]): Index = getOrCreateIndex(Vector(pattern))
  def deleteIndex(pattern: Vector[Set[_ <: ClassType[_]]]): Unit = {
    indexes.remove(pattern)
  }

  def find[T](predicates: List[P[T]], datatype: DataType[T]): List[Resource[T]] = {
    getIndex(Set(datatype)).toList
      .flatMap(_.find(predicates, datatype))
  }

  def find[T](predicates: List[P[T]], property: Property): List[Resource[T]] = {
    getIndex(Set(property)).toList
      .flatMap(_.find(predicates, property))
  }

  def find(values: Vector[Map[_ <: ClassType[_], List[P[_]]]]): List[Vector[Resource[_]]] = {
    getIndex(values.map(_.keySet)).toList.flatMap(_.find(values))
  }

  override protected def deleteNode(node: GNode): Unit = {
    //    `@typeIndex`.delete()
    super.deleteNode(node)
  }

  abstract override protected def createEdge[S, E](id: Long,
                                                   from: GResource[S],
                                                   key: Property,
                                                   to: GResource[E]): GEdge[S, E] = {
    val edge = super.createEdge(id, from, key, to)
    storeEdge(edge.asInstanceOf[GEdge[_, _]])
    edge
  }

  override protected def deleteEdge(edge: GEdge[_, _]): Unit = {
    super.deleteEdge(edge)
  }

  abstract override protected def createValue[T](_id: Long, _value: T, dt: DataType[T]): GValue[T] = {
    val value = super.createValue(_id, _value, dt)
    storeValue(value.asInstanceOf[GValue[_]])
    value
  }

  override protected def deleteValue(value: GValue[_]): Unit = {
    super.deleteValue(value)
  }
}
