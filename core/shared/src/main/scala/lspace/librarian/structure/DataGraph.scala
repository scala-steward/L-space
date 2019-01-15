package lspace.librarian.structure

import lspace.librarian.datatype.DataType
import lspace.librarian.structure.index.Index
import lspace.librarian.structure.index.shape.Shape

trait DataGraph extends Graph {

  def init(): Unit = {
    ns.init()
    index.init()
  }

  def index: IndexGraph
  protected def `@idIndex`: Index
  protected def `@typeIndex`: Index

  override protected def getOrCreateNode(id: Long): GNode = {
    val node = super.getOrCreateNode(id)
    _indexNode(node)
    node
  }

  override protected def deleteNode(node: GNode): Unit = {
//        `@typeIndex`.delete()
    super.deleteNode(node)
  }

  protected def _indexNode(node: GNode): Unit = {
    `@typeIndex`.store(Shape(node))
  }

  /**
    * creates, stores and indexes an edge
    * @param from
    * @param key
    * @param to
    * @tparam S
    * @tparam E
    * @return
    */
  abstract override protected def createEdge[S, E](id: Long,
                                                   _from: GResource[S],
                                                   key: Property,
                                                   _to: GResource[E]): GEdge[S, E] = {
    val edge = super.createEdge(id, _from, key, _to)
    if (ns.properties.get(key.iri).isEmpty) ns.properties.store(key)
    _indexEdge(edge)
    edge
  }

  override protected def deleteEdge(edge: GEdge[_, _]): Unit = {
    super.deleteEdge(edge)
  }

  protected def _indexEdge[S, E](edge: GEdge[S, E]): Unit = {

    val from = edge.from
    val key  = edge.key
    val to   = edge.to

    if (key == Property.default.`@id`) {
      `@idIndex`.store(Shape(from))
    } else if (key == Property.default.`@ids`) {
      `@idIndex`.store(Shape(from))
    } else {
      val kvIndex = index.getOrCreateIndex(__[Any, Any].has(key).untyped)
//      val kvIndexOut = index.getOrCreateIndex(__[Any, Any].out(key).untyped)
      kvIndex.store(Shape(from))
      if (from.labels.nonEmpty) {
        val lkvIndex =
          index.getOrCreateIndex(__[Any, Any].has(Property.default.`@type`).has(key).untyped)
        lkvIndex.store(Shape(from, edge))
      }
    }
  }

  abstract override protected def createValue[T](_id: Long, _value: T, dt: DataType[T]): GValue[T] = {
    val value = super.createValue(_id, _value, dt)
//    if (dt != DataType.default.`@boolean`) _indexValue(value.asInstanceOf[GValue[_]])
    value
  }

  override protected def deleteValue(value: GValue[_]): Unit = {
    super.deleteValue(value)
  }

//  protected def _indexValue(value: GValue[_]): Unit = {
//    index
//      .getOrCreateIndex(Set(value.label))
//      .store(Vector((Map(value.label -> List(value)), value)))
//  }
}
