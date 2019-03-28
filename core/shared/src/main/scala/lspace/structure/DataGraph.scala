package lspace.structure

import lspace.datatype.DataType
import lspace.structure.index.Index
import lspace.structure.index.shape.Shape
import monix.eval.Task
import monix.execution.CancelableFuture
import monix.reactive.Observable

trait DataGraph extends Graph {

  lazy val init: CancelableFuture[Unit] = {
    ns.init.flatMap(u => index.init)(lspace.Implicits.Scheduler.global)
  }

  def index: IndexGraph
  protected def `@idIndex`: Index
  protected def `@typeIndex`: Index

  override protected[lspace] def getOrCreateNode(id: Long): Task[GNode] = {
    for {
      node <- super.getOrCreateNode(id)
      e    <- _indexNode(node)
    } yield node
  }

  override protected def deleteNode(node: GNode): Task[Unit] = {
//        `@typeIndex`.delete()
    super.deleteNode(node)
  }

  protected def _indexNode(node: GNode): Task[Unit] = {
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
                                                   _to: GResource[E]): Task[GEdge[S, E]] = {
    for {
      edge <- super.createEdge(id, _from, key, _to)
      e    <- _indexEdge(edge)
    } yield edge
//    val edge = super.createEdge(id, _from, key, _to)
//    if (ns.properties.get(key.iri).isEmpty) ns.properties.store(key)
//    _indexEdge(edge)
//    edge
  }

  override protected def deleteEdge(edge: GEdge[_, _]): Task[Unit] = {
    super.deleteEdge(edge)
  }

  protected def _indexEdge[S, E](edge: GEdge[S, E]): Task[Unit] = {

    val from = edge.from
    val key  = edge.key
    val to   = edge.to

    if (key == Property.default.`@id`) {
      `@idIndex`.store(Shape(from))
    } else if (key == Property.default.`@ids`) {
      `@idIndex`.store(Shape(from))
    } else {
      Observable
        .fromTask(index.getOrCreateIndex(__[Any, Any].has(key).untyped))
        .flatMap { kvIndex =>
          Observable.fromTask(kvIndex.store(Shape(from))) ++
            (if (from.labels.nonEmpty) {
               Observable
                 .fromTask(index.getOrCreateIndex(__[Any, Any].has(Property.default.`@type`).has(key).untyped))
                 .mapEval { lkvIndex =>
                   lkvIndex.store(Shape(from, edge))
                 }
             } else Observable.empty[Unit])
        }
        .completedL
    }
  }

  abstract override protected def createValue[T](_id: Long, _value: T, dt: DataType[T]): Task[GValue[T]] =
    super.createValue(_id, _value, dt)
//    if (dt != DataType.default.`@boolean`) _indexValue(value.asInstanceOf[GValue[_]])

  override protected def deleteValue(value: GValue[_]): Task[Unit] =
    super.deleteValue(value)
//  protected def _indexValue(value: GValue[_]): Unit = {
//    index
//      .getOrCreateIndex(Set(value.label))
//      .store(Vector((Map(value.label -> List(value)), value)))
//  }
}
