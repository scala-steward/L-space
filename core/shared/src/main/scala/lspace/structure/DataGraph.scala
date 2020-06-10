package lspace.structure

import lspace.datatype.DataType
import lspace.structure.index.Index
import lspace.structure.index.shape.Shape
import monix.eval.Task
import monix.execution.CancelableFuture
import monix.reactive.Observable

trait DataGraph extends Graph {

  lazy val init: Task[Unit] = {
    ns.init.flatMap(u => index.init)
  }.memoizeOnSuccess

  def index: IndexGraph

  override protected[lspace] def getOrCreateNode(id: Long): Task[GNode] = {
    for {
      node <- super.getOrCreateNode(id)
//      e    <- _indexNode(node)
    } yield node
  }

  override protected[lspace] def storeNode(node: GNode): Task[Unit] =
    for {
      _ <- super.storeNode(node)
      _ <- indexNode(node)
    } yield ()

  override protected[lspace] def deleteNode(node: _Node): Task[Unit] = {
//        `@typeIndex`.delete()
    super.deleteNode(node)
  }

  protected[lspace] def indexNode(node: _Node): Task[Unit] = {
    index.indexes.`@typeIndex`.store(Shape(node))
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
  abstract override protected[lspace] def createEdge[S, E](id: Long,
                                                           from: _Resource[S],
                                                           key: Property,
                                                           to: _Resource[E]): Task[GEdge[S, E]] = {
    for {
      edge <- super.createEdge(id, from, key, to)
      e    <- indexEdge(edge)
    } yield edge
//    val edge = super.createEdge(id, _from, key, _to)
//    if (ns.properties.get(key.iri).isEmpty) ns.properties.store(key)
//    _indexEdge(edge)
//    edge
  }

  override protected[lspace] def deleteEdge(edge: _Edge[_, _]): Task[Unit] = {
    super.deleteEdge(edge)
  }

  protected def indexEdge[S, E](edge: _Edge[S, E]): Task[Unit] = {

    val from = edge.from
    val key  = edge.key
    val to   = edge.to

    if (key == Property.default.`@id`) {
      index.indexes.`@idIndex`.store(Shape(from))
    } else if (key == Property.default.`@ids`) {
      index.indexes.`@idIndex`.store(Shape(from))
    } else {
      Observable
        .fromTask(index.indexes.getOrCreate(lspace.__[Any, Any].has(key).untyped))
        .flatMap { kvIndex =>
          Observable.fromTask(kvIndex.store(Shape(from))) ++
            (if (from.labels.nonEmpty) {
               Observable
                 .fromTask(
                   index.indexes.getOrCreate(lspace.__[Any, Any].has(Property.default.`@type`).has(key).untyped))
                 .mapEval { lkvIndex =>
                   lkvIndex.store(Shape(from, edge))
                 }
             } else Observable.empty[Unit])
        }
        .completedL
    }
  }

  abstract override protected[lspace] def createValue[T](_id: Long, _value: T, dt: DataType[T]): Task[GValue[T]] =
    super.createValue(_id, _value, dt)
//    if (dt != DataType.default.`@boolean`) _indexValue(value.asInstanceOf[GValue[_]])

  override protected[lspace] def deleteValue(value: _Value[_]): Task[Unit] =
    super.deleteValue(value)
//  protected def _indexValue(value: GValue[_]): Unit = {
//    index
//      .getOrCreateIndex(Set(value.label))
//      .store(Vector((Map(value.label -> List(value)), value)))
//  }

  override def purge: Task[Unit] =
    for {
      _ <- super.purge
      _ <- if (ns != this && index != this && ns.index != this) ns.purge else Task.unit
      _ <- if (ns != this && index != this) index.purge else Task.unit
    } yield ()
}
