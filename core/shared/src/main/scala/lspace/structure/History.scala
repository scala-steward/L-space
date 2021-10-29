package lspace.structure

import java.time.Instant

import lspace.datatype.{DataType, DateTimeType}
import monix.eval.Task

object History {
  val historyKeys = Set(
    Property.default.`@createdon`,
    Property.default.`@deletedon`,
    Property.default.`@modifiedon`,
    Property.default.`@transcendedon`
  )
}

/** The History-trait ensures data is never deleted but annotated with '@created', '@modified', '@deleted' and
  * '@trancended' tags.
  */
trait History extends Graph {

  override protected[lspace] def getOrCreateNode(id: Long): Task[GNode] =
    for {
      createdNode <- super.getOrCreateNode(id)
      value       <- createValue(id, Instant.now(), DateTimeType.datatype)
      _           <- idProvider.next
      // TODO: make time configurable
      _ <- edges.create(createdNode, Property.default.`@createdon`, value)
    } yield createdNode

  override protected[lspace] def createEdge[S, E](
    id: Long,
    from: _Resource[S],
    key: Property,
    to: _Resource[E]
  ): Task[GEdge[S, E]] =
    for {
      createdEdge <- super.createEdge(id, from, key, to)
      id          <- idProvider.next
      time        <- createValue(id, Instant.now(), DateTimeType.datatype).map(_.asInstanceOf[_Resource[Instant]])
      _ <- super.createEdge[Edge[S, E], Instant](
        id,
        createdEdge.asInstanceOf[_Resource[Edge[S, E]]],
        Property.default.`@createdon`,
        time
      )
    } yield createdEdge

  abstract override protected[lspace] def createValue[T](id: Long, value: T, dt: DataType[T]): Task[GValue[T]] =
    for {
      createdValue <- super.createValue(id, value, dt)
      id           <- idProvider.next
      time         <- createValue(id, Instant.now(), DateTimeType.datatype)
      _            <- edges.create(createdValue, Property.default.`@createdon`, time)
    } yield createdValue

  override protected[lspace] def deleteNode(node: _Node): Task[Unit] =
    for {
      deleteTime <- values.create(Instant.now(), DateTimeType.datatype)
      _          <- edges.create(node, Property.default.`@deletedon`, deleteTime)
    } yield ()

  override protected[lspace] def deleteEdge(edge: _Edge[_, _]): Task[Unit] =
    for {
      deleteTime <- values.create(Instant.now(), DateTimeType.datatype)
      _          <- edges.create(edge, Property.default.`@deletedon`, deleteTime)
    } yield ()

  override protected[lspace] def deleteValue(value: _Value[_]): Task[Unit] =
    for {
      deleteTime <- values.create(Instant.now(), DateTimeType.datatype)
      _          <- edges.create(value, Property.default.`@deletedon`, deleteTime)
    } yield ()
}
