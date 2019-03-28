package lspace.structure

import java.time.Instant

import lspace.datatype.{DataType, DateTimeType}
import monix.eval.Task

object History {
  val historyKeys = Set(Property.default.`@createdon`,
                        Property.default.`@deletedon`,
                        Property.default.`@modifiedon`,
                        Property.default.`@transcendedon`)
}

/**
  * The History-trait ensures data is never deleted but annotated with '@created', '@modified', '@deleted' and '@trancended' tags.
  */
trait History extends Graph {

  override protected[lspace] def getOrCreateNode(id: Long): Task[GNode] = {
    for {
      createdNode <- super.getOrCreateNode(id)
      value       <- createValue(id, Instant.now(), DateTimeType.datatype)
      id          <- idProvider.next
      //TODO: make time configurable
      edge <- edges.create(createdNode, Property.default.`@createdon`, value)
    } yield createdNode
  }

  override protected def createEdge[S, E](id: Long,
                                          from: GResource[S],
                                          key: Property,
                                          to: GResource[E]): Task[GEdge[S, E]] = {
    for {
      createdEdge <- super.createEdge(id, from, key, to)
      id          <- idProvider.next
      time        <- createValue(id, Instant.now(), DateTimeType.datatype).map(_.asInstanceOf[GResource[Instant]])
      timeEdge <- super.createEdge[Edge[S, E], Instant](id,
                                                        createdEdge.asInstanceOf[GResource[Edge[S, E]]],
                                                        Property.default.`@createdon`,
                                                        time)
    } yield createdEdge
  }

  abstract override protected def createValue[T](id: Long, value: T, dt: DataType[T]): Task[GValue[T]] = {
    for {
      createdValue <- super.createValue(id, value, dt)
      id           <- idProvider.next
      time         <- createValue(id, Instant.now(), DateTimeType.datatype)
      timeEdge     <- edges.create(createdValue, Property.default.`@createdon`, time)
    } yield createdValue
  }

  override protected def deleteNode(node: GNode): Task[Unit] =
    for {
      deleteTime     <- values.create(Instant.now(), DateTimeType.datatype)
      deleteTimeEdge <- edges.create(node, Property.default.`@deletedon`, deleteTime)
    } yield Unit

  override protected def deleteEdge(edge: GEdge[_, _]): Task[Unit] =
    for {
      deleteTime     <- values.create(Instant.now(), DateTimeType.datatype)
      deleteTimeEdge <- edges.create(edge, Property.default.`@deletedon`, deleteTime)
    } yield Unit

  override protected def deleteValue(value: GValue[_]): Task[Unit] =
    for {
      deleteTime     <- values.create(Instant.now(), DateTimeType.datatype)
      deleteTimeEdge <- edges.create(value, Property.default.`@deletedon`, deleteTime)
    } yield Unit
}
