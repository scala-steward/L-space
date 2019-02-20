package lspace.structure

import java.time.Instant

import lspace.datatype.{DataType, DateTimeType}

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

  override protected[lspace] def getOrCreateNode(id: Long): GNode = {
    val createdNode = super.getOrCreateNode(id)

    //TODO: make time configurable
    edges.create(createdNode,
                 Property.default.`@createdon`,
                 createValue(idProvider.next, Instant.now(), DateTimeType.datatype))

    createdNode
  }

  override protected def createEdge[S, E](id: Long,
                                          from: GResource[S],
                                          key: Property,
                                          to: GResource[E]): GEdge[S, E] = {
    val createdEdge = super.createEdge(id, from, key, to)

    super.createEdge[Edge[S, E], Instant](
      idProvider.next,
      createdEdge.asInstanceOf[GResource[Edge[S, E]]],
      Property.default.`@createdon`,
      createValue(idProvider.next, Instant.now(), DateTimeType.datatype).asInstanceOf[GResource[Instant]]
    )

    createdEdge
  }

  abstract override protected def createValue[T](id: Long, value: T, dt: DataType[T]): GValue[T] = {
    val createdValue = super.createValue(id, value, dt)

    edges.create(createdValue,
                 Property.default.`@createdon`,
                 createValue(idProvider.next, Instant.now(), DateTimeType.datatype))

    createdValue
  }

  override protected def deleteNode(node: GNode): Unit = {
    edges.create(node, Property.default.`@deletedon`, values.create(Instant.now(), DateTimeType.datatype))
  }

  override protected def deleteEdge(edge: GEdge[_, _]): Unit = {
    edges.create(edge, Property.default.`@deletedon`, values.create(Instant.now(), DateTimeType.datatype))
  }

  override protected def deleteValue(value: GValue[_]): Unit = {
    edges.create(value, Property.default.`@deletedon`, values.create(Instant.now(), DateTimeType.datatype))
  }
}
