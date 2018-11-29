package lspace.librarian.structure

import java.time.Instant

import lspace.librarian.datatype.DateTimeType

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

  abstract override protected def _createNode(id: Long)(ontology: Ontology*): _Node = {
    val createdNode = super._createNode(id)(ontology: _*)

    //TODO: make time configurable
    edges.create(createdNode,
                 Property.default.`@createdon`,
                 _createValue(idProvider.next)(Instant.now())(DateTimeType.datetimeType))

    createdNode
  }

  abstract override protected def _createEdge[S, E](
      id: Long)(from: _Resource[S], key: Property, to: _Resource[E]): _Edge[S, E] = {
    val createdEdge = super._createEdge(id)(from, key, to)

    _createEdge(idProvider.next)(createdEdge,
                                 Property.default.`@createdon`,
                                 _createValue(idProvider.next)(Instant.now())(DateTimeType.datetimeType))

    createdEdge
  }

  abstract override protected def _createValue[T](id: Long)(value: T)(dt: DataType[T]): _Value[T] = {
    val createdValue = super._createValue(id)(value)(dt)

    edges.create(createdValue,
                 Property.default.`@createdon`,
                 _createValue(idProvider.next)(Instant.now())(DateTimeType.datetimeType))

    createdValue
  }

  override protected def _deleteNode(node: _Node): Unit = {
    edges.create(node, Property.default.`@deletedon`, values.create(Instant.now(), DateTimeType.datetimeType))
  }

  override protected def _deleteEdge(edge: _Edge[_, _]): Unit = {
    edges.create(edge, Property.default.`@deletedon`, values.create(Instant.now(), DateTimeType.datetimeType))
  }

  override protected def _deleteValue(value: _Value[_]): Unit = {
    edges.create(value, Property.default.`@deletedon`, values.create(Instant.now(), DateTimeType.datetimeType))
  }
}
