package lspace.librarian.structure

import java.time.Instant

import lspace.librarian.datatype.DateTimeType

object History {
  val historyKeys = Set(Property.default.createdon,
                        Property.default.deletedon,
                        Property.default.modifiedon,
                        Property.default.transcendedOn)
}

/**
  * The History-trait ensures data is never deleted but annotated with '@created', '@modified', '@deleted' and '@trancended' tags.
  */
trait History extends Graph {

  override def createNode(ontology: Ontology*): Node = {
    val createdNode = _createNode(ontology: _*)

    //TODO: make time configurable
    createEdge(createdNode, Property.default.createdon, _createValue(Instant.now())(DateTimeType.datetimeType))

    createdNode
  }

  override def createEdge[S, E](from: Resource[S], key: Property, to: Resource[E]): Edge[S, E] = {
    val createdEdge = _createEdge(from, key, to)

    _createEdge(createdEdge, Property.default.createdon, _createValue(Instant.now())(DateTimeType.datetimeType))

    createdEdge
  }

  override def createValue[T](value: T)(dt: DataType[T]): Value[T] = {
    val createdValue = _createValue(value)(dt)

    createEdge(createdValue, Property.default.createdon, _createValue(Instant.now())(DateTimeType.datetimeType))

    createdValue
  }

  override def deleteNode(node: Node): Unit = {
    createEdge(node, Property.default.deletedon, createValue(Instant.now())(DateTimeType.datetimeType))
  }

  override def deleteEdge(edge: Edge[_, _]): Unit = {
    createEdge(edge, Property.default.deletedon, createValue(Instant.now())(DateTimeType.datetimeType))
  }

  override def deleteValue(value: Value[_]): Unit = {
    createEdge(value, Property.default.deletedon, createValue(Instant.now())(DateTimeType.datetimeType))
  }
}
