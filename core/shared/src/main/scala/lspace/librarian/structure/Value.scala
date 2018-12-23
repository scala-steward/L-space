package lspace.librarian.structure

import lspace.librarian.datatype.ValueURLType
import lspace.librarian.process.traversal.helper.ClassTypeable

object Value {
  val reservedKeys: Set[String] = Set(Property.default.`@type`.iri)

  implicit def default[T <: Value[_]]: ClassTypeable.Aux[T, T, ValueURLType[T]] = new ClassTypeable[T] {
    type C  = T
    type CT = ValueURLType[T]
    def ct: CT = ValueURLType.valueType[T]
  }
}

trait Value[+T] extends Resource[T] {

  def value: T

  def label: DataType[T] // = labels.collect { case tpe: DataType[T] => tpe }.head
  def labels: List[DataType[_]] = List(label)

  def remove(): Unit = graph.values.delete(this)

  override def equals(o: scala.Any): Boolean = o match {
    case resource: graph._Value[_] => sameResource(resource)
    case _                         => false
  }

  def equalValues(o: scala.Any): Boolean = o match {
    case resource: graph._Value[_] => resource.value == value
    case _                         => false
  }

  def prettyPrint: String = s"v:${if (iri.nonEmpty) iri else id.toString}:$value"
}
