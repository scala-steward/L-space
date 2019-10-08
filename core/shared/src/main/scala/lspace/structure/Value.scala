package lspace.structure

import lspace.datatype.{DataType, ValueURLType}
import lspace.librarian.traversal.{step, Traversal}
import lspace.structure.util.ClassTypeable
import monix.eval.Task
import shapeless.{::, HNil}

object Value {
  val reservedKeys: Set[String] = Set(Property.default.`@type`.iri)

  implicit def default[T <: Value[Any]]: ClassTypeable.Aux[T, T, ValueURLType[T]] = new ClassTypeable[T] {
    type C  = T
    type CT = ValueURLType[T]
    def ct: CT = ValueURLType.apply[T]
  }

  implicit class WithValue[T, OutC, Out <: ClassType[OutC]](value: Value[T])(
      implicit cls: ClassTypeable.Aux[T, OutC, Out]) {
    def g: Traversal[ClassType[Any], Out, step.V :: HNil] = lspace.g.V(value.value)
  }
}

trait Value[+T] extends Resource[T] {

  def value: T

  def label: DataType[T] // = labels.collect { case tpe: DataType[T] => tpe }.head
  def labels: List[DataType[T]] = List(label)

  override def hasLabel[L](label: ClassType[L]*): Option[Value[L]] = {
    super.hasLabel(label: _*).asInstanceOf[Option[Value[L]]]
  }

  def remove(): Task[Unit] = graph.values.delete(this)

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
