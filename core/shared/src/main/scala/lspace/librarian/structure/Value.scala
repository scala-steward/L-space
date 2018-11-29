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

trait Value[T] extends Resource[T] {
  //TOEXPLORE: Linking to properties e.g. noderesource-uri # property-name

  def value: T

  /**
    * TODO: try to work without a datatype and have a dedicated Scala-/Java-type for each DataType
    */
  def label: DataType[T] // = labels.collect { case tpe: DataType[T] => tpe }.head
  def labels: List[DataType[_]] = List(label)
  //    out(graph.TYPE).collect { case node: Node => node }.map(DataType.wrap)

//  override def equals(o: scala.Any): Boolean = o match {
//    case resource: Value[_] => value == resource.value
//    case _                  => value == o
//  }

  //  override def start() = Traversal[T, T, step.V, HNil, HNil](step.V(List(this)))(graph, Structure(HNil), LabelsHList(HNil))

  override def hashCode(): Int = value.hashCode()

  def remove(): Unit = graph.values.delete(this)
}
