package lspace.librarian.structure

object TypedProperty {
  implicit def kvToTypedPropertyKey[T](kv: (Property, ClassType[T])): TypedProperty[T] =
    TypedProperty(kv._1, kv._2)
}
case class TypedProperty[T](key: Property, range: ClassType[T]) {
  val iri: String = key.iri + "/" + range.iri
}
