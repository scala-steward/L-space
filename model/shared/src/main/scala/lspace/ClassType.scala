package lspace

// import eu.timepit.refined.api.Refined
// import eu.timepit.refined.string.Uri

// enum ClassType[T]:
//   case ResourceType[T]() extends ClassType[Resource[T]]
//   case ValueType[T]() extends ClassType[Value[T]]
//   case EdgeType[S, E]() extends ClassType[Edge[S, E]]
//   case NodeType() extends ClassType[Node]
sealed trait ClassType[+C]
final case class ResourceType[+R]() extends ClassType[Resource[R]]
final case class NodeType()         extends ClassType[Node]
final case class EdgeType[+S, +E]() extends ClassType[Edge[S, E]]
final case class ValueType[+V]()    extends ClassType[Value[V]]

final case class Ontology(iri: Iri) extends ClassType[Node]
final case class Property(iri: Iri) extends ClassType[Edge[Any, Any]]
sealed trait DataType[+V](iri: Iri) extends ClassType[Value[V]]

object IntType                           extends IntType[Int](Iri("@int")) with DataType(Iri("@int"))
sealed trait IntType[i <: Int](iri: Iri) extends DataType[i]

object StringType                              extends StringType[String](Iri("@string")) with DataType(Iri("@string"))
sealed trait StringType[s <: String](iri: Iri) extends DataType[s]

sealed trait StructuredType[+T](iri: Iri) extends DataType[T]
sealed trait CollectionType[+T](iri: Iri) extends StructuredType[T]

type ClassTypeLeaf[X] = X match {
  case ct *: EmptyTuple => ct
  case ct *: othertypes => ct | ClassTypeLeaf[othertypes]
}
// object UnionType:
//   def apply[T](types: ClassTypeLeaf[T]): UnionType[ClassTypeLeaf[T]] = new UnionType(types) { val iri: Iri = Iri("") }
// end UnionType
sealed trait UnionType[+T](types: T) extends DataType[T]
