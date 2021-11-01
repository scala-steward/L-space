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

object IntType:

end IntType
sealed trait IntType[i <: Int] extends ClassType[Value[i]]
