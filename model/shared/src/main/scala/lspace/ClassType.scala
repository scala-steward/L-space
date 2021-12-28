package lspace

// import eu.timepit.refined.api.Refined
// import eu.timepit.refined.string.Uri

// enum ClassType[T]:
//   case ResourceType[T]() extends ClassType[Resource[T]]
//   case ValueType[T]() extends ClassType[Value[T]]
//   case EdgeType[S, E]() extends ClassType[Edge[S, E]]
//   case NodeType() extends ClassType[Node]
sealed trait ClassType[+C] {
  def iri: Iri
}
object ClassType:
  type Apply[X] <: ClassType[?] = X match {
    case Int    => IntType[X]
    case Long   => LongType[X]
    case String => StringType[X]
  }

final case class ResourceType[+R](iri: Iri) extends ClassType[Resource[R]]
final case class NodeType(iri: Iri)         extends ClassType[Node]
final case class EdgeType[+S, +E](iri: Iri) extends ClassType[Edge[S, E]]
final case class ValueType[+V](iri: Iri)    extends ClassType[Value[V]]

final case class Ontology(iri: Iri)     extends ClassType[Node]
final case class Property(iri: Iri)     extends ClassType[Edge[Any, Any]]
sealed trait DataType[+V](val iri: Iri) extends ClassType[Value[V]]

object IntType                 extends IntType[Int] with DataType(Iri("@int"))
sealed trait IntType[i <: Int] extends DataType[i]

object LongType                  extends LongType[Long] with DataType(Iri("@long"))
sealed trait LongType[i <: Long] extends DataType[i]

object StringType                    extends StringType[String] with DataType(Iri("@string"))
sealed trait StringType[s <: String] extends DataType[s]

sealed trait StructuredType[+T] extends DataType[T]
sealed trait CollectionType[+T] extends StructuredType[T]

object TupleType:
  type ClassTypesToTypes[X] = X match {
    case ClassType[c] *: EmptyTuple => c *: EmptyTuple
    case ClassType[c] *: classtypes => c *: ClassTypesToTypes[classtypes]
  }

  type IsClassType[X <: ClassType[?]] <: ClassType[?] = X match {
    case ClassType[?] => X
  }

  type ClassTypes[X <: Tuple] <: Tuple = X match {
    case classtype *: EmptyTuple => IsClassType[classtype] *: EmptyTuple
    case classtype *: classtypes => IsClassType[classtype] *: ClassTypes[classtypes]
  }

final case class TupleType[t <: Tuple](t: TupleType.ClassTypes[t])
    extends CollectionType[t]
    with DataType(Iri(s"@${t.toList.map { case c: ClassType[_] =>
      c.iri.unapply
    }.mkString}"))

type ClassTypeLeaf[X] = X match {
  case ct *: EmptyTuple => ct
  case ct *: othertypes => ct | ClassTypeLeaf[othertypes]
}
// object UnionType:
//   def apply[T](types: ClassTypeLeaf[T]): UnionType[ClassTypeLeaf[T]] = new UnionType(types) { val iri: Iri = Iri("") }
// end UnionType
sealed trait UnionType[+T](types: T) extends DataType[T]
