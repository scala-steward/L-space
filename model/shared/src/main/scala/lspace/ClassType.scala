package lspace

// import eu.timepit.refined.api.Refined
// import eu.timepit.refined.string.Uri

// enum ClassType[T]:
//   case ResourceType[T]() extends ClassType[Resource[T]]
//   case ValueType[T]() extends ClassType[Value[T]]
//   case EdgeType[S, E]() extends ClassType[Edge[S, E]]
//   case NodeType() extends ClassType[Node]
sealed trait ClassType[+C] extends Matchable, Product, Serializable {
  def iri: Iri
}
object ClassType:
  type Able[X] <: ClassType[?] = X match {
    case Int    => IntType[X]
    case Long   => LongType[X]
    case String => StringType[X]
  }
  def Able[X](x: X): Able[X] = x match {
    case _: Int => IntType
    case _: Long => LongType
    case _: String => StringType
  }

object ResourceType                                                  extends ResourceType[Resource[Any]]
sealed trait ResourceType[+R <: Resource[R]]                         extends ClassType[R]
final case class NodeType(iri: Iri)                                  extends ClassType[Node]
final case class EdgeType[S, E](in: ClassType[S], out: ClassType[E]) extends ClassType[Edge[S, E]]
final case class ValueType[+V](iri: Iri)                             extends ClassType[Value[V]]

final case class Ontology(iri: Iri)     extends ClassType[Node]
final case class Property(iri: Iri)     extends ClassType[Edge[Any, Any]]
sealed trait DataType[+V](val iri: Iri) extends ClassType[Value[V]]

sealed trait NumericType[i] extends DataType[i]

object IntType                 extends IntType[Int] with DataType(Iri("@int"))
sealed trait IntType[i <: Int] extends NumericType[i]

object LongType                  extends LongType[Long] with DataType(Iri("@long"))
sealed trait LongType[i <: Long] extends NumericType[i]

object StringType                    extends StringType[String] with DataType(Iri("@string"))
sealed trait StringType[s <: String] extends DataType[s]

sealed trait StructuredType[+T] extends DataType[T]
sealed trait CollectionType[+T] extends StructuredType[T]

object ListType:
  type Able[X] = List[X]
  def apply[T](ct: ClassType[T]): ListType[Able[T]] = new ListType[Able[T]](ct)
class ListType[T] private (ct: ClassType[?]) extends CollectionType[T]

object SetType:
  type Able[X] = Set[X]
  def apply[T](ct: ClassType[T]): SetType[Able[T]] = new SetType[Able[T]](ct)
class SetType[T](ct: ClassType[?]) extends CollectionType[T]

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

object UnionType:
  type Able[X] = X match {
    case ClassType[t]              => t
    case ClassType[t] | othertypes => t | Able[othertypes]
  }
  def apply[T](types: Set[ClassType[?]]): UnionType[Able[T]] = new UnionType(types) { val iri: Iri = Iri("") }
end UnionType
sealed trait UnionType[+T](types: Set[ClassType[?]]) extends DataType[T]
