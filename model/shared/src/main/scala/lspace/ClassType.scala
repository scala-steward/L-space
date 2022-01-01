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
    case Int    => IntTyped
    case Long   => LongTyped
    case String => StringTyped
    // case List[t] => ListType[List[t]]
  }
  def Able[X](x: X): Able[X] = x match {
    case _: Int    => IntType
    case _: Long   => LongType
    case _: String => StringType
    // case l: List[t] => ListType(l.map(Able).reduce(_ | _))
  }

case object ResourceType extends ResourceType
sealed trait ResourceType extends ClassType[Node | Edge[?, ?] | Value[?]] {
  val iri: Iri = Iri("@node|@edge|@value")
}
case object NodeType  extends NodeType
sealed trait NodeType extends ClassType[Node] { val iri: Iri = Iri("@node") }
final case class EdgeType[S, E](in: ClassType[S], out: ClassType[E]) extends ClassType[Edge[S, E]] {
  val iri: Iri = Iri(s"@edge(${in.iri},${out.iri})")
}
final case class ValueType[+V](dt: DataType[V]) extends ClassType[Value[V]] { val iri: Iri = Iri(s"@value(${dt.iri})") }

final case class Ontology(iri: Iri)     extends ClassType[Node]
final case class Property(iri: Iri)     extends ClassType[Edge[Any, Any]]
sealed trait DataType[+V](val iri: Iri) extends ClassType[V]

sealed trait NumericType[i] extends DataType[i]

type IntTyped = IntType[Int]
case object IntType            extends IntType[Int] with DataType[Int](Iri("@int"))
sealed trait IntType[i <: Int] extends NumericType[i]

type LongTyped = LongType[Long]
case object LongType             extends LongType[Long] with DataType[Long](Iri("@long"))
sealed trait LongType[i <: Long] extends NumericType[i]

// enum StringType[s <: String] extends DataType[s](Iri("@string")):
//   case string extends StringType[String]
//   case refined[s <: String]() extends StringType[s]

// object StringType:
//   type string = StringType[String]
//   def apply = string

type StringTyped = StringType[String]
case object StringType                 extends StringType[String] with DataType[String](Iri("@string"))
sealed trait StringType[s <: String]() extends DataType[s]

sealed trait StructuredType[+T] extends DataType[T]
sealed trait CollectionType[+T] extends StructuredType[T]

object ListType:
  type Able[X] = List[X]
  def apply[T](ct: ClassType[T]): ListType[Able[T]] = new ListType[Able[T]](ct) {}
abstract case class ListType[T] private (ct: ClassType[?])
    extends CollectionType[T]
    with DataType[T](Iri(s"@list(${ct.iri})"))

object SetType:
  type Able[X] = Set[X]
  def apply[T](ct: ClassType[T]): SetType[Able[T]] = new SetType[Able[T]](ct) {}
abstract case class SetType[T] private (ct: ClassType[?])
    extends CollectionType[T]
    with DataType[T](Iri(s"@list(${ct.iri})"))

object TupleType:
  type ClassTypesToTypes[X] <: Tuple = X match {
    case ClassType[c] *: EmptyTuple => c *: EmptyTuple
    case ClassType[c] *: classtypes => c *: ClassTypesToTypes[classtypes]
  }

  type IsClassType[X <: ClassType[?]] <: ClassType[?] = X match {
    case ClassType[?] => X
  }

  type ClassTypes[X] <: Tuple = X match {
    case classtype *: EmptyTuple => IsClassType[classtype] *: EmptyTuple
    case classtype *: classtypes => IsClassType[classtype] *: ClassTypes[classtypes]
  }

  def ClassTypes[X](x: X): ClassTypes[X] = (x match {
    case EmptyTuple                => EmptyTuple
    case (ct: ClassType[?]) *: cts => ct *: ClassTypes(cts)
  }).asInstanceOf[ClassTypes[X]]

  def apply[typeTuple](typeTuple: typeTuple): TupleType[TupleType.ClassTypesToTypes[typeTuple]] =
    new TupleType[TupleType.ClassTypesToTypes[typeTuple]](TupleType.ClassTypes(typeTuple)) {}

abstract case class TupleType[t <: Tuple](types: TupleType.ClassTypes[?])
    extends CollectionType[t]
    with DataType(Iri(s"@${types.toList.map { case c: ClassType[_] =>
      c.iri.unapply
    }.mkString}"))

object UnionType:
  type Able[X] = X match {
    case ClassType[t] *: EmptyTuple => t
    case ClassType[t] *: othertypes => t | Able[othertypes]
    case ClassType[t]               => t
  }
  def apply[T <: Tuple](types: T): UnionType[Able[T]] = new UnionType[Able[T]](types.toList.map {
    case ct: ClassType[?] => ct
  }.toSet) {}

abstract case class UnionType[+T](types: Set[ClassType[?]]) extends ClassType[T] { val iri: Iri = Iri("") }
