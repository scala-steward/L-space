package lspace
package classtypes

import namespace._

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
    case Int     => ClassType[Int]
    case Double  => ClassType[Double]
    case Long    => ClassType[Long]
    case String  => ClassType[String]
    case Boolean => ClassType[Boolean]
    // case List[t] => ListType[List[t]]
  }
  trait Enabled[X]:
    def ct: ClassType[X]

  given intType: Enabled[Int] with
    def ct: ClassType[Int] = IntType.int

  given doubleType: Enabled[Double] with
    def ct: ClassType[Double] = DoubleType.double

  given longType: Enabled[Long] with
    def ct: ClassType[Long] = LongType.long

  given stringType: Enabled[String] with
    def ct: ClassType[String] = StringType.string

  def Able[X](x: X): Able[X] = x match {
    case _: Int     => IntType.int
    case _: Double  => DoubleType.double
    case _: Long    => LongType.long
    case _: String  => StringType.string
    case _: Boolean => BooleanType
    // case l: List[t] => ListType(l.map(Able).reduce(_ | _))
  }

type CTtoT[X] = X match
  case ClassType[t] => t

type AnyResource = ResourceType[Any]
case object AnyResource       extends ResourceType[Any] { val iri: Iri = Iri("@any") }
sealed trait ResourceType[+T] extends ClassType[T]      {
  // val iri: Iri = Iri("@node|@edge|@value")
}
case object NodeType  extends NodeType
sealed trait NodeType extends ResourceType[Node] { val iri: Iri = Iri("@node") }
type AnyEdge = EdgeType[Any, Any]
val AnyEdge: EdgeType[Any, Any] = EdgeType(AnyResource, AnyResource)
final case class EdgeType[S, E](in: ClassType[S], out: ClassType[E]) extends ResourceType[Edge[S, E]] {
  val iri: Iri = Iri(s"@edge(${in.iri},${out.iri})")
}
type AnyValue = ValueType[Any]
val AnyValue: ValueType[Any] = ValueType[Any](AnyDataType)
final case class ValueType[+V](dt: DataType[V]) extends ResourceType[Value[V]] {
  val iri: Iri = Iri(s"@value(${dt.iri})")
}

final case class Ontology(iri: Iri) extends ClassType[Node]
final case class Property(iri: Iri) extends ClassType[Edge[Any, Any]]

type AnyDataType = DataType[Any]
case object AnyDataType extends DataType[Any](Iri("@anydata"))
// object DataType:
//   type Types = Int | Double | Long | String | Boolean
//   // type Types[X] = X match {
//   //   case Int | Double | Long | String | Boolean => X
//   // }
//   type FilterTypes[X] = X match {
//     case Types
//   }
sealed trait DataType[+V](val iri: Iri) extends ClassType[V]

sealed trait NumericType[i] extends DataType[i]

type IntTyped = IntType[Int]
enum IntType[i <: Int](iri: Iri) extends NumericType[i] with DataType[i](iri):
  case int                           extends IntType[Int](`@int`)
  case literal[ii <: Int](value: ii) extends IntType[ii](`@int` ++ s"($value)")

type DoubleTyped = DoubleType[Double]
enum DoubleType[i <: Double](iri: Iri) extends NumericType[i] with DataType[i](iri):
  case double                           extends DoubleType[Double](`@double`)
  case literal[ii <: Double](value: ii) extends DoubleType[ii](`@double` ++ s"($value)")

type LongTyped = LongType[Long]
enum LongType[i <: Long](iri: Iri) extends NumericType[i] with DataType[i](iri):
  case long                           extends LongType[Long](`@long`)
  case literal[ii <: Long](value: ii) extends LongType[ii](`@long` ++ s"($value)")

// enum StringType[s <: String] extends DataType[s](Iri("@string")):
//   case string extends StringType[String]
//   case refined[s <: String]() extends StringType[s]

// object StringType:
//   type string = StringType[String]
//   def apply = string

type StringTyped = StringType[String]
// case object StringType                 extends StringType[String] with DataType[String](Iri("@string"))
// sealed trait StringType[s <: String]() extends DataType[s]
enum StringType[str <: String](iri: Iri) extends DataType[str](iri):
  case string                         extends StringType[String](`@string`)
  case literal[s <: String](value: s) extends StringType[s](`@string` ++ s"($value)")

object StringType:
  type l[s <: String] = literal[s]
  // def l[s <: String] = literal[s]
  val s = string

type BooleanTyped = BooleanType[Boolean]
case object BooleanType                  extends BooleanType[Boolean] with DataType[Boolean](Iri("@boolean"))
sealed trait BooleanType[b <: Boolean]() extends DataType[b]

sealed trait StructuredType[T] extends DataType[T]
sealed trait CollectionType[T] extends StructuredType[T]

// object ListType:
//   type Able[X] = List[X]
//   def apply[T](ct: ClassType[T]): ListType[Able[T]] = new ListType[Able[T]](ct) {}
// abstract case class ListType[T] private (ct: ClassType[?])
//     extends CollectionType[T]
//     with DataType[T](Iri(s"@list(${ct.iri})"))

type ListTyped = ListType[Any]
val AnyList: ListType[Any] = ListType.any
enum ListType[T](iri: Iri) extends CollectionType[List[T]] with DataType[List[T]](iri):
  case any                        extends ListType[Any](Iri(s"@list"))
  case typed[T](ct: ClassType[T]) extends ListType[T](Iri(s"@list(${ct.iri})"))

object ListType:
  def apply[T](ct: ClassType[T]): ListType[T] = ListType.typed(ct)

type SetTyped = SetType[Any]
val AnySet: SetType[Any] = SetType.any
enum SetType[T](iri: Iri) extends CollectionType[Set[T]] with DataType[Set[T]](iri):
  case any                        extends SetType[Any](Iri(s"@set"))
  case typed[T](ct: ClassType[T]) extends SetType[T](Iri(s"@set(${ct.iri})"))

object SetType:
  def apply[T](ct: ClassType[T]): SetType[T] = SetType.typed(ct)

object TupleType:
  type Types[X] <: Tuple = X match {
    case ClassType[c] *: EmptyTuple => c *: EmptyTuple
    case ClassType[c] *: classtypes => c *: Types[classtypes]
  }

  // type IsClassType[X] <: ClassType[?] = X match {
  //   case ClassType[t] => ClassType[t]
  // }

  type ClassTypes[X] <: Tuple = X match {
    case EmptyTuple                 => EmptyTuple
    case ClassType[t] *: classtypes => ClassType[t] *: ClassTypes[classtypes]
  }

  def ClassTypes[X](x: X): ClassTypes[X] = (x match {
    case EmptyTuple                => EmptyTuple
    case (ct: ClassType[?]) *: cts => ct *: ClassTypes(cts)
  }).asInstanceOf[ClassTypes[X]]

  def apply[typeTuple](typeTuple: typeTuple): TupleType[TupleType.Types[typeTuple]] =
    new TupleType[TupleType.Types[typeTuple]](TupleType.ClassTypes(typeTuple)) {}

abstract case class TupleType[t <: Tuple](types: TupleType.ClassTypes[?])
    extends CollectionType[t]
    with DataType[t](Iri(s"@tuple(${types.toList
        .map { case c: ClassType[_] =>
          c.iri.unapply
        }
        .mkString(",")})"))

object UnionType:
  type Able[X] = X match {
    case ClassType[t] *: EmptyTuple => t
    case ClassType[t] *: othertypes => t | Able[othertypes]
    case ClassType[t]               => t
  }
  def apply[T <: Tuple](types: T): UnionType[Able[T]] = new UnionType[Able[T]](types.toList.map {
    case ct: ClassType[?] => ct
  }.toSet) {}
// def unsafe[T](types: Set[ClassType[?]]): UnionType[T] =
//   new UnionType[T](types)

abstract case class UnionType[+T](types: Set[ClassType[?]]) extends ClassType[T] {
  val iri: Iri = Iri(s"@union(${types.toList.map(_.iri.unapply).sorted.mkString("|")})")
}
