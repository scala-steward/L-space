package lspace
package librarian
package logic

import classtypes._

object P:
  val eqv     = Eqv
  val neqv    = Neqv
  val gt      = Gt
  val gte     = Gte
  val lt      = Lt
  val lte     = Lte
  val inside  = Inside
  val outside = Outside
  val between = Between
  val within  = Within
  val oneOf   = OneOf
  val anyOf   = AnyOf
  val allOf   = AllOf
  val noneOf  = NoneOf
  val and     = And
  val or      = Or
  val not     = Not
  val prefix  = Prefix
  val suffix  = Suffix
  val matches = Matches

  infix def ![p <: P[?]](p: p): Not[p] = Not(p)
end P

sealed trait P[+V](label: Name, comment: Comment) extends Matchable derives CanEqual

sealed trait EqP[+V] extends P[V]
object Eqv:
  def apply[V](pvalue: V): Eqv[pvalue.type] = new Eqv(pvalue)
end Eqv
final case class Eqv[+V] private (pvalue: V)
    extends P[V](Name("Eqv"), Comment("Predicate for logical equivalence, ===")),
      EqP[V]

object Neqv:
  def apply[V](pvalue: V): Neqv[pvalue.type] = new Neqv(pvalue)
end Neqv
final case class Neqv[+V] private (pvalue: V)
    extends P[V](Name("Neqv"), Comment("Predicate for logical nonequivalence, !==")),
      EqP[V]

object OrderP:
  import java.time._

  type OrderableType[X] = X match
    case Int | Double | Long     => X
    // case Instant | ZonedDateTime => X
    // case OffsetDateTime          => X
    // case LocalDateTime           => X
    // case LocalDate               => X
    // case LocalTime               => X

  def OrderableType[X](x: X): OrderableType[X] = x match {
    case x: (Int | Double | Long)     => x
    // case x: (Instant | ZonedDateTime) => x
    // case x: OffsetDateTime            => x
    // case x: LocalDateTime             => x
    // case x: LocalDate                 => x
    // case x: LocalTime                 => x
  }

  type OrderableClassType[X] <: ClassType[?] = X match {
    case IntType[t] => IntType[t]
    case DoubleType[t] => DoubleType[t]
    case LongType[t] => LongType[t]
  }
  def OrderableClassType[X](x: X): OrderableClassType[X] = x match {
    case ct: IntType[?] => ct
    case ct: DoubleType[?] => ct
    case ct: LongType[?] => ct
  }
// type Sortable[X] = X match {
//   case Int | Double | Long => X
//   case String => X
//   case java.time.Instant | java.time.ZonedDateTime => java.time.Instant | java.time.ZonedDateTime
// }
// implicitly[(Int | String) =:= (Sortable[Int | String])]
// implicitly[(Int) =:= (Sortable[Int])]
// implicitly[(Int | Double) =:= (Sortable[Int | Double])]
// implicitly[(Int | Double | Long) =:= (Sortable[Int | Double | Long])]
// implicitly[(Int | Double | Long | BigInt) =:= (Sortable[Int | Double | Long | BigInt])]

end OrderP
sealed trait OrderP[+V] extends EqP[V]

object Gt:
  def apply[V](value: V): Gt[OrderP.OrderableType[value.type]] = new Gt(OrderP.OrderableType(value))
end Gt
final case class Gt[+V] private (pvalue: V)
    extends P[V](Name("Gt"), Comment("Predicate for greater than comparison, >")),
      OrderP[V]
object Gte:
  def apply[V](value: V): Gte[OrderP.OrderableType[value.type]] = new Gte(OrderP.OrderableType(value))
end Gte
final case class Gte[+V] private (pvalue: V)
    extends P[V](Name("Gte"), Comment("Predicate for greater than comparison or logical equivalence, >=")),
      OrderP[V]

object Lt:
  def apply[V](value: V): Lt[OrderP.OrderableType[value.type]] = new Lt(OrderP.OrderableType(value))
end Lt
final case class Lt[+V] private (pvalue: V)
    extends P[V](Name("Lt"), Comment("Predicate for less than comparison, <")),
      OrderP[V]

object Lte:
  def apply[V](value: V): Lte[OrderP.OrderableType[value.type]] = new Lte(OrderP.OrderableType(value))
end Lte
final case class Lte[+V] private (pvalue: V)
    extends P[V](Name("Lte"), Comment("Predicate for less than comparison or logical equivalence, <=")),
      OrderP[V]

object Inside:
  def apply[L, U](lower: L, upper: U): Inside[OrderP.OrderableType[lower.type], OrderP.OrderableType[upper.type]] =
    new Inside(OrderP.OrderableType(lower), OrderP.OrderableType(upper))
end Inside
final case class Inside[L, U] private (lower: L, upper: U)
    extends P[(L, U)](Name("Inside"), Comment("Predicate for greater than and less than comparison, > && <")),
      OrderP[(L, U)]

object Outside:
  def apply[L, U](lower: L, upper: U): Outside[OrderP.OrderableType[lower.type], OrderP.OrderableType[upper.type]] =
    new Outside(OrderP.OrderableType(lower), OrderP.OrderableType(upper))
end Outside
final case class Outside[L, U] private (lower: L, upper: U)
    extends P[(L, U)](Name("Outside"), Comment("Predicate for less than and greater than comparison, < && >")),
      OrderP[(L, U)]

object Between:
  def apply[L, U](lower: L, upper: U): Between[OrderP.OrderableType[lower.type], OrderP.OrderableType[upper.type]] =
    new Between(OrderP.OrderableType(lower), OrderP.OrderableType(upper))
end Between
final case class Between[L, U] private (lower: L, upper: U)
    extends P[(L, U)](
      Name("Between"),
      Comment("Predicate for greater than equal and less than equal comparison, >= && <=")
    ),
      OrderP[(L, U)]

sealed trait CollectionP[+V] extends P[V]
// sealed trait SeqP[+V]        extends P[V]
final case class Within[+V] private (value: V*)
    extends P[V](Name("Within"), Comment("Predicate for .., contains")),
      CollectionP[V]

object OneOf:
  type TupleType[X] <: Tuple = X match
    case EmptyTuple      => EmptyTuple
    case value *: values => Able[value] *: TupleType[values]
  def TupleType[X](x: X): TupleType[X] = (x match
    case EmptyTuple      => EmptyTuple
    case value *: values => Able(value) *: TupleType(values)
  ).asInstanceOf[TupleType[X]]

  type Able[X] = X match {
    case Int    => X & Int
    case Double => X & Double
    case Long   => X & Long
  }
  def Able[X](x: X): Able[X] = (x match {
    case _: Int    => x
    case _: Double => x
    case _: Long   => x
  }).asInstanceOf[Able[X]]

  def apply[values](values: values): OneOf[TupleType[values]] = new OneOf(TupleType(values))

final case class OneOf[values] private (value: values)
    extends P[values](Name("OneOf"), Comment("Predicate for .., oneOf")),
      CollectionP[values]
final case class AnyOf[values] private (value: values)
    extends P[values](Name("AnyOf"), Comment("Predicate for .., oneOf")),
      CollectionP[values]
final case class AllOf[values] private (value: values)
    extends P[values](Name("AllOf"), Comment("Predicate for .., oneOf")),
      CollectionP[values]
final case class NoneOf[values] private (value: values)
    extends P[values](Name("AllOf"), Comment("Predicate for .., oneOf")),
      CollectionP[values]

object And:
  def apply[predicates <: Tuple](predicates: predicates): And[AndTuple[predicates]] = new And(AndTuple(predicates))

  type AndTuple[X] <: Tuple = X match
    case EmptyTuple => EmptyTuple
    case x *: xs    => Tuple.Concat[PType[x], AndTuple[xs]]
    case _          => PType[X]

  def AndTuple[X](x: X): AndTuple[X] = (x match
    case EmptyTuple => Tuple.apply()
    case x *: xs    => (ptype(x) ++ AndTuple(xs))
    case x          => ptype(x)
  ).asInstanceOf[AndTuple[X]]

  type PType[X] <: Tuple = X match
    case And[predicates] => predicates
    case P[t]            => Tuple1[X]

  def ptype[X](x: X): PType[X] = x match
    case x: And[_] => x.predicates
    case x: P[_]   => Tuple1(x)

  type AndType[X <: Tuple] = X match
    case P[t]       => t
    case EmptyTuple => Nothing
    case x *: xs    => AndType[x] | AndType[xs]

end And
final case class And[predicates <: Tuple] private (predicates: predicates)
    extends P[And.AndType[predicates]](Name("And"), Comment("Predicate for logical conjunction"))

object Or:
  def apply[predicates <: Tuple](predicates: predicates): Or[OrTuple[predicates]] = new Or(OrTuple(predicates))

  type OrTuple[X] <: Tuple = X match
    case EmptyTuple => EmptyTuple
    case x *: xs    => Tuple.Concat[PType[x], OrTuple[xs]]
    case _          => PType[X]

  def OrTuple[X](x: X): OrTuple[X] = (x match
    case EmptyTuple => Tuple.apply()
    case x *: xs    => (ptype(x) ++ OrTuple(xs))
    case x          => ptype(x)
  ).asInstanceOf[OrTuple[X]]

  type PType[X] <: Tuple = X match
    case Or[predicates] => predicates
    case P[t]           => Tuple1[X]

  def ptype[X](x: X): PType[X] = x match
    case x: Or[_] => x.predicates
    case x: P[_]  => Tuple1(x)

  type OrType[X <: Tuple] = X match
    case P[t]       => t
    case EmptyTuple => Nothing
    case x *: xs    => OrType[x] | OrType[xs]

end Or
final case class Or[predicates <: Tuple] private (predicates: predicates)
    extends P[Or.OrType[predicates]](Name("Or"), Comment("Predicate for logical disjunction"))

object Not:
  type NotType[X] = X match
    case P[t] => t

  def apply[p <: P[?]](p: p): Not[p] = new Not(p)
end Not
final case class Not[p <: P[?]] private (p: p) extends P[p](Name("Not"), Comment("Predicate for .., !"))

object Prefix:
  type PrefixType[X] = X match
    case String  => X
    case Seq[_]  => X
    case Product => X
end Prefix
final case class Prefix[V](value: V) extends P[Prefix.PrefixType[V]](Name("Prefix"), Comment("Predicate for .., !"))

object Suffix:
  type SuffixType[X] = X match
    case String  => X
    case Seq[_]  => X
    case Product => X
end Suffix
final case class Suffix[V](value: V) extends P[Suffix.SuffixType[V]](Name("Suffix"), Comment("Predicate for .., !"))

object Matches:

  def apply[regex <: String](value: regex): Matches[value.type] = new Matches[value.type](value)
final case class Matches[V] private (value: V)
    extends P[String](Name("Matches"), Comment("Predicate for matching against a regular expression."))
