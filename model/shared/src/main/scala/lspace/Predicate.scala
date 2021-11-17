package lspace

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
  val and     = And
  val or      = Or
  val prefix  = Prefix
  val suffix  = Suffix

  infix def ![predicate <: P[_]](predicate: predicate): Not[Not.NotType[predicate]] = Not(predicate)
end P

sealed trait P[+V](label: Name, comment: Comment) extends Matchable derives CanEqual

sealed trait EqP[+V]                extends P[V]
object Eqv:
  def apply[V](pvalue: V): Eqv[pvalue.type] = Eqv(pvalue)
end Eqv
final case class Eqv[+V] private(pvalue: V) extends P[V](Name("Eqv"), Comment("Predicate for logical equivalence, ===")), EqP[V]


object Neqv:
  def apply[V](pvalue: V): Neqv[pvalue.type] = Neqv(pvalue)
end Neqv
final case class Neqv[+V] private(pvalue: V)
    extends P[V](Name("Neqv"), Comment("Predicate for logical nonequivalence, !==")),
      EqP[V]

object OrderP:
  import java.time._

  type OrderableType[X] = X match
    case Int            => X
    case Double         => X
    case Long           => X
    case Instant        => X
    case ZonedDateTime  => X
    case OffsetDateTime => X
    case LocalDateTime  => X
    case LocalDate      => X
    case LocalTime      => X
end OrderP
sealed trait OrderP[+V] extends EqP[V]

object Gt:
  def apply[V](value: V): Gt[OrderP.OrderableType[V]] = Gt(value)
end Gt
final case class Gt[+V](pvalue: V)
    extends P[V](Name("Gt"), Comment("Predicate for greater than comparison, >")),
      OrderP[V]
object Gte:
  def apply[V](value: V): Gte[OrderP.OrderableType[V]] = Gte(value)
end Gte
final case class Gte[+V](pvalue: V)
    extends P[V](Name("Gte"), Comment("Predicate for greater than comparison or logical equivalence, >=")),
      OrderP[V]

object Lt:
  def apply[V](value: V): Lt[OrderP.OrderableType[V]] = Lt(value)
end Lt
final case class Lt[+V](pvalue: V) extends P[V](Name("Lt"), Comment("Predicate for less than comparison, <")), OrderP[V]

object Lte:
  def apply[V](value: V): Lte[OrderP.OrderableType[V]] = Lte(value)
end Lte
final case class Lte[+V](pvalue: V)
    extends P[V](Name("Lte"), Comment("Predicate for less than comparison or logical equivalence, <=")),
      OrderP[V]

object Inside:
  def apply[V](lower: V, upper: V): Inside[OrderP.OrderableType[V]] = Inside(lower, upper)
end Inside
final case class Inside[+V](lower: V, upper: V)
    extends P[V](Name("Inside"), Comment("Predicate for greater than and less than comparison, > && <")),
      OrderP[V]

object Outside:
  def apply[V](lower: V, upper: V): Outside[OrderP.OrderableType[V]] = Outside(lower, upper)
end Outside
final case class Outside[+V](lower: V, upper: V)
    extends P[V](Name("Outside"), Comment("Predicate for less than and greater than comparison, < && >")),
      OrderP[V]

object Between:
  def apply[V](lower: V, upper: V): Between[OrderP.OrderableType[V]] = Between(lower, upper)
end Between
final case class Between[+V](lower: V, upper: V)
    extends P[V](Name("Between"), Comment("Predicate for greater than equal and less than equal comparison, >= && <=")),
      OrderP[V]

sealed trait CollectionP[+V] extends P[V]
// sealed trait SeqP[+V]        extends P[V]
final case class Within[+V](value: V*)
    extends P[V](Name("Within"), Comment("Predicate for .., contains")),
      CollectionP[V]
final case class OneOf[+V](value: V*) extends P[V](Name("OneOf"), Comment("Predicate for .., oneOf")), CollectionP[V]
final case class AnyOf[+V](value: V*) extends P[V](Name("AnyOf"), Comment("Predicate for .., oneOf")), CollectionP[V]
final case class AllOf[+V](value: V*) extends P[V](Name("AllOf"), Comment("Predicate for .., oneOf")), CollectionP[V]

object And:
  def apply[predicates <: Tuple](predicates: predicates): And[AndTuple[predicates]] = And(predicates)

  type AndTuple[X] <: Tuple = X match
    case EmptyTuple => EmptyTuple
    case x *: xs    => Tuple.Concat[PType[x], AndTuple[xs]]
    case _          => PType[X]

  def andTuple[X](x: X): AndTuple[X] = (x match
    case EmptyTuple => Tuple.apply()
    case x *: xs    => (ptype(x) ++ andTuple(xs))
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
  def apply[predicates <: Tuple](predicates: predicates): Or[OrTuple[predicates]] = Or(predicates)

  type OrTuple[X] <: Tuple = X match
    case EmptyTuple => EmptyTuple
    case x *: xs    => Tuple.Concat[PType[x], OrTuple[xs]]
    case _          => PType[X]

  def orTuple[X](x: X): OrTuple[X] = (x match
    case EmptyTuple => Tuple.apply()
    case x *: xs    => (ptype(x) ++ orTuple(xs))
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
end Not
final case class Not[V](value: V) extends P[Not.NotType[V]](Name("Not"), Comment("Predicate for .., !"))

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
