package lspace.librarian.logic.predicate

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import lspace.datatype.DataType
import lspace.structure._
import lspace.structure.util.ClassTypeable
import monix.eval.Task

object P extends OntologyDef(lspace.NS.vocab.Lspace + "librarian/P", label = "P", comment = "Predicate ontology") {

  object keys

//  implicit def nodeToP(node: Node): P[_] = P.toP(node)

  //TODO: migrate to Task[P[_]], this way we can start building resolvers for remote nodes (reusing published predicates)
  def toP(node: Node): P[_] = node match {
    case p: P[_] => p
    case _ =>
      node.labels match {
        case types if types.contains(And.ontology)       => And.toP(node)
        case types if types.contains(Or.ontology)        => Or.toP(node)
        case types if types.contains(Eqv.ontology)       => Eqv.toP(node)
        case types if types.contains(Neqv.ontology)      => Neqv.toP(node)
        case types if types.contains(Gt.ontology)        => Gt.toP(node)
        case types if types.contains(Gte.ontology)       => Gte.toP(node)
        case types if types.contains(Lt.ontology)        => Lt.toP(node)
        case types if types.contains(Lte.ontology)       => Lte.toP(node)
        case types if types.contains(Between.ontology)   => Between.toP(node)
        case types if types.contains(Outside.ontology)   => Outside.toP(node)
        case types if types.contains(Inside.ontology)    => Inside.toP(node)
        case types if types.contains(Intersect.ontology) => Intersect.toP(node)
        case types if types.contains(Within.ontology)    => Within.toP(node)
        //        case types if types.contains(Without.ontology)        => Without.toP(node)
        case types if types.contains(Disjoint.ontology)       => Disjoint.toP(node)
        case types if types.contains(Contains.ontology)       => Contains.toP(node)
        case types if types.contains(Prefix.ontology)         => Prefix.toP(node)
        case types if types.contains(Suffix.ontology)         => Suffix.toP(node)
        case types if types.contains(Regex.ontology)          => Regex.toP(node)
        case types if types.contains(Fuzzy.ontology)          => Fuzzy.toP(node)
        case types if types.contains(ContainsPrefix.ontology) => ContainsPrefix.toP(node)
        case types if types.contains(ContainsRegex.ontology)  => ContainsRegex.toP(node)
        case types if types.contains(ContainsFuzzy.ontology)  => ContainsFuzzy.toP(node)
        case types =>
          throw new Exception(s"No valid P-ontology found for types ${types}")
      }
  }

  lazy val predicates: List[PredicateDef] = List(
    And,
    Or,
    Eqv,
    Neqv,
    Gt,
    Gte,
    Lt,
    Lte,
    Between,
    Outside,
    Inside,
    Intersect,
    Within,
    Disjoint,
    Contains,
    Prefix,
    Suffix,
    Regex,
    Fuzzy,
    ContainsPrefix,
    ContainsRegex,
    ContainsFuzzy
  )

  trait OrderHelper[T]
  object OrderHelper {
    implicit object int           extends OrderHelper[Int]
    implicit object double        extends OrderHelper[Double]
    implicit object long          extends OrderHelper[Long]
    implicit object instant       extends OrderHelper[Instant]
    implicit object localdatetime extends OrderHelper[LocalDateTime]
    implicit object localdate     extends OrderHelper[LocalDate]
    implicit object localtime     extends OrderHelper[LocalTime]
  }

  def &&[T, PR[+Z] <: P[Z]](predicate: PR[T]*): And = new And(predicate.toList)
  def ||[T, PR[+Z] <: P[Z]](predicate: PR[T]*): Or  = Or(predicate.toList)
  def eqv[T, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Eqv[T] =
    Eqv(value)
  def neqv[T, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Neqv[T] =
    Neqv(value)
  def gt[T: OrderHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Gt[T] =
    Gt(value)
  def gte[T: OrderHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Gte[T] =
    Gte(value)
  def lt[T: OrderHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Lt[T] =
    Lt(value)
  def lte[T: OrderHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Lte[T] =
    Lte(value)
  //  def between[T](range: QuantityRange[T]): P[T] =Between(range.lower, range.upper)
  def between[T: OrderHelper, T0, TT0 <: ClassType[_]](lower: T, upper: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Between[T] = Between(lower, upper)
  def outside[T: OrderHelper, T0, TT0 <: ClassType[_]](lower: T, upper: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Outside[T] = Outside(lower, upper)
  def inside[T: OrderHelper, T0, TT0 <: ClassType[_]](lower: T, upper: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Inside[T] = Inside(lower, upper)
  //  def within(values: List[Any]): P[Any] = within(values.toSet)
  def within[T, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Within[T] =
    Within(value)
  //  def within[T: ObjectHelper](value: T): GeoWithin[T] =GeoWithin(value)
  //  def without(values: List[Any]): P[Any] = without(values.toSet)
  //  def without[T: CollectionHelper](value: T, values: T*): Without[T] =Without(value :: values.toList)
  def intersect[T, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Intersect[T] =
    Intersect(value)
  def disjoint[T, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Disjoint[T] =
    Disjoint(value)
  //  def contains[T: ObjectHelper](value: T): GeoContains[T] =GeoContains(value)
  def contains[T](value: T): Contains[T] =
    Contains(value)
  def contains[T, PR[Z] <: P[T]](value: PR[T]): Contains[PR[T]] =
    Contains(value) //TODO ...
  def prefix[T, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Prefix[T] =
    Prefix(value)
  def startsWith[T, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Prefix[T] =
    Prefix(value)
  def suffix[T, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Suffix[T] =
    Suffix(value)
  def endsWith[T, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Suffix[T] =
    Suffix(value)
  def regex(value: scala.util.matching.Regex): Regex = Regex(value)
  def fuzzy[T, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Fuzzy[T] =
    Fuzzy(value)

  import shapeless.=:!=
  implicit class WithPredicate[T <: P[_]](_predicate: T)(implicit ev: T =:!= And, ev2: T =:!= Or) {
    def &&[T0, PR0[Z] <: P[Z], T1, TT1 <: ClassType[_]](predicate: PR0[T0])(
        implicit ct: ClassTypeable.Aux[T0, T1, TT1]): And = new And(_predicate :: predicate :: Nil)
    def ||[T0, PR0[Z] <: P[Z], T1, TT1 <: ClassType[_]](predicate: PR0[T0])(
        implicit ct: ClassTypeable.Aux[T0, T1, TT1]): Or = Or(_predicate :: predicate :: Nil)
  }

  implicit def clsP[T]: ClassTypeable.Aux[P[T], Node, ClassType[Node]] = new ClassTypeable[P[T]] {
    type C  = Node
    type CT = ClassType[Node]
    def ct: CT = P.ontology
  }
}

trait P[+T] extends Product with Serializable {
  def _pvalue: Any
  def toNode: Task[Node]
  def prettyPrint: String
}

trait EqP[+T] extends P[T] {
  def _pvalue: Any = pvalue
  def pvalue: T
}
object EqP
    extends PredicateDef(label = "EqP", comment = "Equality/comparable predicate", `@extends` = () => P.ontology :: Nil) {

  object keys extends P.Properties {
    object value
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/p/value",
          "value",
          "Any value",
          `@range` = () => Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil
        ) {}
  }

  override lazy val properties: List[Property] = keys.value.property :: P.properties

  trait Properties extends P.Properties {
    lazy val value: Property = keys.value
  }
}
trait OrderP[+T] extends EqP[T]
object OrderP
    extends PredicateDef(label = "OrderP", comment = "Order/sortable predicate", `@extends` = () => EqP.ontology :: Nil) {

  object keys extends EqP.Properties
  override lazy val properties: List[Property] = EqP.properties

  trait Properties extends EqP.Properties
}
trait RangeP[+T] extends P[T] {
  def _pvalue: Any = lower -> upper
  def lower: T
  def upper: T
}
object RangeP
    extends PredicateDef(label = "RangeP", comment = "Range predicate", `@extends` = () => P.ontology :: Nil) {

  object keys extends P.Properties {
    object lower
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/p/range/lower",
          "lower",
          "Any value"
        ) {}
    object upper
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/p/range/upper",
          "upper",
          "Any value"
        ) {}
  }

  override lazy val properties: List[Property] = keys.lower.property :: keys.upper.property :: P.properties

  trait Properties extends P.Properties {
    lazy val lower: Property = keys.lower
    lazy val upper: Property = keys.upper
  }
}
//trait ObjectP[T] extends P[T]
//object ObjectP
//    extends PredicateDef(label = "ObjectP", comment = "Object predicate", `@extends` = () => EqP.ontology :: Nil) {
//
//  object keys extends EqP.Properties
//  override lazy val properties: List[Property] = EqP.properties
//
//  trait Properties extends EqP.Properties
//}
trait CollectionP[+T] extends P[T] {
  def _pvalue: Any = pvalue
  def pvalue: T
}
object CollectionP
    extends PredicateDef(label = "CollectionP", comment = "Collection predicate", `@extends` = () => P.ontology :: Nil) {

  object keys extends P.Properties {
    object value
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/p/collection/value",
          "values",
          "Polyglot list of values"
        ) {}
  }
  override lazy val properties: List[Property] = keys.value.property :: P.properties

  trait Properties extends P.Properties {
    lazy val value: Property = keys.value
  }
}
trait SeqP[+T] extends CollectionP[T]
object SeqP
    extends PredicateDef(label = "SeqP", comment = "Sequence predicate", `@extends` = () => CollectionP.ontology :: Nil) {

  object keys extends CollectionP.Properties
  override lazy val properties: List[Property] = CollectionP.properties
  trait Properties extends CollectionP.Properties
}
