package lspace.librarian.process.traversal

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import lspace.librarian.process.traversal.p._
import lspace.librarian.structure._
import lspace.librarian.datatype._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.Ontology.OntologyDef
import lspace.librarian.util.AssertionNotSupported
import lspace.types.vector.Geometry
import shapeless.{::, HList, HNil, LUBConstraint}
import squants.{Quantity, QuantityRange}

import scala.collection.immutable.ListSet

object P extends OntologyDef(lspace.NS.vocab.Lspace + "librarian/P", label = "P", comment = "Predicate ontology") {

  object keys

  implicit def nodeToP(node: Node): P[_] = P.toP(node)

  def toP(node: Node): P[_] = node match {
    case p: P[_] => p
    case _ =>
      node.labels match {
        case types if types.contains(p.And.ontology)       => p.And.toP(node)
        case types if types.contains(p.Or.ontology)        => p.Or.toP(node)
        case types if types.contains(p.Eqv.ontology)       => p.Eqv.toP(node)
        case types if types.contains(p.Neqv.ontology)      => p.Neqv.toP(node)
        case types if types.contains(p.Gt.ontology)        => p.Gt.toP(node)
        case types if types.contains(p.Gte.ontology)       => p.Gte.toP(node)
        case types if types.contains(p.Lt.ontology)        => p.Lt.toP(node)
        case types if types.contains(p.Lte.ontology)       => p.Lte.toP(node)
        case types if types.contains(p.Between.ontology)   => p.Between.toP(node)
        case types if types.contains(p.Outside.ontology)   => p.Outside.toP(node)
        case types if types.contains(p.Inside.ontology)    => p.Inside.toP(node)
        case types if types.contains(p.Intersect.ontology) => p.Intersect.toP(node)
        case types if types.contains(p.Within.ontology)    => p.Within.toP(node)
//        case types if types.contains(p.Without.ontology)        => p.Without.toP(node)
        case types if types.contains(p.Disjoint.ontology)       => p.Disjoint.toP(node)
        case types if types.contains(p.Contains.ontology)       => p.Contains.toP(node)
        case types if types.contains(p.Prefix.ontology)         => p.Prefix.toP(node)
        case types if types.contains(p.Suffix.ontology)         => p.Suffix.toP(node)
        case types if types.contains(p.Regex.ontology)          => p.Regex.toP(node)
        case types if types.contains(p.Fuzzy.ontology)          => p.Fuzzy.toP(node)
        case types if types.contains(p.ContainsPrefix.ontology) => p.ContainsPrefix.toP(node)
        case types if types.contains(p.ContainsRegex.ontology)  => p.ContainsRegex.toP(node)
        case types if types.contains(p.ContainsFuzzy.ontology)  => p.ContainsFuzzy.toP(node)
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
//    GeoWithin,
//    Without,
    Disjoint,
    Contains,
//    GeoContains,
    Prefix,
    Suffix,
    Regex,
    Fuzzy,
    ContainsPrefix,
    ContainsRegex,
    ContainsFuzzy
  )

  import Numeric.Implicits._
  import Ordering.Implicits._

  sealed trait Helper[T] {
    def eqv(avalue: Any, pvalue: T): Boolean  = pvalue == avalue
    def neqv(avalue: Any, pvalue: T): Boolean = pvalue == avalue
  }
  sealed trait EqHelper[T] extends Helper[T] {
//    def within(avalue: Any, pvalues: Set[Any])  = pvalues.contains(avalue)
//    def without(avalue: Any, pvalues: Set[Any]) = !pvalues.contains(avalue)
//    def contains(avalue: Any, pvalue: T): Boolean
  }
  object EqHelper {
    def map[T](value: T): (T, EqHelper[T]) = {
      val helper: (_, EqHelper[_]) = value match {
        case v: Int           => v -> Helper.IntHelper
        case v: Double        => v -> Helper.DoubleHelper
        case v: Long          => v -> Helper.LongHelper
        case v: Quantity[_]   => v -> Helper.QuantityHelper
        case v: Instant       => v -> Helper.InstantHelper
        case v: LocalDateTime => v -> Helper.LocalDateTimeHelper
        case v: LocalDate     => v -> Helper.LocalDateHelper
        case v: LocalTime     => v -> Helper.LocalTimeHelper
        //        case v: QuantityRange[_] => v -> Helper.QuantityRangeHelper
        case v: String  => v -> Helper.TextHelper
        case v: Boolean => v -> Helper.BooleanHelper
        //        case v: TimeUnit => v -> Helper.TimeHelper
        case v: Geometry => v -> Helper.GeoHelper
        case v: Iterable[_] =>
          v match {
//            case v: ListSet[_] => v -> Helper.listsetHelper
            case v: List[_] => v -> Helper.listHelper[Any, Seq]
            case v: Set[_]  => v -> Helper.setHelper[Any, Set]
//            case v: Vector[_]  => v -> Helper.vectorHelper
          }
        //        case v: IriResource => v -> Helper.IriHelper
        case v: Resource[_] => v -> Helper.ResourceHelper
      }
      helper.asInstanceOf[(T, EqHelper[T])]
    }
  }
  trait OrderHelper[T] extends EqHelper[T] {
    def gt(avalue: Any, pvalue: T): Boolean
    def gte(avalue: Any, pvalue: T): Boolean
    def lt(avalue: Any, pvalue: T): Boolean
    def lte(avalue: Any, pvalue: T): Boolean
  }
  object OrderHelper {
    def map[T](value: T): (T, OrderHelper[T]) = {
      val helper: (_, OrderHelper[_]) = value match {
        case v: Int           => v -> Helper.IntHelper
        case v: Double        => v -> Helper.DoubleHelper
        case v: Long          => v -> Helper.LongHelper
        case v: String        => v -> Helper.TextHelper
        case v: Quantity[_]   => v -> Helper.QuantityHelper
        case v: Instant       => v -> Helper.InstantHelper
        case v: LocalDateTime => v -> Helper.LocalDateTimeHelper
        case v: LocalDate     => v -> Helper.LocalDateHelper
        case v: LocalTime     => v -> Helper.LocalTimeHelper
        //        case v: TimeUnit => v -> Helper.TimeHelper
      }
      helper.asInstanceOf[(T, OrderHelper[T])]
    }
    def get[T](classtype: ClassType[T]): OrderHelper[T] =
      (classtype match {
        case dt: IntType[_]    => P.Helper.IntHelper
        case dt: DoubleType[_] => P.Helper.DoubleHelper
        case dt: LongType[_]   => P.Helper.LongHelper
        case dt: TextType[_]   => P.Helper.TextHelper
        case dt: DateTimeType[_] if dt.iri == LocalDateTimeType.datatype.iri =>
          P.Helper.InstantHelper
        case dt: DateTimeType[_] if dt.iri == DateTimeType.datatype.iri =>
          P.Helper.LocalDateTimeHelper
        case dt: LocalDateType[_] => P.Helper.LocalDateHelper
        case dt: LocalTimeType[_] => P.Helper.LocalTimeHelper
        case _ =>
          throw new Exception(s"unsupported order step, @type to order on is ${classtype.iri}")
      }).asInstanceOf[P.OrderHelper[T]]
  }
  sealed trait PartialOrderHelper[T] extends Helper[T]
  sealed trait NumericHelper[T]      extends RangeHelper[T] {}
  sealed trait RangeHelper[T]        extends OrderHelper[T] {}
  object RangeHelper {
    def map[T](value: T): (T, RangeHelper[T]) = {
      val helper: (_, RangeHelper[_]) = value match {
        case v: Int           => v -> Helper.IntHelper
        case v: Double        => v -> Helper.DoubleHelper
        case v: Long          => v -> Helper.LongHelper
        case v: Quantity[_]   => v -> Helper.QuantityHelper
        case v: Instant       => v -> Helper.InstantHelper
        case v: LocalDateTime => v -> Helper.LocalDateTimeHelper
        case v: LocalDate     => v -> Helper.LocalDateHelper
        case v: LocalTime     => v -> Helper.LocalTimeHelper
        //        case v: QuantityRange[_] => v -> Helper.QuantityRangeHelper
        //        case v: TimeUnit => v -> Helper.TimeHelper
      }
      helper.asInstanceOf[(T, RangeHelper[T])]
    }
  }

  sealed trait StringHelper[T] extends SeqHelper[T] {
    def contains(avalue: Any, pvalue: T): Boolean
//    def prefix(avalue: Any, pvalue: T): Boolean
//    def suffix(avalue: Any, pvalue: T): Boolean
    def regex(avalue: Any, pvalue: scala.util.matching.Regex): Boolean
    def fuzzy(avalue: Any, pvalue: T): Boolean
    def containsPrefix(avalue: Any, pvalue: T): Boolean
    def containsRegex(avalue: Any, pvalue: scala.util.matching.Regex): Boolean
    def containsFuzzy(avalue: Any, pvalue: T): Boolean
  }
  object StringHelper {
    def map[T](value: T): (T, StringHelper[T]) = {
      val helper: (_, StringHelper[_]) = value match {
        case v: String => v -> Helper.TextHelper
        case _         => throw new Exception("No StringHelper found")
      }
      helper.asInstanceOf[(T, StringHelper[T])]
    }
  }

//  sealed trait ObjectHelper[T] extends EqHelper[T] {
//    def intersect(avalue: Any, pvalue: T): Boolean
//    def disjoint(avalue: Any, pvalue: T): Boolean
//    def contains(avalue: Any, pvalue: T): Boolean
//    def within(avalue: Any, pvalue: T): Boolean
//    //    def within(avalue: Any, pvalue: T): Boolean
//  }
//  object ObjectHelper {
//    def map[T](value: T): (T, ObjectHelper[T]) = {
//      val helper: (_, ObjectHelper[_]) = value match {
//        case v: Geometry => v -> Helper.GeoHelper
//        case _           => throw new Exception("No ObjectHelper found")
//      }
//      helper.asInstanceOf[(T, ObjectHelper[T])]
//    }
//  }

  object CollectionHelper {
    def map[T](value: T): (T, CollectionHelper[T]) = {
      val helper: (_, CollectionHelper[_]) = value match {
        case v: String   => v -> Helper.TextHelper
        case v: Geometry => v -> Helper.GeoHelper
        case v: Seq[_]   => v -> Helper.listHelper[Any, Seq]
        case v: Set[_]   => v -> Helper.setHelper[Any, Set]
        case _           => throw new Exception("No CollectionHelper found")
      }
      helper.asInstanceOf[(T, CollectionHelper[T])]
    }
  }
  sealed trait CollectionHelper[T] extends EqHelper[T] {
    def intersect(avalue: Any, pvalue: T): Boolean
    def disjoint(avalue: Any, pvalue: T): Boolean
    def contains(avalue: Any, pvalue: T): Boolean
    def within(avalue: Any, pvalue: T): Boolean
  }

  object SeqHelper {
    def map[T](value: T): (T, SeqHelper[T]) = {
      val helper: (_, SeqHelper[_]) = value match {
        case v: String => v -> Helper.TextHelper
        case v: Seq[_] => v -> Helper.listHelper[Any, Seq]
//        case v: Vector[T] => v -> Helper.vectorHelper[T]
        case _ => throw new Exception("No SeqHelper found")
      }
      helper.asInstanceOf[(T, SeqHelper[T])]
    }
  }
  sealed trait SeqHelper[T] extends EqHelper[T] with CollectionHelper[T] {
    def startsWith(avalue: Any, pvalue: T): Boolean
    def endsWith(avalue: Any, pvalue: T): Boolean
  }

  object Helper {
    //    implicit object IriResourceHelper extends IriHelper[IriResource]
    implicit def OntologyHelper = new EqHelper[Ontology] {
      def contains(avalue: Any, pvalue: Ontology): Boolean = throw new Exception("")
    }
    implicit def PropertyHelper = new EqHelper[Property] {
      def contains(avalue: Any, pvalue: Property): Boolean = throw new Exception("")
    }
    implicit def ResourceHelper[T <: Resource[T]] = new EqHelper[T] {
      //      override def eqv(avalue: Any, pvalue: T): Boolean = super.eqv(avalue, pvalue.value)
      //      override def neqv(avalue: Any, pvalue: T): Boolean = super.neqv(avalue, pvalue.value)
//      def contains(avalue: Any, pvalue: T): Boolean = throw new Exception("")
    }
    implicit object IntHelper extends NumericHelper[Int] {
      def gt(avalue: Any, pvalue: Int): Boolean = avalue match {
        case avalue: Int    => avalue > pvalue
        case avalue: Double => avalue > pvalue
        case avalue: Long   => avalue > pvalue
        case _              => false
      }
      def gte(avalue: Any, pvalue: Int): Boolean = avalue match {
        case avalue: Int    => avalue >= pvalue
        case avalue: Double => avalue >= pvalue
        case avalue: Long   => avalue >= pvalue
        case _              => false
      }
      def lt(avalue: Any, pvalue: Int): Boolean = avalue match {
        case avalue: Int    => avalue < pvalue
        case avalue: Double => avalue < pvalue
        case avalue: Long   => avalue < pvalue
        case _              => false
      }
      def lte(avalue: Any, pvalue: Int): Boolean = avalue match {
        case avalue: Int    => avalue <= pvalue
        case avalue: Double => avalue <= pvalue
        case avalue: Long   => avalue <= pvalue
        case _              => false
      }
//      def contains(avalue: Any, pvalue: Int): Boolean = throw new Exception("")
    }
    implicit object DoubleHelper extends NumericHelper[Double] {
      def gt(avalue: Any, pvalue: Double): Boolean = avalue match {
        case avalue: Int    => avalue > pvalue
        case avalue: Double => avalue > pvalue
        case avalue: Long   => avalue > pvalue
        case _              => false
      }
      def gte(avalue: Any, pvalue: Double): Boolean = avalue match {
        case avalue: Int    => avalue >= pvalue
        case avalue: Double => avalue >= pvalue
        case avalue: Long   => avalue >= pvalue
        case _              => false
      }
      def lt(avalue: Any, pvalue: Double): Boolean = avalue match {
        case avalue: Int    => avalue < pvalue
        case avalue: Double => avalue < pvalue
        case avalue: Long   => avalue < pvalue
        case _              => false
      }
      def lte(avalue: Any, pvalue: Double): Boolean = avalue match {
        case avalue: Int    => avalue <= pvalue
        case avalue: Double => avalue <= pvalue
        case avalue: Long   => avalue <= pvalue
        case _              => false
      }
//      def contains(avalue: Any, pvalue: Double): Boolean = throw new Exception("")
    }
    implicit object LongHelper extends NumericHelper[Long] {
      def gt(avalue: Any, pvalue: Long): Boolean = avalue match {
        case avalue: Int    => avalue > pvalue
        case avalue: Double => avalue > pvalue
        case avalue: Long   => avalue > pvalue
        case _              => false
      }
      def gte(avalue: Any, pvalue: Long): Boolean = avalue match {
        case avalue: Int    => avalue >= pvalue
        case avalue: Double => avalue >= pvalue
        case avalue: Long   => avalue >= pvalue
        case _              => false
      }
      def lt(avalue: Any, pvalue: Long): Boolean = avalue match {
        case avalue: Int    => avalue < pvalue
        case avalue: Double => avalue < pvalue
        case avalue: Long   => avalue < pvalue
        case _              => false
      }
      def lte(avalue: Any, pvalue: Long): Boolean = avalue match {
        case avalue: Int    => avalue <= pvalue
        case avalue: Double => avalue <= pvalue
        case avalue: Long   => avalue <= pvalue
        case _              => false
      }
//      def contains(avalue: Any, pvalue: Long): Boolean = throw new Exception("")
    }
    implicit object QuantityHelper extends NumericHelper[Quantity[_]] {
      def gt(avalue: Any, pvalue: Quantity[_]): Boolean = avalue match {
        case avalue: Quantity[Any] => avalue.compare(pvalue) > 0
        case _                     => false
      }
      def gte(avalue: Any, pvalue: Quantity[_]): Boolean = avalue match {
        case avalue: Quantity[Any] => avalue.compare(pvalue) >= 0
        case _                     => false
      }
      def lt(avalue: Any, pvalue: Quantity[_]): Boolean = avalue match {
        case avalue: Quantity[Any] => avalue.compare(pvalue) < 0
        case _                     => false
      }
      def lte(avalue: Any, pvalue: Quantity[_]): Boolean = avalue match {
        case avalue: Quantity[Any] => avalue.compare(pvalue) <= 0
        case _                     => false
      }
//      def contains(avalue: Any, pvalue: Quantity[_]): Boolean = throw new Exception("")
    }
    //    implicit object DateRangeHelper extends RangeHelper[Date] {}
    //    implicit object QuantityRangeHelper extends RangeHelper[QuantityRange[_]] {
    //      def gt(avalue: Any, pvalue: QuantityRange[_]): Boolean = avalue match {
    //        case avalue: Quantity[_] => pvalue.upper
    //        case _ => false
    //      }
    //      def gte(avalue: Any, pvalue: QuantityRange[_]): Boolean
    //      def lt(avalue: Any, pvalue: QuantityRange[_]): Boolean
    //      def lte(avalue: Any, pvalue: QuantityRange[_]): Boolean
    //    }
    //    implicit object ContainsHelper extends Contains.Helper[String] {
    //      def contains(avalue: Any, pvalue: String): Boolean = avalue match {
    //        case avalue: String => avalue.contains(pvalue)
    //        case _ => false
    //      }
    //    }
    implicit object TextHelper extends StringHelper[String] with OrderHelper[String] {
      def gt(avalue: Any, pvalue: String): Boolean = avalue match {
        case avalue: String => avalue > pvalue
        case _              => false
      }
      def gte(avalue: Any, pvalue: String): Boolean = avalue match {
        case avalue: String => avalue >= pvalue
        case _              => false
      }
      def lt(avalue: Any, pvalue: String): Boolean = avalue match {
        case avalue: String => avalue < pvalue
        case _              => false
      }
      def lte(avalue: Any, pvalue: String): Boolean = avalue match {
        case avalue: String => avalue <= pvalue
        case _              => false
      }
      def intersect(avalue: Any, pvalue: String): Boolean = avalue match {
        case avalue: String => avalue.intersect(pvalue).nonEmpty
        case _              => false
      }
      def disjoint(avalue: Any, pvalue: String): Boolean = avalue match {
        case avalue: String => avalue.intersect(pvalue).isEmpty
        case _              => false
      }
      def within(avalue: Any, pvalue: String): Boolean = avalue match {
        case avalue: String => pvalue.containsSlice(avalue)
        case _              => false
      }
      def startsWith(avalue: Any, pvalue: String): Boolean = avalue match {
        case avalue: String => avalue.startsWith(pvalue)
        case _              => false
      }
      def endsWith(avalue: Any, pvalue: String): Boolean = avalue match {
        case avalue: String => avalue.endsWith(pvalue)
        case _              => false
      }
      def contains(avalue: Any, pvalue: String): Boolean = avalue match {
        case avalue: String => avalue.contains(pvalue)
        case _              => false
      }
      def regex(avalue: Any, pvalue: scala.util.matching.Regex): Boolean = avalue match {
        case avalue: String => avalue.matches(pvalue.regex)
        case _              => false
      }
      def fuzzy(avalue: Any, pvalue: String): Boolean = avalue match {
        case avalue: String => throw AssertionNotSupported("fuzzy string match not supported")
        case _              => false
      }
      def containsPrefix(avalue: Any, pvalue: String): Boolean = avalue match {
        case avalue: String => avalue.split(' ').exists(_.startsWith(pvalue))
        case _              => false
      }
      def containsRegex(avalue: Any, pvalue: scala.util.matching.Regex): Boolean = avalue match {
        case avalue: String => avalue.split(' ').exists(_.matches(pvalue.regex))
        case _              => false
      }
      def containsFuzzy(avalue: Any, pvalue: String): Boolean = avalue match {
        case avalue: String => throw AssertionNotSupported("fuzzy string match not supported")
        case _              => false
      }
    }
    implicit object BooleanHelper extends EqHelper[Boolean] {
//      def contains(avalue: Any, pvalue: Boolean): Boolean = throw new Exception("")
    }
    implicit object InstantHelper extends RangeHelper[Instant] {
      def gt(avalue: Any, pvalue: Instant): Boolean = avalue match {
        case avalue: Instant => avalue.isAfter(pvalue)
        case _               => false
      }
      def gte(avalue: Any, pvalue: Instant): Boolean = avalue match {
        case avalue: Instant => avalue.isAfter(pvalue) || eqv(avalue, pvalue)
        case _               => false
      }
      def lt(avalue: Any, pvalue: Instant): Boolean = avalue match {
        case avalue: Instant => avalue.isBefore(pvalue)
        case _               => false
      }
      def lte(avalue: Any, pvalue: Instant): Boolean = avalue match {
        case avalue: Instant => avalue.isBefore(pvalue) || eqv(avalue, pvalue)
        case _               => false
      }
//      def contains(avalue: Any, pvalue: Instant): Boolean = avalue match {
//        //        case avalue: LocalDate => LocalDate.of(Instant.)
//        case _ => false
//      }
    }
    implicit object LocalDateTimeHelper extends RangeHelper[LocalDateTime] {
      def gt(avalue: Any, pvalue: LocalDateTime): Boolean = avalue match {
        case avalue: LocalDateTime => avalue.isAfter(pvalue)
        case _                     => false
      }
      def gte(avalue: Any, pvalue: LocalDateTime): Boolean = avalue match {
        case avalue: LocalDateTime => avalue.isAfter(pvalue) || eqv(avalue, pvalue)
        case _                     => false
      }
      def lt(avalue: Any, pvalue: LocalDateTime): Boolean = avalue match {
        case avalue: LocalDateTime => avalue.isBefore(pvalue)
        case _                     => false
      }
      def lte(avalue: Any, pvalue: LocalDateTime): Boolean = avalue match {
        case avalue: LocalDateTime => avalue.isBefore(pvalue) || eqv(avalue, pvalue)
        case _                     => false
      }
//      def contains(avalue: Any, pvalue: LocalDateTime): Boolean = avalue match {
//        //        case avalue: LocalDate => LocalDate.of(Instant.)
//        case _ => false
//      }
    }
    implicit object LocalDateHelper extends RangeHelper[LocalDate] {
      def gt(avalue: Any, pvalue: LocalDate): Boolean = avalue match {
        case avalue: LocalDate => avalue.isAfter(pvalue)
        case _                 => false
      }
      def gte(avalue: Any, pvalue: LocalDate): Boolean = avalue match {
        case avalue: LocalDate => avalue.isAfter(pvalue) || eqv(avalue, pvalue)
        case _                 => false
      }
      def lt(avalue: Any, pvalue: LocalDate): Boolean = avalue match {
        case avalue: LocalDate => avalue.isBefore(pvalue)
        case _                 => false
      }
      def lte(avalue: Any, pvalue: LocalDate): Boolean = avalue match {
        case avalue: LocalDate => avalue.isBefore(pvalue) || eqv(avalue, pvalue)
        case _                 => false
      }
//      def contains(avalue: Any, pvalue: LocalDate): Boolean = avalue match {
//        //        case avalue: LocalDate => LocalDate.of(Instant.)
//        case _ => false
//      }
    }
    implicit object LocalTimeHelper extends RangeHelper[LocalTime] {
      def gt(avalue: Any, pvalue: LocalTime): Boolean = avalue match {
        case avalue: LocalTime => avalue.isAfter(pvalue)
        case _                 => false
      }
      def gte(avalue: Any, pvalue: LocalTime): Boolean = avalue match {
        case avalue: LocalTime => avalue.isAfter(pvalue) || eqv(avalue, pvalue)
        case _                 => false
      }
      def lt(avalue: Any, pvalue: LocalTime): Boolean = avalue match {
        case avalue: LocalTime => avalue.isBefore(pvalue)
        case _                 => false
      }
      def lte(avalue: Any, pvalue: LocalTime): Boolean = avalue match {
        case avalue: LocalTime => avalue.isBefore(pvalue) || eqv(avalue, pvalue)
        case _                 => false
      }
//      def contains(avalue: Any, pvalue: LocalTime): Boolean = avalue match {
//        //        case avalue: LocalDate => LocalDate.of(Instant.)
//        case _ => false
//      }
    }
    //    implicit object TimeHelper extends RangeHelper[TimeUnit] {}
    implicit def GeoHelper[T <: Geometry] = new CollectionHelper[T] {
      def intersect(avalue: Any, pvalue: T): Boolean = avalue match {
        case avalue: Geometry => avalue.intersect(pvalue)
        case _                => false
      }
      def disjoint(avalue: Any, pvalue: T): Boolean = avalue match {
        case avalue: Geometry => avalue.disjoint(pvalue)
        case _                => false
      }
      def contains(avalue: Any, pvalue: T): Boolean = avalue match {
        case avalue: Geometry => avalue.contains(pvalue)
        case _                => false
      }
      def within(avalue: Any, pvalue: T): Boolean = avalue match {
        case avalue: Geometry => avalue.within(pvalue)
        case _                => false
      }
    }

    implicit def listHelper[T, F[+Z] <: Seq[Z]] = new SeqHelper[F[T]] {
      override def eqv(avalue: Any, pvalue: F[T]): Boolean = avalue match {
        case avalue: F[_] => avalue == pvalue
        case _            => false
      }
      override def neqv(avalue: Any, pvalue: F[T]): Boolean = avalue match {
        case avalue: F[_] => avalue != pvalue
        case _            => false
      }
      def intersect(avalue: Any, pvalue: F[T]): Boolean = avalue match {
        case avalue: F[_] => avalue.intersect(pvalue).nonEmpty
        case _            => false
      }
      def disjoint(avalue: Any, pvalue: F[T]): Boolean = avalue match {
        case avalue: F[_] => avalue.intersect(pvalue).isEmpty
        case _            => false
      }
      def contains(avalue: Any, pvalue: F[T]): Boolean = avalue match {
        case avalue: F[_] => avalue.containsSlice(pvalue)
        case _            => false
      }
      def within(avalue: Any, pvalue: F[T]): Boolean = avalue match {
        case avalue: F[_] => pvalue.containsSlice(avalue)
        case _            => false
      }
      def startsWith(avalue: Any, pvalue: F[T]): Boolean = avalue match {
        case avalue: F[_] => avalue.startsWith(pvalue)
        case _            => false
      }
      def endsWith(avalue: Any, pvalue: F[T]): Boolean = avalue match {
        case avalue: F[_] => avalue.endsWith(pvalue)
        case _            => false
      }
    }
    implicit def setHelper[T, F[Z] <: Set[Z]] = new CollectionHelper[F[T]] {
      def intersect(avalue: Any, pvalue: F[T]): Boolean = avalue match {
        case avalue: F[T] => avalue.intersect(pvalue).nonEmpty
        case _            => false
      }
      def disjoint(avalue: Any, pvalue: F[T]): Boolean = avalue match {
        case avalue: F[T] => avalue.intersect(pvalue).isEmpty
        case _            => false
      }
      def contains(avalue: Any, pvalue: F[T]): Boolean = avalue match {
        case avalue: F[T] => avalue.intersect(pvalue).size == pvalue.size
        case _            => false
      }
      def within(avalue: Any, pvalue: F[T]): Boolean = avalue match {
        case avalue: F[T] => pvalue.intersect(avalue).size == pvalue.size
        case _            => false
      }
    }
//    implicit def listsetHelper[T] = new CollectionHelper[ListSet[T]] {
//      def intersect(avalue: Any, pvalue: ListSet[T]): Boolean =
//        avalue.intersect(pvalue).nonEmpty
//      def disjoint(avalue: Any, pvalue: ListSet[T]): Boolean =
//        avalue.intersect(pvalue).isEmpty
//      def contains(avalue: Any, pvalue: ListSet[T])(implicit ev: ListSet[T] <:< Iterable[_]): Boolean =
//        avalue.toList.containsSlice(pvalue.toList)
//      def contains(avalue: Any, pvalue: ListSet[T]): Boolean = avalue match {
//        case avalue: ListSet[_] => avalue.toList.containsSlice(pvalue.toList)
//        case _                  => false
//      }
//      def within(avalue: Any, pvalue: ListSet[T]): Boolean =
//        pvalue.toList.containsSlice(avalue.toList)
//    }
//    implicit def vectorHelper[T] = new SeqHelper[Vector[T]] {
//      override def eqv(avalue: Any, pvalue: Vector[T]): Boolean = avalue match {
//        case avalue: Vector[_] => avalue == pvalue
//        case _                 => false
//      }
//      override def neqv(avalue: Any, pvalue: Vector[T]): Boolean = avalue match {
//        case avalue: Vector[_] => avalue != pvalue
//        case _                 => false
//      }
//      def intersect(avalue: Any, pvalue: Vector[T]): Boolean = avalue match {
//        case avalue: Vector[_] => avalue.intersect(pvalue).nonEmpty
//        case _                 => false
//      }
//      def disjoint(avalue: Any, pvalue: Vector[T]): Boolean = avalue match {
//        case avalue: Vector[_] => avalue.intersect(pvalue).isEmpty
//        case _                 => false
//      }
//      def contains(avalue: Any, pvalue: Vector[T]): Boolean = avalue match {
//        case avalue: Vector[_] => avalue.containsSlice(pvalue)
//        case _                 => false
//      }
//      def within(avalue: Any, pvalue: Vector[T]): Boolean = avalue match {
//        case avalue: Vector[_] => pvalue.containsSlice(avalue)
//        case _                 => false
//      }
//      def startsWith(avalue: Any, pvalue: Vector[T]): Boolean = avalue match {
//        case avalue: Vector[_] => avalue.startsWith(pvalue)
//        case _                 => false
//      }
//      def endsWith(avalue: Any, pvalue: Vector[T]): Boolean = avalue match {
//        case avalue: Vector[_] => avalue.endsWith(pvalue)
//        case _                 => false
//      }
//    }
    //    implicit def mapHelper[K,V] = new CollectionHelper[Map[K,V]] {
    //      def intersect(avalue: Map[K,V], pvalue: Map[K,V]): Boolean = avalue.intersect(pvalue).nonEmpty
    //      def disjoint(avalue: Map[K,V], pvalue: Map[K,V]): Boolean = avalue.intersect(pvalue).isEmpty
    //      def contains(avalue: Map[K,V], pvalue: Map[K,V]): Boolean = avalue.containsSlice(pvalue)
    //      def within(avalue: Map[K,V], pvalue: Map[K,V]): Boolean = pvalue.containsSlice(avalue)
    //    }

  }

  def &&[T, PR[+Z] <: P[Z]](predicate: PR[T]*): p.And = p.And(predicate.toList)
  def ||[T, PR[+Z] <: P[Z]](predicate: PR[T]*): p.Or  = p.Or(predicate.toList)
  def eqv[T: EqHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Eqv[T] =
    p.Eqv(value)
  def neqv[T: EqHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Neqv[T] =
    p.Neqv(value)
  def gt[T: OrderHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Gt[T] =
    p.Gt(value)
  def gte[T: OrderHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Gte[T] =
    p.Gte(value)
  def lt[T: OrderHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Lt[T] =
    p.Lt(value)
  def lte[T: OrderHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Lte[T] =
    p.Lte(value)
  //  def between[T](range: QuantityRange[T]): P[T] = p.Between(range.lower, range.upper)
  def between[T: RangeHelper, T0, TT0 <: ClassType[_]](lower: T, upper: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Between[T] = p.Between(lower, upper)
  def outside[T: RangeHelper, T0, TT0 <: ClassType[_]](lower: T, upper: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Outside[T] = p.Outside(lower, upper)
  def inside[T: RangeHelper, T0, TT0 <: ClassType[_]](lower: T, upper: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Inside[T] = p.Inside(lower, upper)
  //  def within(values: List[Any]): P[Any] = within(values.toSet)
  def within[T: CollectionHelper, T0, TT0 <: ClassType[_]](value: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Within[T] = p.Within(value)
  //  def within[T: ObjectHelper](value: T): GeoWithin[T] = p.GeoWithin(value)
  //  def without(values: List[Any]): P[Any] = without(values.toSet)
//  def without[T: CollectionHelper](value: T, values: T*): Without[T] = p.Without(value :: values.toList)
  def intersect[T: CollectionHelper, T0, TT0 <: ClassType[_]](value: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Intersect[T] = p.Intersect(value)
  def disjoint[T: CollectionHelper, T0, TT0 <: ClassType[_]](value: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Disjoint[T] = p.Disjoint(value)
  //  def contains[T: ObjectHelper](value: T): GeoContains[T] = p.GeoContains(value)
  def contains[T: CollectionHelper, T0, TT0 <: ClassType[_]](value: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Contains[T] = p.Contains(value)
  def prefix[T: SeqHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Prefix[T] =
    p.Prefix(value)
  def startsWith[T: SeqHelper, T0, TT0 <: ClassType[_]](value: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Prefix[T] = p.Prefix(value)
  def suffix[T: SeqHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Suffix[T] =
    p.Suffix(value)
  def endsWith[T: SeqHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Suffix[T] =
    p.Suffix(value)
  def regex(value: scala.util.matching.Regex): Regex = p.Regex(value)
  def fuzzy[T: StringHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Fuzzy[T] =
    p.Fuzzy(value)
  def containsPrefix[T: StringHelper, T0, TT0 <: ClassType[_]](value: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): ContainsPrefix[T] = p.ContainsPrefix(value)
  def containsRegex(value: scala.util.matching.Regex): ContainsRegex = p.ContainsRegex(value)
  def containsFuzzy[T: StringHelper, T0, TT0 <: ClassType[_]](value: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): ContainsFuzzy[T] = p.ContainsFuzzy(value)

  //  MemGraphDefault.ns.storeOntology(ontology)

  import shapeless.=:!=
  implicit class WithPredicate[T <: P[_]](_predicate: T)(implicit ev: T =:!= And, ev2: T =:!= Or) {
    def &&[T0, PR0[Z] <: P[Z]](predicate: PR0[T0]*): p.And = p.And(_predicate :: predicate.toList)
    def ||[T0, PR0[Z] <: P[Z]](predicate: PR0[T0]*): p.Or  = p.Or(_predicate :: predicate.toList)
  }
//  implicit class WithPredicateHList[K <: HList](p: K)(implicit
//                                                      val d: LUBConstraint[K, P[_]]) {
//
//    def eqv[T: EqHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.eqv(value) :: p
//    def neqv[T: EqHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.neqv(value) :: p
//    def gt[T: OrderHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.gt(value) :: p
//    def gte[T: OrderHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.gte(value) :: p
//    def lt[T: OrderHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.lt(value) :: p
//    def lte[T: OrderHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.lte(value) :: p
//    def between[T: RangeHelper, T0, TT0 <: ClassType[_]](lower: T, upper: T)(
//        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.between(lower, upper) :: p
//    def outside[T: RangeHelper, T0, TT0 <: ClassType[_]](lower: T, upper: T)(
//        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.outside(lower, upper) :: p
//    def inside[T: RangeHelper, T0, TT0 <: ClassType[_]](lower: T, upper: T)(
//        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.inside(lower, upper) :: p
//    def within[T: CollectionHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.within(value) :: p
//    //    def within[T, Out <: HList](value: T)(implicit prepend: Prepend.Aux[K, GeoWithin[T] :: HNil, Out]) =
//    //      P.within(value))
////    def without[T, Out <: HList](value: T, values: T*) =
////      P.without(value, values: _*) :: p
//    def intersect[T: CollectionHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.intersect(value) :: p
//    def disjoint[T: CollectionHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.disjoint(value) :: p
//    def contains[T: CollectionHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.contains(value) :: p
//    //    def contains[T: ObjectHelper, Out <: HList](value: T)(implicit prepend: Prepend.Aux[K, GeoContains[T] :: HNil, Out]) =
//    //      P.contains(value))
//    def prefix[T: SeqHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.prefix(value) :: p
//    def suffix[T: SeqHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.suffix(value) :: p
//    def regex[Out <: HList](value: scala.util.matching.Regex) =
//      P.regex(value) :: p
//    def fuzzy[T: StringHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.fuzzy(value) :: p
//    def containsPrefix[T: StringHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.containsPrefix(value) :: p
//    def containsRegex[Out <: HList](value: scala.util.matching.Regex) =
//      P.containsRegex(value) :: p
//    def containsFuzzy[T: StringHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
//      P.containsFuzzy(value) :: p
//  }
//
//  implicit class WithPredicate[T <: P[_]](p: T) //extends WithPredicateHList(p :: HNil)
//  //  implicit def toHList[T <: P[_]](p: T) = p :: HNil
}

trait P[+T] extends Product with Serializable {
  def assert(avalue: Any): Boolean

  def toNode: Node
  def prettyPrint: String
}

trait EqP[+T] extends P[T] {
  def pvalue: Any
  //  def datatype: DataType[T] = outE(EqP.keys.value).head.inV.types.head
}
object EqP
    extends PredicateDef(label = "EqP", comment = "Equality/comparable predicate", `@extends` = () => P.ontology :: Nil) {

  object keys extends P.Properties {
    object value
        extends Property.PropertyDef(
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
  def lower: T
  def upper: T
}
object RangeP
    extends PredicateDef(label = "RangeP", comment = "Range predicate", `@extends` = () => P.ontology :: Nil) {

  object keys extends P.Properties {
    object lower
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/p/range/lower",
          "lower",
          "Any value"
        ) {}
    object upper
        extends Property.PropertyDef(
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
  def pvalue: T
}
object CollectionP
    extends PredicateDef(label = "CollectionP", comment = "Collection predicate", `@extends` = () => P.ontology :: Nil) {

  object keys extends P.Properties {
    object value
        extends Property.PropertyDef(
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

trait PredicateWrapper[+T] {
  //  def ontology: Ontology
  def toP(node: Node): T
}

abstract class PredicateDef(label: String,
                            comment: String = "",
                            `@extends`: () => List[Ontology] = () => List(P.ontology))
    extends OntologyDef(lspace.NS.vocab.Lspace + s"librarian/p/${label}", Set(), label, comment, `@extends`)
