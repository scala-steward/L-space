package lspace.librarian.process.traversal

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.time.temporal.Temporal

import lspace.librarian.process.traversal.p._
import lspace.librarian.structure._
import lspace.NS
import lspace.librarian.datatype._
import lspace.librarian.process.traversal.EqP.ontologyNode
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.mem.{MemGraph, MemGraphDefault}
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.util.AssertionNotSupported
import lspace.types._
import lspace.types.vector.Geometry
import shapeless.ops.hlist.Prepend
import shapeless.{::, HList, HNil, LUBConstraint}
import squants.time.TimeUnit
import squants.{Quantity, QuantityRange}

import scala.collection.immutable.ListSet
import scala.util.Try

object P {
  private val ontologyNode =
    MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/P")
  ontologyNode.addLabel(Ontology.ontology)

  lazy val ontology: Ontology = Ontology(ontologyNode)
  //  lazy val classType: ClassType[P[_]] = ClassType[P[_]](ontology.iri /*, P.wrap*/ )

  implicit def nodeToP(node: Node): P[_] = P.wrap(node)

  def wrap(node: Node): P[_] = node match {
    case p: P[_] => p
    case _ =>
      node.labels match {
        case types if types.contains(p.Eqv.ontology)            => p.Eqv.wrap(node)
        case types if types.contains(p.Neqv.ontology)           => p.Neqv.wrap(node)
        case types if types.contains(p.Gt.ontology)             => p.Gt.wrap(node)
        case types if types.contains(p.Gte.ontology)            => p.Gte.wrap(node)
        case types if types.contains(p.Lt.ontology)             => p.Lt.wrap(node)
        case types if types.contains(p.Lte.ontology)            => p.Lte.wrap(node)
        case types if types.contains(p.Between.ontology)        => p.Between.wrap(node)
        case types if types.contains(p.Outside.ontology)        => p.Outside.wrap(node)
        case types if types.contains(p.Inside.ontology)         => p.Inside.wrap(node)
        case types if types.contains(p.Intersect.ontology)      => p.Intersect.wrap(node)
        case types if types.contains(p.Within.ontology)         => p.Within.wrap(node)
        case types if types.contains(p.Without.ontology)        => p.Without.wrap(node)
        case types if types.contains(p.Disjoint.ontology)       => p.Disjoint.wrap(node)
        case types if types.contains(p.Contains.ontology)       => p.Contains.wrap(node)
        case types if types.contains(p.Prefix.ontology)         => p.Prefix.wrap(node)
        case types if types.contains(p.Suffix.ontology)         => p.Suffix.wrap(node)
        case types if types.contains(p.Regex.ontology)          => p.Regex.wrap(node)
        case types if types.contains(p.Fuzzy.ontology)          => p.Fuzzy.wrap(node)
        case types if types.contains(p.ContainsPrefix.ontology) => p.ContainsPrefix.wrap(node)
        case types if types.contains(p.ContainsRegex.ontology)  => p.ContainsRegex.wrap(node)
        case types if types.contains(p.ContainsFuzzy.ontology)  => p.ContainsFuzzy.wrap(node)
        case types =>
          throw new Exception(s"No valid P-ontology found for types ${types}")
      }
  }
  lazy val predicates: List[PredicateCompanion] = List(
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
    GeoWithin,
    Without,
    Disjoint,
    Contains,
    GeoContains,
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
    def within(avalue: Any, pvalues: Set[T]) = avalue match {
      case avalue: T => pvalues.contains(avalue)
      case _         => false
    }
    def without(avalue: Any, pvalues: Set[T]) = avalue match {
      case avalue: T => !pvalues.contains(avalue)
      case _         => true
    }
    def contains(avalue: Any, pvalue: T): Boolean
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
            case v: ListSet[_] => v -> Helper.listsetHelper
            case v: List[_]    => v -> Helper.listHelper
            case v: Set[_]     => v -> Helper.setHelper
            case v: Vector[_]  => v -> Helper.vectorHelper
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
        case v: Quantity[_]   => v -> Helper.QuantityHelper
        case v: Instant       => v -> Helper.InstantHelper
        case v: LocalDateTime => v -> Helper.LocalDateTimeHelper
        case v: LocalDate     => v -> Helper.LocalDateHelper
        case v: LocalTime     => v -> Helper.LocalTimeHelper
        case v: String        => v -> Helper.TextHelper
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
        case dt: DateTimeType[_] if dt.iri == LocalDateTimeType.localdatetimeType.iri =>
          P.Helper.InstantHelper
        case dt: DateTimeType[_] if dt.iri == DateTimeType.datetimeType.iri =>
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

  sealed trait StringHelper[T] extends OrderHelper[T] {
    def prefix(avalue: Any, pvalue: T): Boolean
    def suffix(avalue: Any, pvalue: T): Boolean
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

  sealed trait ObjectHelper[T] extends EqHelper[T] {
    def intersect(avalue: Any, pvalue: T): Boolean
    def disjoint(avalue: Any, pvalue: T): Boolean
    def contains(avalue: Any, pvalue: T): Boolean
    def within(avalue: Any, pvalue: T): Boolean
    //    def within(avalue: Any, pvalue: T): Boolean
  }
  object ObjectHelper {
    def map[T](value: T): (T, ObjectHelper[T]) = {
      val helper: (_, ObjectHelper[_]) = value match {
        case v: Geometry => v -> Helper.GeoHelper
        case _           => throw new Exception("No ObjectHelper found")
      }
      helper.asInstanceOf[(T, ObjectHelper[T])]
    }
  }

  object Helper {
    //    implicit object IriResourceHelper extends IriHelper[IriResource]
    implicit def ResourceHelper[T <: Resource[T]] = new EqHelper[T] {
      //      override def eqv(avalue: Any, pvalue: T): Boolean = super.eqv(avalue, pvalue.value)
      //      override def neqv(avalue: Any, pvalue: T): Boolean = super.neqv(avalue, pvalue.value)
      def contains(avalue: Any, pvalue: T): Boolean = throw new Exception("")
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
      def contains(avalue: Any, pvalue: Int): Boolean = throw new Exception("")
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
      def contains(avalue: Any, pvalue: Double): Boolean = throw new Exception("")
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
      def contains(avalue: Any, pvalue: Long): Boolean = throw new Exception("")
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
      def contains(avalue: Any, pvalue: Quantity[_]): Boolean = throw new Exception("")
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
    implicit object TextHelper extends StringHelper[String] {
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
      def prefix(avalue: Any, pvalue: String): Boolean = avalue match {
        case avalue: String => avalue.startsWith(pvalue)
        case _              => false
      }
      def suffix(avalue: Any, pvalue: String): Boolean = avalue match {
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
      def contains(avalue: Any, pvalue: Boolean): Boolean = throw new Exception("")
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
      def contains(avalue: Any, pvalue: Instant): Boolean = avalue match {
        //        case avalue: LocalDate => LocalDate.of(Instant.)
        case _ => false
      }
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
      def contains(avalue: Any, pvalue: LocalDateTime): Boolean = avalue match {
        //        case avalue: LocalDate => LocalDate.of(Instant.)
        case _ => false
      }
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
      def contains(avalue: Any, pvalue: LocalDate): Boolean = avalue match {
        //        case avalue: LocalDate => LocalDate.of(Instant.)
        case _ => false
      }
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
      def contains(avalue: Any, pvalue: LocalTime): Boolean = avalue match {
        //        case avalue: LocalDate => LocalDate.of(Instant.)
        case _ => false
      }
    }
    //    implicit object TimeHelper extends RangeHelper[TimeUnit] {}
    implicit object GeoHelper extends ObjectHelper[Geometry] {
      def intersect(avalue: Any, pvalue: Geometry): Boolean = avalue match {
        case avalue: Geometry => avalue.intersect(pvalue)
        case _                => false
      }
      def disjoint(avalue: Any, pvalue: Geometry): Boolean = avalue match {
        case avalue: Geometry => avalue.disjoint(pvalue)
        case _                => false
      }
      def contains(avalue: Any, pvalue: Geometry): Boolean = avalue match {
        case avalue: Geometry => avalue.contains(pvalue)
        case _                => false
      }
      def within(avalue: Any, pvalue: Geometry): Boolean = avalue match {
        case avalue: Geometry => avalue.within(pvalue)
        case _                => false
      }
    }

    sealed trait CollectionHelper[T] extends EqHelper[T] {
      def intersect(avalue: T, pvalue: T): Boolean
      def disjoint(avalue: T, pvalue: T): Boolean
      def contains(avalue: T, pvalue: T)(implicit ev: T <:< Iterable[_]): Boolean
      def within(avalue: T, pvalue: T): Boolean
    }
    implicit def listHelper[T] = new CollectionHelper[List[T]] {
      def eqv(avalue: List[T], pvalue: List[T]): Boolean       = avalue.intersect(pvalue).nonEmpty
      def neqv(avalue: List[T], pvalue: List[T]): Boolean      = avalue.intersect(pvalue).nonEmpty
      def intersect(avalue: List[T], pvalue: List[T]): Boolean = avalue.intersect(pvalue).nonEmpty
      def disjoint(avalue: List[T], pvalue: List[T]): Boolean  = avalue.intersect(pvalue).isEmpty
      def contains(avalue: List[T], pvalue: List[T])(implicit ev: List[T] <:< Iterable[_]): Boolean =
        avalue.containsSlice(pvalue)
      def contains(avalue: Any, pvalue: List[T]): Boolean = avalue match {
        case avalue: List[T] => avalue.containsSlice(pvalue)
        case _               => false
      }
      def within(avalue: List[T], pvalue: List[T]): Boolean = pvalue.containsSlice(avalue)
    }
    implicit def setHelper[T] = new CollectionHelper[Set[T]] {
      def intersect(avalue: Set[T], pvalue: Set[T]): Boolean = avalue.intersect(pvalue).nonEmpty
      def disjoint(avalue: Set[T], pvalue: Set[T]): Boolean  = avalue.intersect(pvalue).isEmpty
      def contains(avalue: Set[T], pvalue: Set[T])(implicit ev: Set[T] <:< Iterable[_]): Boolean =
        avalue.intersect(pvalue).size == pvalue.size
      def contains(avalue: Any, pvalue: Set[T]): Boolean = avalue match {
        case avalue: Set[T] => avalue.intersect(pvalue).size == pvalue.size
        case _              => false
      }
      def within(avalue: Set[T], pvalue: Set[T]): Boolean =
        pvalue.intersect(avalue).size == pvalue.size
    }
    implicit def listsetHelper[T] = new CollectionHelper[ListSet[T]] {
      def intersect(avalue: ListSet[T], pvalue: ListSet[T]): Boolean =
        avalue.intersect(pvalue).nonEmpty
      def disjoint(avalue: ListSet[T], pvalue: ListSet[T]): Boolean =
        avalue.intersect(pvalue).isEmpty
      def contains(avalue: ListSet[T], pvalue: ListSet[T])(implicit ev: ListSet[T] <:< Iterable[_]): Boolean =
        avalue.toList.containsSlice(pvalue.toList)
      def contains(avalue: Any, pvalue: ListSet[T]): Boolean = avalue match {
        case avalue: ListSet[_] => avalue.toList.containsSlice(pvalue.toList)
        case _                  => false
      }
      def within(avalue: ListSet[T], pvalue: ListSet[T]): Boolean =
        pvalue.toList.containsSlice(avalue.toList)
    }
    implicit def vectorHelper[T] = new CollectionHelper[Vector[T]] {
      def intersect(avalue: Vector[T], pvalue: Vector[T]): Boolean =
        avalue.intersect(pvalue).nonEmpty
      def disjoint(avalue: Vector[T], pvalue: Vector[T]): Boolean = avalue.intersect(pvalue).isEmpty
      def contains(avalue: Vector[T], pvalue: Vector[T])(implicit ev: Vector[T] <:< Iterable[_]): Boolean =
        avalue.containsSlice(pvalue)
      def contains(avalue: Any, pvalue: Vector[T]): Boolean = avalue match {
        case avalue: Vector[_] => avalue.containsSlice(pvalue)
        case _                 => false
      }
      def within(avalue: Vector[T], pvalue: Vector[T]): Boolean = pvalue.containsSlice(avalue)
    }
    //    implicit def mapHelper[K,V] = new CollectionHelper[Map[K,V]] {
    //      def intersect(avalue: Map[K,V], pvalue: Map[K,V]): Boolean = avalue.intersect(pvalue).nonEmpty
    //      def disjoint(avalue: Map[K,V], pvalue: Map[K,V]): Boolean = avalue.intersect(pvalue).isEmpty
    //      def contains(avalue: Map[K,V], pvalue: Map[K,V]): Boolean = avalue.containsSlice(pvalue)
    //      def within(avalue: Map[K,V], pvalue: Map[K,V]): Boolean = pvalue.containsSlice(avalue)
    //    }

  }

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
  def within[T](value: T, values: T*): Within[T] = p.Within(value :: values.toList)
  //  def within[T: ObjectHelper](value: T): GeoWithin[T] = p.GeoWithin(value)
  //  def without(values: List[Any]): P[Any] = without(values.toSet)
  def without[T](value: T, values: T*): Without[T] = p.Without(value :: values.toList)
  def intersect[T: ObjectHelper, T0, TT0 <: ClassType[_]](value: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Intersect[T] = p.Intersect(value)
  def disjoint[T: ObjectHelper, T0, TT0 <: ClassType[_]](value: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Disjoint[T] = p.Disjoint(value)
  //  def contains[T: ObjectHelper](value: T): GeoContains[T] = p.GeoContains(value)
  def contains[T: EqHelper, T0, TT0 <: ClassType[_]](value: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Contains[T] = p.Contains(value)
  def prefix[T: StringHelper, T0, TT0 <: ClassType[_]](value: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Prefix[T] = p.Prefix(value)
  def suffix[T: StringHelper, T0, TT0 <: ClassType[_]](value: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): Suffix[T] = p.Suffix(value)
  def regex(value: scala.util.matching.Regex): Regex         = p.Regex(value)
  def fuzzy[T: StringHelper, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Fuzzy[T] =
    p.Fuzzy(value)
  def containsPrefix[T: StringHelper, T0, TT0 <: ClassType[_]](value: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): ContainsPrefix[T] = p.ContainsPrefix(value)
  def containsRegex(value: scala.util.matching.Regex): ContainsRegex = p.ContainsRegex(value)
  def containsFuzzy[T: StringHelper, T0, TT0 <: ClassType[_]](value: T)(
      implicit ct: ClassTypeable.Aux[T, T0, TT0]): ContainsFuzzy[T] = p.ContainsFuzzy(value)

  //  MemGraphDefault.ns.storeOntology(ontology)

  implicit class WithPredicateHList[K <: HList](p: K)(implicit
                                                      val d: LUBConstraint[K, P[_]]) {

    def eqv[T: EqHelper, Out <: HList, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.eqv(value) :: p
    def neqv[T: EqHelper, Out <: HList, T0, TT0 <: ClassType[_]](value: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.neqv(value) :: p
    def gt[T: OrderHelper, Out <: HList, T0, TT0 <: ClassType[_]](value: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.gt(value) :: p
    def gte[T: OrderHelper, Out <: HList, T0, TT0 <: ClassType[_]](value: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.gte(value) :: p
    def lt[T: OrderHelper, Out <: HList, T0, TT0 <: ClassType[_]](value: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.lt(value) :: p
    def lte[T: OrderHelper, Out <: HList, T0, TT0 <: ClassType[_]](value: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.lte(value) :: p
    def between[T: RangeHelper, Out <: HList, T0, TT0 <: ClassType[_]](lower: T, upper: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.between(lower, upper) :: p
    def outside[T: RangeHelper, Out <: HList, T0, TT0 <: ClassType[_]](lower: T, upper: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.outside(lower, upper) :: p
    def inside[T: RangeHelper, Out <: HList, T0, TT0 <: ClassType[_]](lower: T, upper: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.inside(lower, upper) :: p
    def within[T, Out <: HList](value: T, values: T*) =
      P.within(value, values: _*) :: p
    //    def within[T, Out <: HList](value: T)(implicit prepend: Prepend.Aux[K, GeoWithin[T] :: HNil, Out]) =
    //      P.within(value))
    def without[T, Out <: HList](value: T, values: T*) =
      P.without(value, values: _*) :: p
    def intersect[T: ObjectHelper, Out <: HList, T0, TT0 <: ClassType[_]](value: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.intersect(value) :: p
    def disjoint[T: ObjectHelper, Out <: HList, T0, TT0 <: ClassType[_]](value: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.disjoint(value) :: p
    def contains[T: EqHelper, Out <: HList, T0, TT0 <: ClassType[_]](value: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.contains(value) :: p
    //    def contains[T: ObjectHelper, Out <: HList](value: T)(implicit prepend: Prepend.Aux[K, GeoContains[T] :: HNil, Out]) =
    //      P.contains(value))
    def prefix[T: StringHelper, Out <: HList, T0, TT0 <: ClassType[_]](value: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.prefix(value) :: p
    def suffix[T: StringHelper, Out <: HList, T0, TT0 <: ClassType[_]](value: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.suffix(value) :: p
    def regex[Out <: HList](value: scala.util.matching.Regex) =
      P.regex(value) :: p
    def fuzzy[T: StringHelper, Out <: HList, T0, TT0 <: ClassType[_]](value: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.fuzzy(value) :: p
    def containsPrefix[T: StringHelper, Out <: HList, T0, TT0 <: ClassType[_]](value: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.containsPrefix(value) :: p
    def containsRegex[Out <: HList](value: scala.util.matching.Regex) =
      P.containsRegex(value) :: p
    def containsFuzzy[T: StringHelper, Out <: HList, T0, TT0 <: ClassType[_]](value: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]) =
      P.containsFuzzy(value) :: p
  }

  implicit class WithPredicate[T <: P[_]](p: T) //extends WithPredicateHList(p :: HNil)
  //  implicit def toHList[T <: P[_]](p: T) = p :: HNil
}

trait P[+T] extends Node {
  def assert(avalue: Any): Boolean
}

trait EqP[T] extends P[T] {
  def pvalue: Any
  //  def datatype: DataType[T] = outE(EqP.keys.value).head.inV.types.head
}
object EqP {
  private val ontologyNode =
    MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/p/EqP")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- Property.default.`@extends` --> P.ontology
  ontologyNode --- Property.default.`@label` --> "EqP" --- Property.default.`@language` --> "en"
  ontologyNode --- Property.default.`@comment` --> "A simple predicate" --- Property.default.`@language` --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)

  object keys {
    private val valueNode =
      MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/p/compare/value")
    valueNode.addLabel(Property.ontology)
    valueNode --- Property.default.`@label` --> "value" --- Property.default.`@language` --> "en"
    valueNode --- Property.default.`@comment` --> "Any value" --- Property.default.`@language` --> "en"
    valueNode --- Property.default.`@container` --> NS.types.`@list`
    lazy val value: Property = Property(valueNode)
    //    val typedValue = value.addRange(DataType.nodeType)
    //    val valueString = value + DataType.default.textType
    //    val valueInt = value + DataType.default.intType
    //    val valueDouble = value + DataType.default.doubleType
    //    val valueLong = value + DataType.default.longType
    //    val valueDateTime = value + DataType.default.dateTimeType
    //    val valueDate = value + DataType.default.dateType
    //    //    val valueTime = value + DataTypes.TimeType)
    //    val valueGeo = value + DataType.default.geoType
    //    val valueBoolean = value + DataType.default.boolType
    //    val valueUrl = value + DataType.default.uRLType
    //    val valueEpoch = value + DataType.default.epochType)
  }
  ontologyNode --- Property.default.`@properties` --> keys.value

  //  MemGraphDefault.ns.storeOntology(ontology)
}
trait OrderP[T] extends EqP[T]
object OrderP {}
trait RangeP[T] extends P[T] {
  def lower: T
  def upper: T
}
object RangeP {
  private val ontologyNode =
    MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/p/RangeP")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- Property.default.`@label` --> "RangeP" --- Property.default.`@language` --> "en"
  ontologyNode --- Property.default.`@comment` --> "A compound predicate" --- Property.default.`@language` --> "en"
  ontologyNode --- Property.default.`@extends` --> P.ontology
  lazy val ontology: Ontology = Ontology(ontologyNode)

  object keys {
    private val lowerNode =
      MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/p/range/lower")
    lowerNode.addLabel(Property.ontology)
    lowerNode --- Property.default.`@label` --> "lower" --- Property.default.`@language` --> "en"
    lazy val lower = Property(lowerNode)

    private val upperNode =
      MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/p/range/upper")
    upperNode.addLabel(Property.ontology)
    upperNode --- Property.default.`@label` --> "upper" --- Property.default.`@language` --> "en"
    lazy val upper = Property(upperNode)

    //    val typedValue = value.addRange(DataType.nodeType)
    //    val lowerInt = lower.addRange(lower.graph.intType)
    //    val lowerDouble = lower.addRange(lower.graph.doubleType)
    //    val lowerLong = lower.addRange(lower.graph.longType)
    //    val lowerDateTime = lower.addRange(lower.graph.dateTimeType)
    //    val lowerDate = lower.addRange(lower.graph.dateType)
    //    val lowerTime = lower.addRange(lower.graph.timeType)
    //
    //    val upperInt = upper.addRange(upper.graph.intType)
    //    val upperDouble = upper.addRange(upper.graph.doubleType)
    //    val upperLong = upper.addRange(upper.graph.longType)
    //    val upperDateTime = upper.addRange(upper.graph.dateTimeType)
    //    val upperDate = upper.addRange(upper.graph.dateType)
    //    val upperTime = upper.addRange(upper.graph.timeType)
  }

  ontologyNode --- Property.default.`@properties` --> keys.lower
  ontologyNode --- Property.default.`@properties` --> keys.upper
  //  MemGraphDefault.ns.storeOntology(ontology)
}

trait CollectionP[T] extends P[T] {
  def pvalues: List[T]
}
object CollectionP {
  private val ontologyNode =
    MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/p/CollectionP")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- Property.default.`@label` --> "CollectionP" --- Property.default.`@language` --> "en"
  ontologyNode --- Property.default.`@comment` --> "A complete predicate" --- Property.default.`@language` --> "en"
  ontologyNode --- Property.default.`@extends` --> P.ontology

  lazy val ontology: Ontology = Ontology(ontologyNode)

  object keys {
    private val valueNode = MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/p/compare/values")
    valueNode.addLabel(Property.ontology)
    valueNode --- Property.default.`@extends` --> EqP.keys.value
    valueNode --- Property.default.`@label` --> "values" --- Property.default.`@language` --> "en"
    valueNode --- Property.default.`@comment` --> "Polyglot list of values" --- Property.default.`@language` --> "en"
    valueNode --- Property.default.`@container` --> NS.types.`@set`

    lazy val value  = Property(valueNode)
    lazy val valueP = value + P.ontology //TODO: test nested predicate structures
  }

  ontologyNode --- Property.default.`@properties` --> keys.value
  //  MemGraphDefault.ns.storeOntology(ontology)
}
object ObjectP {
  private val ontologyNode =
    MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/p/ObjectP")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- Property.default.`@label` --> "ObjectP" --- Property.default.`@language` --> "en"
  ontologyNode --- Property.default.`@comment` --> ".." --- Property.default.`@language` --> "en"
  ontologyNode --- Property.default.`@extends` --> P.ontology

  lazy val ontology: Ontology = Ontology(ontologyNode)
}
trait ObjectP[T] extends P[T]

trait PredicateWrapper[+T] {
  //  def ontology: Ontology
  def wrap(node: Node): T
}

abstract class PredicateCompanion(label: String, comment: String = "") {
  protected[traversal] val ontologyNode =
    MemGraphDefault.ns.nodes.upsert(s"sptth/tbd.tld/librarian/p/${label}")
  ontologyNode.addLabel(Ontology.ontology)
  if (label != "")
    ontologyNode --- Property.default.`@label` --> label --- Property.default.`@language` --> "en"
  if (comment != "")
    ontologyNode --- Property.default.`@comment` --> comment --- Property.default.`@language` --> "en"
  //  ontologyNode --- Property.default.comment --> "" --- Property.default.language --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)
}
