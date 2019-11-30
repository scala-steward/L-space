package lspace.structure

import lspace.datatype._
import lspace.Label.D._
import lspace.types.geo._
import monix.eval.{Coeval, Task}
import shapeless.Coproduct

import scala.collection.immutable.{Iterable, ListSet}
import scala.collection.mutable

object ClassType {
  trait DataTypeMatcher {
    def detect[V](value: V): Option[DataType[V]]
  }
//  trait UnionClassType[T <: Coproduct] extends ClassType[T] {}
//  trait UnionClassType[A, B] extends ClassType[A | B] {}

  object matchers {
    private lazy val datatypeMatchers = mutable.HashSet[DataTypeMatcher]()
    def add(matcher: DataTypeMatcher): Unit = datatypeMatchers.synchronized {
      datatypeMatchers += matcher
    }
    def +(matcher: DataTypeMatcher): Unit = add(matcher)
    def findDataType[V](value: V): Option[DataType[V]] =
      datatypeMatchers.toStream.map(_.detect(value)).collectFirst { case Some(datatype) => datatype }
  }

  def detect[T](value: T): ClassType[Any] = {
    implicit class WithV(value: Any) {
      def ct: ClassType[Any] = detect(value)
    }
    value match {
      case r: IriResource =>
        r match {
          case r: Resource[_] =>
            r match {
              case r: Node       => r.labels.reduceOption[ClassType[Any]](_ + _).getOrElse(Ontology.empty)
              case r: Edge[_, _] => r.key
              case r: Value[_]   => r.label
            }
          case r: ClassType[_] =>
            r match {
              case r: Ontology    => Ontology.ontology
              case r: Property    => Property.ontology
              case r: DataType[_] => DataType.ontology
            }
          case _ => ClassType.empty
        }
      case r => DataType.detect(r)
    }
  }

  object classtypes {
    def all: List[ClassType[_]] =
      List[ClassType[_]]() ++ Ontology.ontologies.all ++ Property.properties.all ++ DataType.datatypes.all
    def get(iri: String): Option[ClassType[_]] = {
      DataType.datatypes
        .get(iri)
        .asInstanceOf[Option[ClassType[_]]]
        .orElse(Ontology.ontologies.get(iri))
        .orElse(Property.properties.get(iri))
    }

    def getAndUpdate(node: Node): ClassType[Any] = {
      if (node.hasLabel(Ontology.ontology).nonEmpty) {
        if (node.hasLabel(DataType.ontology).nonEmpty) DataType.datatypes.getAndUpdate(node)
        else Ontology.ontologies.getAndUpdate(node)
      } else if (node.hasLabel(Property.ontology).nonEmpty) {
        Property.properties.getAndUpdate(node)
      } else {
        throw new Exception(s"${node.iri} does not look like a classtype ${node.labels
          .map(_.iri)} ${node.outE().map(e => e.key.iri + " " + e.to.prettyPrint)}")
      }
    }

//    def cached(iri: String): Option[ClassType[_]] =
//      Ontology.ontologies.cached(iri).orElse(Property.properties.cached(iri)).orElse(DataType.datatypes.cached(iri))
  }

  def makeT[T]: ClassType[T] = new ClassType[T] {

    val iri: String       = ""
    val iris: Set[String] = Set()

    //    val _extendedClasses: List[_ <: DataType[_]] = List()
    object extendedClasses {
      def apply(): List[ClassType[Any]] = List()
      def apply(iri: String): Boolean   = false
    }
//    val _properties: List[Property]              = List()
//    val base: Option[String] = None

    override def toString: String = s"classtype:$iri"
  }
  //helper, empty iri is used to recognize and filter out this classtype
  lazy val stubAny: ClassType[Any] = makeT[Any]
  lazy val empty: ClassType[Any]   = stubAny

  //helper, empty iri is used to recognize and filter out this classtype
  lazy val stubNothing: ClassType[Nothing] = makeT[Nothing]

//  implicit def clsClasstype[T]: ClassTypeable.Aux[ClassType[T], T, ClassType[T]] = new ClassTypeable[ClassType[T]] {
//    type C  = T
//    type CT = ClassType[T]
//    def ct: CT = default[T]
//  }

}

/**
  *
  * @tparam T
  */
trait ClassType[+T] extends IriResource {

  def iris: Set[String] //TODO var iriList: Coeval[Set[String]]
  def `@ids` = iris

  //TODO: improve, explore union-types
  def +[T1](ct: ClassType[T1]): ClassType[Any] = (this, ct) match {
    case (ct1: DataType[_], ct2: DataType[_]) =>
      if (ct1.`@extends`(ct2)) ct2
      else if (ct2.`@extends`(ct1)) ct1
      else
        (ct1, ct2) match {
          case (ct1: LiteralType[_], ct2: LiteralType[_]) =>
            (ct1, ct2) match {
              case (ct1: NumericType[_], ct2: NumericType[_]) =>
                (ct1, ct2) match {
                  case (ct1: IntType[_], ct2: IntType[_])       => ct1
                  case (ct1: DoubleType[_], ct2: DoubleType[_]) => ct1
                  case (ct1: LongType[_], ct2: LongType[_])     => ct1
                  case _                                        => NumericType.datatype
                }
              case (ct1: CalendarType[_], ct2: CalendarType[_]) =>
                (ct1, ct2) match {
                  case (ct1: DateTimeType[_], ct2: DateTimeType[_])           => ct1
                  case (ct1: LocalDateTimeType[_], ct2: LocalDateTimeType[_]) => ct1
                  case (ct1: LocalTimeType[_], ct2: LocalTimeType[_])         => ct1
                  case (ct1: LocalDateType[_], ct2: LocalDateType[_])         => ct1
                  case _                                                      => NumericType.datatype
                }
              case (ct1: TextType[_], ct2: TextType[_]) => ct1
              case (ct1: BoolType[_], ct2: BoolType[_]) => ct1
              case _                                    => LiteralType.datatype
            }
          case (ct1: StructuredType[_], ct2: StructuredType[_]) => //TODO: improve
            (ct1, ct2) match {
              case (ct1: CollectionType[_], ct2: CollectionType[_]) =>
                (ct1, ct2) match {
                  case (ct1: ListType[_], ct2: ListType[_]) =>
                    ct1.valueRange ++ ct2.valueRange reduceOption (_ + _) map (`@list`(_)) getOrElse (`@list`())
                  case (ct1: ListSetType[_], ct2: ListSetType[_]) =>
                    ct1.valueRange ++ ct2.valueRange reduceOption (_ + _) map (`@listset`(_)) getOrElse (`@listset`())
                  case (ct1: MapType[_], ct2: MapType[_]) =>
                    MapType(
                      ct1.keyRange ++ ct2.keyRange reduceOption (_ + _) getOrElse (ClassType.stubAny),
                      ct1.valueRange ++ ct2.valueRange reduceOption (_ + _) getOrElse (ClassType.stubAny)
                    )
                  case (ct1: SetType[_], ct2: SetType[_]) => SetType(ct1.valueRange ++ ct2.valueRange reduce (_ + _))
                  case (ct1: VectorType[_], ct2: VectorType[_]) =>
                    ct1.valueRange ++ ct2.valueRange reduceOption (_ + _) map (`@vector`(_)) getOrElse (`@vector`())
                  case (ct1: OptionType[_], ct2: OptionType[_]) =>
                    ct1.valueRange ++ ct2.valueRange reduceOption (_ + _) map (`@option`(_)) getOrElse (`@option`())
                  case _ => CollectionType.datatype
                }
              case (ct1: GeometricType[_], ct2: GeometricType[_]) =>
                (ct1, ct2) match {
                  case (ct1: GeopointType[_], ct2: GeopointType[_])                 => ct1
                  case (ct1: GeoMultipointType[_], ct2: GeoMultipointType[_])       => ct1
                  case (ct1: GeoLineType[_], ct2: GeoLineType[_])                   => ct1
                  case (ct1: GeoMultiLineType[_], ct2: GeoMultiLineType[_])         => ct1
                  case (ct1: GeoPolygonType[_], ct2: GeoPolygonType[_])             => ct1
                  case (ct1: GeoMultiPolygonType[_], ct2: GeoMultiPolygonType[_])   => ct1
                  case (ct1: GeoMultiGeometryType[_], ct2: GeoMultiGeometryType[_]) => ct1
                  //TODO: other geo-types
                  case _ => GeometricType.datatype
                }
              case (ct1: QuantityType[_], ct2: QuantityType[_]) =>
                (ct1, ct2) match {
                  case (ct1: DurationType[_], ct2: DurationType[_]) => ct1
                  case _                                            => QuantityType.datatype
                }
              case (ct1: TupleType[_], ct2: TupleType[_]) =>
                if (ct1.rangeTypes.size == ct2.rangeTypes.size) {
                  TupleType(ct1.rangeTypes.zip(ct2.rangeTypes).map {
                    case (ct1, ct2) if ct1.isEmpty || ct2.isEmpty => None
                    case (Some(ct1), Some(ct2))                   => Some(ct1 + ct2)
                  })
                } else TupleType.datatype
              case (ct1: ColorType[_], ct2: ColorType[_]) => ct1 //TODO: wrong
              case _                                      => StructuredType.datatype
            }
          case (ct1: IriType[_], ct2: IriType[_]) =>
            (ct1, ct2) match {
              case (ct1: NodeURLType[_], ct2: NodeURLType[_])   => ct1
              case (ct1: EdgeURLType[_], ct2: EdgeURLType[_])   => ct1
              case (ct1: ValueURLType[_], ct2: ValueURLType[_]) => ct1
              case _                                            => IriType.datatype
            }
          case (ct1: GraphType[_], ct2: GraphType[_]) => ct1
          case _                                      => DataType.datatype
        }
    case (ct1: Ontology, ct2: Ontology) =>
      if (ct1.`@extends`(ct2)) ct2 else if (ct2.`@extends`(ct1)) ct1 else Ontology.empty
    case (ct1: Property, ct2: Property) =>
      if (ct1.`@extends`(ct2)) ct2 else if (ct2.`@extends`(ct1)) ct1 else Property.empty
    case _ => ClassType.stubAny
  }
//  protected def _properties: List[Property]

//  protected def _extendedClasses: List[_ <: ClassType[_]]

  //  def extendedClasses: List[ClassType[_]] // = out(DataType.default.EXTENDS).collect { case node: Node => node }.map(ClassType.wrap).asInstanceOf[List[ClassType[_]]]
  /**
    * TODO: deprecate this method by implementing Ontology hierarchy
    * @param classType
    * @return
    */
  def `extends`(classType: ClassType[_]): Boolean = {
    val _extends = extendedClasses()
    if (_extends.contains(classType)) true
    else {
      var result: Boolean = false
      val oIt             = _extends /*.filterNot(_.`extends`(this))*/ .reverseIterator
      while (oIt.hasNext && !result) {
        result = oIt.next().`extends`(classType)
      }
      result
    }
  }

  /**
    * alternative to `@extends`
    * @param classType
    * @return
    */
  def `@extends`(classType: ClassType[_]): Boolean = `extends`(classType)

  /**
    * alternative to `@extends`
    * @param classType
    * @return
    */
  def <:<(classType: ClassType[_]): Boolean = `extends`(classType)

//  @deprecated(s"migrate to properties(iri: String)")
//  def property(iri: String): Option[Property]    = properties(iri)
//  def `@property`(iri: String): Option[Property] = properties(iri)

  protected var propertiesList
    : Coeval[Set[Property]] = Coeval(Set[Property]()).memoizeOnSuccess //_properties().toSet ++ extendedClasses.flatMap(_.properties)
  object properties {
    def apply(): Set[Property] = propertiesList()
    def apply(iri: String): Option[Property] = propertiesList().find(_.iris.contains(iri)).orElse {
      var result: Option[Property] = None
      val oIt                      = extendedClasses().reverseIterator
      while (oIt.hasNext && result.isEmpty) {
        result = oIt.next().properties(iri)
      }
      result
    }
    def +(property: => Property): this.type = this.synchronized {
      propertiesList = propertiesList.map(_ + property).memoizeOnSuccess
      this
    }
    def ++(properties: => Iterable[Property]): this.type = this.synchronized {
      propertiesList = propertiesList.map(_ ++ properties).memoizeOnSuccess
      this
    }
    def -(property: => Property): this.type = this.synchronized {
      propertiesList = propertiesList.map(_ - property).memoizeOnSuccess
      this
    }
    def --(properties: => Iterable[Property]): this.type = this.synchronized {
      propertiesList = propertiesList.map(_ -- properties).memoizeOnSuccess
      this
    }
  }
  def `@properties` = properties

  /**
    * TODO: create hash-tree for faster evaluation
    * @return
    */
//  def extendedClasses: List[ClassType[Any]] // = _extendedClasses()
//  trait Extends {
//    def apply(): List[ClassType[Any]]
//    def apply(iri: String): Boolean
//  }
  def extendedClasses: {
    def apply(): List[ClassType[Any]]
    def apply(iri: String): Boolean
  }

  protected lazy val labelMap: scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()
  object label {
    def apply(): Map[String, String] = labelMap.toMap
    def apply(iri: String)           = labelMap.get(iri)
    def +(language: String = "en", label: String): this.type = this.synchronized {
      labelMap += (language -> label)
      this
    }
    def ++(label: Map[String, String]): this.type = this.synchronized {
      labelMap ++= label
      this
    }
  }
  def `@label` = label

  protected lazy val commentMap: scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()
  object comment {
    def apply(): Map[String, String] = commentMap.toMap
    def apply(iri: String)           = commentMap.get(iri)
    def +(language: String = "en", comment: String): this.type = this.synchronized {
      commentMap += (language -> comment)
      this
    }
    def ++(label: Map[String, String]): this.type = this.synchronized {
      commentMap ++= label
      this
    }
  }
  def `@comment` = comment

//  def base: Option[String]
//  def `@base` = base
}
