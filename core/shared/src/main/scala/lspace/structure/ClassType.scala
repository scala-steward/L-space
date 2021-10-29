package lspace.structure

import lspace.Label.D._
import lspace.datatype._

import scala.collection.immutable.Iterable
import scala.collection.mutable

object ClassType {
  trait DataTypeMatcher {
    def detect[V](value: V): Option[DataType[V]]
  }
//  trait UnionClassType[T <: Coproduct] extends ClassType[T] {}
//  trait UnionClassType[A, B] extends ClassType[A | B] {}

  object matchers {
    private lazy val datatypeMatchers = mutable.HashSet[DataTypeMatcher]()
    def add(matcher: DataTypeMatcher): Unit = datatypeMatchers.synchronized[Unit] {
      datatypeMatchers += matcher
    }
    def +(matcher: DataTypeMatcher): Unit = add(matcher)
    def findDataType[V](value: V): Option[DataType[V]] =
      datatypeMatchers.to(LazyList).map(_.detect(value)).collectFirst { case Some(datatype) => datatype }
  }

  def detect[T](value: T): ClassType[Any] =
//    implicit class WithV(value: Any) {
//      def ct: ClassType[Any] = detect(value)
//    }
    value match {
      case r: IriResource =>
        r match {
          case r: Resource[_] =>
            r match {
              case r: Node       => r.labels.reduceOption[ClassType[Any]](_ + _).getOrElse(Ontology.empty)
              case r: Edge[_, _] => r.key
              case r: Value[_]   => r.label
              case _             => throw new Exception(s"unexpected type ${r.getClass.getSimpleName}")
            }
          case r: ClassType[_] =>
            r match {
              case _: Ontology    => Ontology.ontology
              case _: Property    => Property.ontology
              case _: DataType[_] => DataType.ontology
              case _              => throw new Exception(s"unexpected type ${r.getClass.getSimpleName}")
            }
          case _ => ClassType.empty
        }
      case r => DataType.detect(r)
    }

  object classtypes {
    def all: List[ClassType[_]] =
      List[ClassType[_]]() ++ Ontology.ontologies.all ++ Property.properties.all ++ DataType.datatypes.all
    def get(iri: String): Option[ClassType[_]] =
      DataType.datatypes
        .get(iri)
        .asInstanceOf[Option[ClassType[_]]]
        .orElse(Ontology.ontologies.get(iri))
        .orElse(Property.properties.get(iri))

    def getAndUpdate(node: Node): ClassType[Any] =
      if (node.hasLabel(Ontology.ontology).nonEmpty) {
        if (node.hasLabel(DataType.ontology).nonEmpty) DataType.datatypes.getAndUpdate(node)
        else Ontology.ontologies.getAndUpdate(node)
      } else if (node.hasLabel(Property.ontology).nonEmpty) {
        Property.properties.getAndUpdate(node)
      } else {
        throw new Exception(s"${node.iri} does not look like a classtype ${node.labels
          .map(_.iri)} ${node.outE().map(e => e.key.iri + " " + e.to.prettyPrint)}")
      }

//    def cached(iri: String): Option[ClassType[_]] =
//      Ontology.ontologies.cached(iri).orElse(Property.properties.cached(iri)).orElse(DataType.datatypes.cached(iri))
  }

  def makeT[T]: ClassType[T] = new ClassType[T] {

    val iri: String       = ""
    val iris: Set[String] = Set()

    //    val _extendedClasses: List[_ <: DataType[_]] = List()
//    object extendedClasses {
//      type T = ClassType[Any]
//      def apply(): List[ClassType[Any]]                                  = List()
//      def all(exclude: Set[ClassType[Any]] = Set()): Set[ClassType[Any]] = Set()
//      def contains(iri: String): Boolean                                 = false
//    }
//    val _properties: List[Property]              = List()
//    val base: Option[String] = None

    override def toString: String = s"classtype:$iri"
  }
  // helper, empty iri is used to recognize and filter out this classtype
  lazy val stubAny: ClassType[Any] = makeT[Any]
  lazy val empty: ClassType[Any]   = stubAny

  // helper, empty iri is used to recognize and filter out this classtype
  lazy val stubNothing: ClassType[Nothing] = makeT[Nothing]

//  implicit def clsClasstype[T]: ClassTypeable.Aux[ClassType[T], T, ClassType[T]] = new ClassTypeable[ClassType[T]] {
//    type C  = T
//    type CT = ClassType[T]
//    def ct: CT = default[T]
//  }

  implicit class WithAnyClassType(_ct: ClassType[Any]) {
    lazy val extendedClasses: ExtendedClasses[ClassType[Any]] = new ExtendedClasses[ClassType[Any]] {
      def ct: ClassType[Any] = _ct

      def apply(): List[ClassType[Any]] = ct.extendedClassesList

      /** @param exclude
        *   a classtype set to prevent circular recursion recursively fetches all extended classes (parent of parents)
        * @return
        */
      def all(exclude: Set[ClassType[Any]] = Set.empty[ClassType[Any]]): Set[ClassType[Any]] = {
        val _extends = apply().toSet -- exclude
        _extends ++ (_extends - ct).flatMap(_.extendedClasses.all(_extends ++ exclude))
      }

      def contains(iri: String): Boolean = {
        val _extends = apply().toSet
        _extends.exists(_.iris.contains(iri)) || (_extends - ct)
          .filterNot(_.`extends`(ct))
          .exists(_.extendedClasses.contains(iri))
      }

      def +(parent: => ClassType[Any]): ExtendedClasses[ClassType[Any]] = ct.synchronized {
        ct.extendedClassesList = if (!ct.extendedClassesList.contains(parent)) {
          parent match {
            case d: DataType[Any] => d.extendedBy.+(ct.asInstanceOf[DataType[Any]])
            case p: Property      => p.extendedBy.+(ct.asInstanceOf[Property])
            case o: Ontology      => o.extendedBy.+(ct.asInstanceOf[Ontology])
          }
          (ct.extendedClassesList :+ parent).distinct
        } else {
          ct.extendedClassesList
        }
        this
      }

      def ++(parents: => Iterable[ClassType[Any]]): ExtendedClasses[ClassType[Any]] = ct.synchronized {
        ct.extendedClassesList = (ct.extendedClassesList ++ parents).distinct
        parents.map {
          case d: DataType[Any] => d.extendedBy.+(ct.asInstanceOf[DataType[Any]])
          case p: Property      => p.extendedBy.+(ct.asInstanceOf[Property])
          case o: Ontology      => o.extendedBy.+(ct.asInstanceOf[Ontology])
        }
        this
      }

      def -(parent: => ClassType[Any]): ExtendedClasses[ClassType[Any]] = ct.synchronized {
        ct.extendedClassesList = ct.extendedClassesList.filterNot(_ == parent)
        parent match {
          case d: DataType[Any] => d.extendedBy.-(ct.asInstanceOf[DataType[Any]])
          case p: Property      => p.extendedBy.-(ct.asInstanceOf[Property])
          case o: Ontology      => o.extendedBy.+(ct.asInstanceOf[Ontology])
        }
        this
      }

      def --(parent: => Iterable[ClassType[Any]]): ExtendedClasses[ClassType[Any]] = ct.synchronized {
        ct.extendedClassesList = ct.extendedClassesList.filterNot(parent.toList.contains)
        this
      }
    }
  }

//  implicit class WithClassType[CT[+Z] <: ClassType[Z], T](_ct: CT[T]) {
//    lazy val extendedBy: ExtendedByClasses[CT[T]] = new ExtendedByClasses[CT[T]] {
//      def ct: CT[T] = _ct
//
//      def apply(): List[CT[T]] = ct.extendedByClassesList.asInstanceOf[List[CT[T]]]
//
//      def all(exclude: Set[CT[T]] = Set()): Set[CT[T]] = {
//        val _extends = apply().toSet -- exclude
//        _extends ++ (_extends - ct).flatMap(_.extendedBy.all(_extends ++ exclude))
//      }
//
//      def contains(iri: String): Boolean = {
//        val _extends = apply().toSet
//        _extends.exists(_.iris.contains(iri)) || (_extends - ct)
//          .filterNot(_.`extends`(ct))
//          .exists(_.extendedBy.contains(iri))
//      }
//
//      def +(child: => CT[T]): ExtendedByClasses[CT[T]] = ct.synchronized {
//        ct.extendedByClassesList =
//          if (!ct.extendedByClassesList.contains(child))
//            (ct.extendedByClassesList :+ child).distinct
//          else {
//            ct.extendedByClassesList
//          }
//        this
//      }
//    }
//  }
}

/** @tparam T
  */
trait ClassType[+T] extends IriResource { self =>

  def iris: Set[String] // TODO var iriList: Set[String]
  def `@ids` = iris

  // TODO: improve, explore union-types
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
                  case (ct1: IntType[_], _: IntType[_])       => ct1
                  case (ct1: DoubleType[_], _: DoubleType[_]) => ct1
                  case (ct1: LongType[_], _: LongType[_])     => ct1
                  case _                                      => NumericType.datatype
                }
              case (ct1: CalendarType[_], ct2: CalendarType[_]) =>
                (ct1, ct2) match {
                  case (ct1: DateTimeType[_], _: DateTimeType[_])           => ct1
                  case (ct1: LocalDateTimeType[_], _: LocalDateTimeType[_]) => ct1
                  case (ct1: LocalTimeType[_], _: LocalTimeType[_])         => ct1
                  case (ct1: LocalDateType[_], _: LocalDateType[_])         => ct1
                  case _                                                    => NumericType.datatype
                }
              case (ct1: TextType[_], _: TextType[_]) => ct1
              case (ct1: BoolType[_], _: BoolType[_]) => ct1
              case _                                  => LiteralType.datatype
            }
          case (ct1: StructuredType[_], ct2: StructuredType[_]) => // TODO: improve
            (ct1, ct2) match {
              case (ct1: CollectionType[_], ct2: CollectionType[_]) =>
                (ct1, ct2) match {
                  case (ct1: ListType[_], ct2: ListType[_]) =>
                    (ct1.valueRange ++ ct2.valueRange).reduceOption(_ + _).map(`@list`[Any]).getOrElse(`@list`())
                  case (ct1: ListSetType[_], ct2: ListSetType[_]) =>
                    (ct1.valueRange ++ ct2.valueRange).reduceOption(_ + _).map(`@listset`[Any]).getOrElse(`@listset`())
                  case (ct1: MapType[_], ct2: MapType[_]) =>
                    MapType(
                      (ct1.keyRange ++ ct2.keyRange).reduceOption(_ + _).getOrElse(ClassType.stubAny),
                      (ct1.valueRange ++ ct2.valueRange).reduceOption(_ + _).getOrElse(ClassType.stubAny)
                    )
                  case (ct1: SetType[_], ct2: SetType[_]) => SetType((ct1.valueRange ++ ct2.valueRange).reduce(_ + _))
                  case (ct1: VectorType[_], ct2: VectorType[_]) =>
                    (ct1.valueRange ++ ct2.valueRange).reduceOption(_ + _).map(`@vector`[Any]).getOrElse(`@vector`())
                  case (ct1: OptionType[_], ct2: OptionType[_]) =>
                    (ct1.valueRange ++ ct2.valueRange).reduceOption(_ + _).map(`@option`[Any]).getOrElse(`@option`())
                  case _ => CollectionType.datatype
                }
              case (ct1: GeometricType[_], ct2: GeometricType[_]) =>
                (ct1, ct2) match {
                  case (ct1: GeopointType[_], _: GeopointType[_])                 => ct1
                  case (ct1: GeoMultipointType[_], _: GeoMultipointType[_])       => ct1
                  case (ct1: GeoLineType[_], _: GeoLineType[_])                   => ct1
                  case (ct1: GeoMultiLineType[_], _: GeoMultiLineType[_])         => ct1
                  case (ct1: GeoPolygonType[_], _: GeoPolygonType[_])             => ct1
                  case (ct1: GeoMultiPolygonType[_], _: GeoMultiPolygonType[_])   => ct1
                  case (ct1: GeoMultiGeometryType[_], _: GeoMultiGeometryType[_]) => ct1
                  // TODO: other geo-types
                  case _ => GeometricType.datatype
                }
              case (ct1: QuantityType[_], ct2: QuantityType[_]) =>
                (ct1, ct2) match {
                  case (ct1: DurationType[_], _: DurationType[_]) => ct1
                  case _                                          => QuantityType.datatype
                }
              case (ct1: TupleType[_], ct2: TupleType[_]) =>
                if (ct1.rangeTypes.size == ct2.rangeTypes.size) {
                  TupleType(ct1.rangeTypes.zip(ct2.rangeTypes).map {
                    case (Some(ct1), Some(ct2)) => Some(ct1 + ct2)
                    case (_, _)                 => None
                  })
                } else TupleType.datatype
              case (ct1: ColorType[_], _: ColorType[_]) => ct1 // TODO: wrong
              case _                                    => StructuredType.datatype
            }
          case (ct1: IriType[_], ct2: IriType[_]) =>
            (ct1, ct2) match {
              case (ct1: NodeURLType[_], _: NodeURLType[_])   => ct1
              case (ct1: EdgeURLType[_], _: EdgeURLType[_])   => ct1
              case (ct1: ValueURLType[_], _: ValueURLType[_]) => ct1
              case _                                          => IriType.datatype
            }
          case (ct1: GraphType[_], _: GraphType[_]) => ct1
          case _                                    => DataType.datatype
        }
    case (ct1: Ontology, ct2: Ontology) =>
      if (ct1.`@extends`(ct2)) ct2 else if (ct2.`@extends`(ct1)) ct1 else Ontology.empty
    case (ct1: Property, ct2: Property) =>
      if (ct1.`@extends`(ct2)) ct2 else if (ct2.`@extends`(ct1)) ct1 else Property.empty
    case _ => ClassType.stubAny
  }
//  protected def _properties: List[Property]

  protected def _extendedClasses: List[ClassType[Any]] = Nil

  protected[lspace] var extendedClassesList: List[ClassType[Any]] = _extendedClasses
  //  def extendedClasses: List[ClassType[_]] // = out(DataType.default.EXTENDS).collect { case node: Node => node }.map(ClassType.wrap).asInstanceOf[List[ClassType[_]]]
  /** TODO: deprecate this method by implementing Ontology hierarchy
    * @param classType
    * @return
    */
  def `extends`(classType: ClassType[_]): Boolean = {
    val _extends = this.extendedClasses.all(Set())
    _extends.toList.contains(classType)
//    if (_extends.contains(classType)) true
//    else {
//      var result: Boolean = false
//      val oIt             = _extends /*.filterNot(_.`extends`(this))*/ .reverseIterator
//      while (oIt.hasNext && !result) {
//        result = oIt.next().`extends`(classType)
//      }
//      result
//    }
  }

  /** alternative to `@extends`
    * @param classType
    * @return
    */
  def `@extends`(classType: ClassType[_]): Boolean = `extends`(classType)

  /** alternative to `@extends`
    * @param classType
    * @return
    */
  def <:<(classType: ClassType[_]): Boolean = `extends`(classType)

  /** TODO: create hash-tree for faster evaluation
    * @return
    */
//  def extendedClasses: List[ClassType[Any]] // = _extendedClasses()
//  trait Extends {
//    def apply(): List[ClassType[Any]]
//    def apply(iri: String): Boolean
//  }

  protected[lspace] var extendedByClassesList: List[ClassType[Any]] = List()

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

trait ExtendedClasses[T] {
  def ct: T
  def apply(): List[T]
  def all(exclude: Set[T] = Set.empty[T]): Set[T]
  def contains(iri: String): Boolean

  def +(parent: => T): ExtendedClasses[T]
  def ++(parents: => Iterable[T]): ExtendedClasses[T]
  def -(parent: => T): ExtendedClasses[T]
  def --(parent: => Iterable[T]): ExtendedClasses[T]
}

trait ExtendedByClasses[T] {
  def ct: T
  def apply(): List[T]
  def all(exclude: Set[T] = Set.empty[T]): Set[T]
  def contains(iri: String): Boolean

  def +(child: => T): ExtendedByClasses[T]
//  def ++(parents: => Iterable[T]): ExtendedByClasses[T]
  def -(parent: => T): ExtendedByClasses[T]
//  def --(parent: => Iterable[T]): ExtendedByClasses[T]
}
