package lspace.datatype

import lspace.NS
import lspace.structure.ClassType.matchers
import lspace.structure._
import lspace.structure.util.ClassTypeable
import lspace.types.geo._
import monix.eval.Coeval

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.concurrent.ConcurrentHashMap
import scala.collection.concurrent
import scala.collection.immutable.{Iterable, ListSet}
import scala.jdk.CollectionConverters._

object DataType
//    extends OntologyDef(
//      NS.types.`@datatype`,
//      Set(NS.types.`@datatype`, NS.types.schemaDataType, "https://schema.org/DataType"),
//      NS.types.`@datatype`,
//      `@extends` =  Ontology.ontology :: Nil
//    )
{

  lazy val ontology: Ontology = {
    val ontology = new Ontology(
      NS.types.`@datatype`,
      Set(NS.types.`@datatype`, NS.types.schemaDataType, "https://schema.org/DataType")
    )
    ontology.iris.foreach(Ontology.ontologies.byIri.update(_, ontology))
    ontology.extendedClasses + Ontology.ontology
    ontology
  }

  object keys

  lazy val datatype: DataType[Any] = new DataType[Any] {
    val iri: String                = NS.types.`@datatype`
    override val iris: Set[String] = Set(NS.types.schemaDataType)
    labelMap ++= Map("en" -> NS.types.`@datatype`)
  }

//  def wrapped[T]: DataType[DataType[T]] =
  def urlType[T]: IriType[T] = new IriType[T] {
    val iri: String = NS.types.`@datatype`
  }

  def detect[T](value: T): DataType[T] = {
    implicit class WithV[V](value: V) {
      def ct: ClassType[Any] = ClassType.detect(value)
    }
    (value match {
      case r: IriResource =>
        r match {
          case r: Resource[_] =>
            r match {
              case _: Node       => throw new Exception(s"a Node is not an instance of DataType")
              case _: Edge[_, _] => throw new Exception(s"an Edge is not an instance of DataType")
              case _: Value[_]   => DataType.default.`@valueURL`
              case _             => throw new Exception(s"invalid type ${r.getClass.getSimpleName}")
            }
          case _: ClassType[_] => throw new Exception(s"a ClassType is not an instance of DataType")
          case _               => DataType.default.`@url`
        }
      case _: String        => DataType.default.`@string`
      case _: Int           => DataType.default.`@int`
      case _: Double        => DataType.default.`@double`
      case _: Long          => DataType.default.`@long`
      case _: Instant       => DataType.default.`@datetime`
      case _: LocalDateTime => DataType.default.`@localdatetime`
      case _: LocalDate     => DataType.default.`@date`
      case _: LocalTime     => DataType.default.`@time`
      //      case _: Time          => DataType.default.`@duration`
      case _: Boolean => DataType.default.`@boolean`
      case v: Geometry =>
        v match {
          case _: Point         => DataType.default.`@geopoint`
          case _: MultiPoint    => DataType.default.`@geomultipoint`
          case _: Line          => DataType.default.`@geoline`
          case _: MultiLine     => DataType.default.`@geomultiline`
          case _: Polygon       => DataType.default.`@geopolygon`
          case _: MultiPolygon  => DataType.default.`@geomultipolygon`
          case _: MultiGeometry => DataType.default.`@geomultigeo`
          case _                => DataType.default.`@geo`
        }
      case v: Iterable[_] =>
        v match {
          case v: Map[_, _] =>
            if (v.nonEmpty)
              DataType.default.mapType(v.keys.toList.map(_.ct).reduce(_ + _), v.values.toList.map(_.ct).reduce(_ + _))
            else
              DataType.default.mapType() //TODO: recursively map nested values to map -> toList.distinct ?
          case v: ListSet[_] =>
            if (v.nonEmpty) DataType.default.listsetType(v.toList.map(_.ct).reduce(_ + _))
            else DataType.default.listsetType()
          case v: List[_] =>
            if (v.nonEmpty) DataType.default.listType(v.toList.map(_.ct).reduce(_ + _)) else DataType.default.listType()
          case v: Set[_] =>
            if (v.nonEmpty) DataType.default.setType(v.toList.map(_.ct).reduce(_ + _)) else DataType.default.setType()
          case v: Vector[_] =>
            if (v.nonEmpty) DataType.default.vectorType(v.toList.map(_.ct).reduce(_ + _))
            else DataType.default.vectorType()
          case _ => throw new Exception(s"invalid type ${v.getClass.getSimpleName}")
        }
      case v: Product =>
        v match {
//          case v: (_)                      => TupleType(List(Some(v.ct)))
          case v: (_, _)       => TupleType(List(Some(v._1.ct), Some(v._2.ct)))
          case v: (_, _, _)    => TupleType(List(Some(v._1.ct), Some(v._2.ct), Some(v._3.ct)))
          case v: (_, _, _, _) => TupleType(List(Some(v._1.ct), Some(v._2.ct), Some(v._3.ct), Some(v._4.ct)))
          case v: (_, _, _, _, _) =>
            TupleType(List(Some(v._1.ct), Some(v._2.ct), Some(v._3.ct), Some(v._4.ct), Some(v._5.ct)))
          case v: (_, _, _, _, _, _) =>
            TupleType(List(Some(v._1.ct), Some(v._2.ct), Some(v._3.ct), Some(v._4.ct), Some(v._5.ct), Some(v._6.ct)))
          case v: (_, _, _, _, _, _, _) =>
            TupleType(
              List(
                Some(v._1.ct),
                Some(v._2.ct),
                Some(v._3.ct),
                Some(v._4.ct),
                Some(v._5.ct),
                Some(v._6.ct),
                Some(v._7.ct)
              )
            )
          case v: (_, _, _, _, _, _, _, _) =>
            TupleType(
              List(
                Some(v._1.ct),
                Some(v._2.ct),
                Some(v._3.ct),
                Some(v._4.ct),
                Some(v._5.ct),
                Some(v._6.ct),
                Some(v._7.ct),
                Some(v._8.ct)
              )
            )
          case v: (_, _, _, _, _, _, _, _, _) =>
            TupleType(
              List(
                Some(v._1.ct),
                Some(v._2.ct),
                Some(v._3.ct),
                Some(v._4.ct),
                Some(v._5.ct),
                Some(v._6.ct),
                Some(v._7.ct),
                Some(v._8.ct),
                Some(v._9.ct)
              )
            )
          case v: (_, _, _, _, _, _, _, _, _, _) =>
            TupleType(
              List(
                Some(v._1.ct),
                Some(v._2.ct),
                Some(v._3.ct),
                Some(v._4.ct),
                Some(v._5.ct),
                Some(v._6.ct),
                Some(v._7.ct),
                Some(v._8.ct),
                Some(v._9.ct),
                Some(v._10.ct)
              )
            )
          case v: (_, _, _, _, _, _, _, _, _, _, _) =>
            TupleType(
              List(
                Some(v._1.ct),
                Some(v._2.ct),
                Some(v._3.ct),
                Some(v._4.ct),
                Some(v._5.ct),
                Some(v._6.ct),
                Some(v._7.ct),
                Some(v._8.ct),
                Some(v._9.ct),
                Some(v._10.ct),
                Some(v._11.ct)
              )
            )
          case _ => throw new Exception(s"invalid type ${v.getClass.getSimpleName}")
        }
      case v =>
        matchers.findDataType(v).getOrElse(throw new Exception(s"not a known range ${value.getClass}"))
    }).asInstanceOf[DataType[T]]
  }

  object datatypes {

    /** imcomplete...
      */
    object default {
      import DataType.default._
      val datatypes = List(
        `@literal`,
        `@string`,
        `@number`,
        `@int`,
        `@double`,
        `@long`,
        `@temporal`,
        `@date`,
        `@datetime`,
        `@localdatetime`,
        `@time`,
//        `@duration`,
        `@boolean`,
        `@geo`,
        `@geopoint`,
        `@geoline`,
        `@geomultipoint`,
        `@geomultiline`,
        `@geopolygon`,
        `@geomultipolygon`,
        `@geomultigeo`,
        `@graph`,
        `@url`,
        `@nodeURL`,
        `@edgeURL`,
        `@valueURL`,
        ListType.datatype,
        ListSetType.datatype,
        SetType.datatype,
        VectorType.datatype,
        MapType.datatype,
        TupleType.datatype
        //      `@class`,
        //      `@property`,
        //      `@datatype`
      )
      if (datatypes.size > 99) throw new Exception("extend default-datatype-id range!")
      val byId    = (0L to datatypes.size - 1).toList.zip(datatypes).toMap
      val byIri   = byId.toList.flatMap { case (_, dt) => (dt.iri :: dt.iris.toList).map(_ -> dt) }.toMap
      val idByIri = byId.toList.flatMap { case (id, dt) => (dt.iri :: dt.iris.toList).map(_ -> id) }.toMap
    }
    private[lspace] val byIri: concurrent.Map[String, DataType[_]] =
      new ConcurrentHashMap[String, DataType[_]]().asScala
    private[lspace] val building: concurrent.Map[String, Coeval[DataType[_]]] =
      new ConcurrentHashMap[String, Coeval[DataType[_]]]().asScala

    def all: List[DataType[_]] = byIri.values.toList.distinct
//    def get(iri: String): Option[Coeval[DataType[_]]] =
//      default.byIri
//        .get(iri)
//        .orElse(byIri.get(iri))
//        .map(o => Coeval.now(o))
//        .orElse(building.get(iri))
    def get(iri: String, iris: Set[String] = Set()): Option[DataType[_]] = {
      val allIris = iris + iri
      allIris.flatMap(iri => default.byIri.get(iri).orElse(byIri.get(iri))).toList match {
        case List(datatype) => Some(datatype)
        case Nil            => None
        case datatypes =>
          scribe.warn(
            "It looks like multiple datatypes which have some @id's in common are found, this should not happen..."
          )
          datatypes.headOption
      }
    }
    def getOrCreate(iri: String, iris: Set[String]): DataType[_] = get(iri, iris).getOrElse {
      synchronized {
        get(iri, iris).getOrElse {
          val datatype = (iris + iri).flatMap(CollectionType.get).toList match {
            case List(datatype) => datatype
            case Nil            => throw new Exception(s"could not build collectiontype for @id's ${iris + iri}")
            case datatypes =>
              scribe.warn(
                "It looks like multiple datatypes which have some @id's in common are found, this should not happen..."
              )
              datatypes.head
          }
          datatype.iris.foreach(byIri.update(_, datatype))
          datatype.extendedClasses.all() //force eagerly init of extended classes
//          datatype.properties()          //force eagerly init of associated properties
          datatype
        }
      }
    }
    def getAndUpdate(node: Node): DataType[_] = {
      val datatype = getOrCreate(node.iri, node.iris)

      datatype.label ++ node
        .outE(Property.default.typed.labelString)
        .flatMap { edge =>
          val l = edge.out(Property.default.typed.languageString)
          if (l.nonEmpty) l.map(_ -> edge.to.value)
          else List("en"          -> edge.to.value)
        }
        .toMap
      datatype.comment ++ node
        .outE(Property.default.typed.commentString)
        .flatMap { edge =>
          val l = edge.out(Property.default.typed.commentString)
          if (l.nonEmpty) l.map(_ -> edge.to.value)
          else List("en"          -> edge.to.value)
        }
        .toMap

//      datatype.properties ++ (node
//        .out(Property.default.typed.propertyProperty) ++ node
//        .in(lspace.NS.types.schemaDomainIncludes)
//        .collect { case node: Node => node })
//        .filter(_.labels.contains(Property.ontology))
//        .map(Property.properties.getAndUpdate)

      datatype.extendedClasses.++(
        node
          .out(Property.default.`@extends`)
//        .headOption
          .collect {
            case nodes: List[_] =>
              nodes.collect {
                case node: Node if node.hasLabel(DataType.ontology).isDefined =>
                  datatypes
                    .get(node.iri)
                    .getOrElse {
                      datatypes.getAndUpdate(node)
                    } //orElse???
                case iri: String =>
                  datatypes
                    .get(iri)
                    .getOrElse(throw new Exception("@extends looks like an iri but cannot be wrapped by a property"))
              }
            case node: Node if node.hasLabel(DataType.ontology).isDefined =>
              List(datatypes.get(node.iri).getOrElse(datatypes.getAndUpdate(node)))
          }
          .toList
          .flatten
      )

      datatype
    }

//    def cache(datatype: DataType[_]): Unit = {
//      byIri += datatype.iri -> datatype
//      datatype.iris.foreach { iri =>
//        datatypes.byIri += iri -> datatype
//      }
//    }
    def cached(long: Long): Option[DataType[_]] = default.byId.get(long)
//    def cached(iri: String): Option[DataType[_]] = default.byIri.get(iri).orElse(byIri.get(iri))

//    def remove(iri: String): Unit = byIri.remove(iri)
  }

  implicit def clsDatatype[T]: ClassTypeable.Aux[DataType[T], T, DataType[T]] = new ClassTypeable[DataType[T]] {
    type C  = T
    type CT = DataType[T]
    def ct: CT = new DataType[T] { val iri: String = "" }
  }

  object default {
    val `@url` = IriType.datatype

    val `@nodeURL`  = NodeURLType.datatype
    val `@edgeURL`  = EdgeURLType.datatype
    val `@valueURL` = ValueURLType.datatype
    val `@class`    = Ontology.urlType
    val `@property` = Property.urlType
    val `@datatype` = DataType.urlType[DataType[Any]]

    val `@literal`: LiteralType[Any]   = LiteralType.datatype
    val `@string`                      = TextType.datatype
    val `@number`: NumericType[AnyVal] = NumericType.datatype
    val `@int`                         = IntType.datatype
    val `@double`                      = DoubleType.datatype
    val `@long`                        = LongType.datatype
    val `@date`                        = LocalDateType.datatype
    val `@datetime`                    = DateTimeType.datatype
    val `@localdatetime`               = LocalDateTimeType.datatype
    val `@time`                        = LocalTimeType.datatype
    val `@temporal`: CalendarType[Any] = CalendarType.datatype
    val `@duration`: DurationType[Any] = DurationType.datatype
    val `@quantity`: QuantityType[Any] = QuantityType.datatype
    //  val epochType: EpochType = EpochType
    val `@boolean` = BoolType.datatype

    val `@geo`                   = GeometricType.datatype
    val `@geopoint`              = GeopointType.datatype
    val `@geomultipoint`         = GeoMultipointType.datatype
    val `@geoline`               = GeoLineType.datatype
    val `@geomultiline`          = GeoMultiLineType.datatype
    val `@geopolygon`            = GeoPolygonType.datatype
    val `@geomultipolygon`       = GeoMultiPolygonType.datatype
    val `@geomultigeo`           = GeoMultiGeometryType.datatype
    val `@color`: ColorType[Any] = ColorType.datatype
    val `@graph`                 = GraphType.datatype

    val `@structured` = StructuredType.datatype

    def `@option`[V](ct: ClassType[V]): OptionType[Option[V]] = optionType(ct)
    def optionType[V](ct: ClassType[V]): OptionType[Option[V]] =
      OptionType(ct.asInstanceOf[ClassType[V]])
    def `@option`[T](implicit tpe: ClassTypeable[T]): OptionType[T] = optionType(tpe)
    def optionType[T](implicit tpe: ClassTypeable[T]): OptionType[T] =
      OptionType(tpe.ct.asInstanceOf[ClassType[T]]).asInstanceOf[OptionType[T]]
    def `@option`(): OptionType[Option[Any]]                  = OptionType()
    def optionType(): OptionType[Option[Any]]                 = OptionType()
    def `@vector`[V](ct: ClassType[V]): VectorType[Vector[V]] = vectorType(ct)
    def vectorType[V](ct: ClassType[V]): VectorType[Vector[V]] =
      VectorType(ct.asInstanceOf[ClassType[V]])
    def `@vector`[T](implicit tpe: ClassTypeable[T]): VectorType[Vector[T]] = vectorType(tpe)
    def vectorType[T](implicit tpe: ClassTypeable[T]): VectorType[Vector[T]] =
      VectorType(tpe.ct.asInstanceOf[ClassType[T]]).asInstanceOf[VectorType[Vector[T]]]
    def `@vector`(): VectorType[Vector[Any]]            = VectorType()
    def vectorType(): VectorType[Vector[Any]]           = VectorType()
    def `@list`[V](ct: ClassType[V]): ListType[List[V]] = listType(ct)
    def listType[V](ct: ClassType[V]): ListType[List[V]] =
      ListType(ct.asInstanceOf[ClassType[V]])
    def `@list`[T](implicit tpe: ClassTypeable[T]): ListType[List[T]] = listType(tpe)
    def listType[T](implicit tpe: ClassTypeable[T]): ListType[List[T]] =
      ListType(tpe.ct.asInstanceOf[ClassType[T]]).asInstanceOf[ListType[List[T]]]
    def `@list`(): ListType[List[Any]]                           = ListType()
    def listType(): ListType[List[Any]]                          = ListType()
    def `@listset`[V](ct: ClassType[V]): ListSetType[ListSet[V]] = listsetType(ct)
    def listsetType[V](ct: ClassType[V]): ListSetType[ListSet[V]] =
      ListSetType(ct.asInstanceOf[ClassType[V]])
    def `@listset`[T](implicit tpe: ClassTypeable[T]): ListSetType[ListSet[T]] = listsetType(tpe)
    def listsetType[T](implicit tpe: ClassTypeable[T]): ListSetType[ListSet[T]] =
      ListSetType(tpe.ct.asInstanceOf[ClassType[T]]).asInstanceOf[ListSetType[ListSet[T]]]
    def `@listset`()                                 = ListSetType()
    def listsetType()                                = ListSetType()
    def `@set`[V](ct: ClassType[V]): SetType[Set[V]] = setType(ct)
    def setType[V](ct: ClassType[V]): SetType[Set[V]] =
      SetType(ct.asInstanceOf[ClassType[V]])
    def `@set`[T](implicit tpe: ClassTypeable[T]): SetType[Set[T]] = setType(tpe)
    def setType[T](implicit tpe: ClassTypeable[T]): SetType[Set[T]] =
      SetType(tpe.ct.asInstanceOf[ClassType[T]]).asInstanceOf[SetType[Set[T]]]
    def `@set`(): SetType[Set[Any]]                                            = SetType()
    def setType(): SetType[Set[Any]]                                           = SetType()
    def `@map`[K, V](kct: ClassType[K], vct: ClassType[V]): MapType[Map[K, V]] = mapType(kct, vct)
    def mapType[K, V](kct: ClassType[K], vct: ClassType[V]): MapType[Map[K, V]] =
      MapType(kct.asInstanceOf[ClassType[K]], vct.asInstanceOf[ClassType[V]])
    def `@map`[K, V](implicit ktpe: ClassTypeable[K], vtpe: ClassTypeable[V]): MapType[Map[K, V]] = mapType(ktpe, vtpe)
    def mapType[K, V](implicit ktpe: ClassTypeable[K], vtpe: ClassTypeable[V]): MapType[Map[K, V]] =
      MapType(ktpe.ct.asInstanceOf[ClassType[K]], vtpe.ct.asInstanceOf[ClassType[V]]) //.asInstanceOf[MapType[K, V]]
    def `@map`(): MapType[Map[Any, Any]]                           = MapType()
    def mapType(): MapType[Map[Any, Any]]                          = MapType()
    def `@tuple`[T](ct: ClassType[_]*): TupleType[T]               = TupleType[T](ct.toList.map(Some(_)))
    def `@tuple`[T](cts: List[Option[ClassType[_]]]): TupleType[T] = TupleType[T](cts)
    def tupleType[T](ct: ClassType[_]*): TupleType[T]              = TupleType[T](ct.toList.map(Some(_)))
//    def tupleType[A, AT[+Z] <: ClassType[Z], ATOut <: ClassType[_], B, BT[+Z] <: ClassType[Z], BTOut <: ClassType[_]](
//                                                                                                                        act: AT[A],
//                                                                                                                        bct: BT[B]) = TupleType(List(act), List(bct))
//    def tupleType[A, B](implicit cta: ClassTypeable[A], ctb: ClassTypeable[B]) =
//      TupleType(List(cta.ct), List(ctb.ct)).asInstanceOf[TupleType[A, B]]
//    def tupleType() = TupleType(List(), List())

    val default = new DataType[Any] {
      type Out = Any
      val iri: String = ""
    }
  }

  implicit class WithDatatype[CT[+Z] <: DataType[Z], T](_ct: CT[T]) {
    lazy val extendedBy: ExtendedByClasses[CT[T]] = new ExtendedByClasses[CT[T]] {
      def ct: CT[T] = _ct

      def apply(): List[CT[T]] = ct.extendedByClassesList.asInstanceOf[List[CT[T]]]

      def all(exclude: Set[CT[T]] = Set()): Set[CT[T]] = {
        val _extends = apply().toSet -- exclude
        _extends ++ (_extends - ct).flatMap(_.extendedBy.all(_extends ++ exclude))
      }

      def contains(iri: String): Boolean = {
        val _extends = apply().toSet
        _extends.exists(_.iris.contains(iri)) || (_extends - ct)
          .filterNot(_.`extends`(ct))
          .exists(_.extendedBy.contains(iri))
      }

      def +(child: => CT[T]): ExtendedByClasses[CT[T]] = ct.synchronized {
        ct.extendedByClassesList =
          if (!ct.extendedByClassesList.contains(child))
            (ct.extendedByClassesList :+ child).distinct
          else {
            ct.extendedByClassesList
          }
        this
      }

      def -(parent: => CT[T]): ExtendedByClasses[CT[T]] = ct.synchronized {
        ct.extendedClassesList = ct.extendedClassesList.filterNot(_ == parent)
        parent match {
          case dt: DataType[_] => dt.asInstanceOf[DataType[Any]].extendedBy.-(ct.asInstanceOf[DataType[Any]])
          case _         =>
        }
        this
      }
    }
  }
}

/** @tparam T
  */
trait DataType[+T] extends ClassType[T] { self =>
//  type CT = DataType[_]

  def iris: Set[String]           = Set() + iri
  def _properties: List[Property] = List()

  override def toString: String = s"datatype:$iri"
}
