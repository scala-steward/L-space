package lspace.librarian.datatype

import java.util.concurrent.ConcurrentHashMap

import lspace.NS
import lspace.NS.types
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.OntologyDef
import lspace.librarian.structure._
import monix.eval.{Coeval, Task}

object DataType
    extends OntologyDef(NS.types.`@datatype`,
                        Set(NS.types.schemaDataType),
                        NS.types.`@datatype`,
                        `@extends` = () => Ontology.ontology :: Nil) {

  object keys

  lazy val datatype: DataType[Any] = new DataType[Any] {
    val iri: String                         = NS.types.`@datatype`
    override val iris: Set[String]          = Set(NS.types.schemaDataType)
    override val label: Map[String, String] = Map("en" -> NS.types.`@datatype`)
  }

  def urlType[T]: IriType[T] = new IriType[T] {
    val iri: String = NS.types.`@datatype`
  }

  def apply(node: Node): DataType[_] = node match {
    case literalType: LiteralType[_] => literalType
    case node =>
      node.iri match {
        case types.`@id` | types.schemaURL            => IriType.datatype
        case types.`@nodeURL`                         => NodeURLType.datatype
        case types.`@edgeURL`                         => EdgeURLType.datatype
        case types.`@valueURL`                        => ValueURLType.datatype
        case types.`@string` | types.schemaText       => TextType.datatype
        case types.`@number` | types.schemaNumber     => IntType.datatype
        case types.`@int` | types.schemaInteger       => IntType.datatype
        case types.`@double` | types.schemaFloat      => DoubleType.datatype
        case types.`@long`                            => LongType.datatype
        case types.`@date` | types.schemaDate         => LocalDateType.datatype
        case types.`@datetime` | types.schemaDateTime => DateTimeType.datatype
        case types.`@time` | types.schemaTime         => LocalTimeType.datatype
        case types.`@temporal`                        => CalendarType.datatype
        case types.`@duration`                        => DurationType.datatype
        case types.`@quantity`                        => QuantityType.datatype
//        case types.`@epoch`                           => EpochType
        case types.`@boolean` | types.schemaBoolean => BoolType.datatype
        case types.`@geo`                           => GeometricType.datatype
        case types.`@color`                         => TextType.datatype
        case _ =>
          throw new Exception(s"ontology/property ${node.iri} not known in graph ${node.graph.iri}")
      }
  }

  implicit def clsDatatype[T]: ClassTypeable.Aux[DataType[T], T, DataType[T]] = new ClassTypeable[DataType[T]] {
    type C  = T
    type CT = DataType[T]
    def ct: CT = new DataType[T] { val iri: String = "" }
  }

  private lazy val _this = this
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
    val `@duration`: DurationType      = DurationType.datatype
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

    def vectorType[V, VT[+Z] <: ClassType[Z], VTOut <: ClassType[_]](ct: VT[V]) =
      VectorType(List(ct.asInstanceOf[ClassType[V]]))
    def vectorType[T](implicit tpe: ClassTypeable[T]) =
      VectorType(List(tpe.ct.asInstanceOf[ClassType[T]])).asInstanceOf[VectorType[T]]
    def vectorType() = VectorType(List[ClassType[Any]]())
    def listType[V, VT[+Z] <: ClassType[Z], VTOut <: ClassType[_]](ct: VT[V]) =
      ListType(List(ct.asInstanceOf[ClassType[V]]))
    def listType[T](implicit tpe: ClassTypeable[T]) =
      ListType(List(tpe.ct.asInstanceOf[ClassType[T]])).asInstanceOf[ListType[T]]
    def listType() = ListType(List[ClassType[Any]]())
    def listsetType[V, VT[+Z] <: ClassType[Z], VTOut <: ClassType[_]](ct: VT[V]) =
      ListSetType(List(ct.asInstanceOf[ClassType[V]]))
    def listsetType[T](implicit tpe: ClassTypeable[T]) =
      ListSetType(List(tpe.ct.asInstanceOf[ClassType[T]])).asInstanceOf[ListSetType[T]]
    def listsetType() = ListSetType(List[ClassType[Any]]())
    def setType[V, VT[+Z] <: ClassType[Z], VTOut <: ClassType[_]](ct: VT[V]) =
      SetType(List(ct.asInstanceOf[ClassType[V]]))
    def setType[T](implicit tpe: ClassTypeable[T]) =
      SetType(List(tpe.ct.asInstanceOf[ClassType[T]])).asInstanceOf[SetType[T]]
    def setType() = SetType(List[ClassType[Any]]())
    def mapType[K, KT[+Z] <: ClassType[Z], KTOut <: ClassType[_], V, VT[+Z] <: ClassType[Z], VTOut <: ClassType[_]](
        kct: KT[K],
        vct: VT[V]) = MapType(List(kct.asInstanceOf[ClassType[K]]), List(vct.asInstanceOf[ClassType[V]]))
    def mapType[K, V](implicit ktpe: ClassTypeable[K], vtpe: ClassTypeable[V]): MapType[K, V] =
      MapType(List(ktpe.ct.asInstanceOf[ClassType[K]]), List(vtpe.ct.asInstanceOf[ClassType[V]])) //.asInstanceOf[MapType[K, V]]
    def mapType() = MapType(List[ClassType[Any]](), List[ClassType[Any]]())
    def tuple2Type[A, AT[+Z] <: ClassType[Z], ATOut <: ClassType[_], B, BT[+Z] <: ClassType[Z], BTOut <: ClassType[_]](
        act: AT[A],
        bct: BT[B]) = Tuple2Type(List(act), List(bct))
    def tuple2Type[A, B](implicit cta: ClassTypeable[A], ctb: ClassTypeable[B]) =
      Tuple2Type(List(cta.ct), List(ctb.ct)).asInstanceOf[Tuple2Type[A, B]]
    def tuple2Type() = Tuple2Type(List(), List())
    def tuple3Type[A,
                   AT[+Z] <: ClassType[Z],
                   ATOut <: ClassType[_],
                   B,
                   BT[+Z] <: ClassType[Z],
                   BTOut <: ClassType[_],
                   C,
                   CT[+Z] <: ClassType[Z],
                   CTOut <: ClassType[_]](act: AT[A], bct: BT[B], cct: CT[C]) =
      Tuple3Type(List(act), List(bct), List(cct))
    def tuple3Type[A, B, C](implicit cta: ClassTypeable[A], ctb: ClassTypeable[B], ctc: ClassTypeable[C]) =
      Tuple3Type(List(cta.ct), List(ctb.ct), List(ctc.ct)).asInstanceOf[Tuple3Type[A, B, C]]
    def tuple3Type() = Tuple3Type(List(), List(), List())
    def tuple4Type[A,
                   AT[+Z] <: ClassType[Z],
                   ATOut <: ClassType[_],
                   B,
                   BT[+Z] <: ClassType[Z],
                   BTOut <: ClassType[_],
                   C,
                   CT[+Z] <: ClassType[Z],
                   CTOut <: ClassType[_],
                   D,
                   DT[+Z] <: ClassType[Z],
                   DTOut <: ClassType[_]](act: AT[A], bct: BT[B], cct: CT[C], dct: DT[D]) =
      Tuple4Type(List(act), List(bct), List(cct), List(dct))
    def tuple4Type[A, B, C, D](a: A, b: B, c: C, d: D)(implicit cta: ClassTypeable[A],
                                                       ctb: ClassTypeable[B],
                                                       ctc: ClassTypeable[C],
                                                       ctd: ClassTypeable[D]) =
      Tuple4Type(List(cta.ct), List(ctb.ct), List(ctc.ct), List(ctd.ct))
    def tuple4Type[A, B, C, D](implicit cta: ClassTypeable[A],
                               ctb: ClassTypeable[B],
                               ctc: ClassTypeable[C],
                               ctd: ClassTypeable[D]) =
      Tuple4Type(List(cta.ct), List(ctb.ct), List(ctc.ct), List(ctd.ct))
        .asInstanceOf[Tuple4Type[A, B, C, D]]
    def tuple4Type() = Tuple4Type(List(), List(), List(), List())

    val default = new DataType[Any] {
      type Out = Any
      val iri: String = ""
    }
  }

  /**
    * imcomplete...
    */
  object allDataTypes {
    import default._
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
      `@duration`,
      `@boolean`,
      `@geo`,
      `@geopoint`,
      `@graph`,
      `@url`,
      `@nodeURL`,
      `@edgeURL`,
      `@valueURL`
//      `@class`,
//      `@property`,
//      `@datatype`
    )
    if (datatypes.size > 99) throw new Exception("extend default-datatype-id range!")
    val byId    = (0l to datatypes.size - 1 toList).zip(datatypes).toMap
    val byIri   = byId.toList.flatMap { case (id, dt) => dt.iri :: dt.iris.toList map (_ -> dt) }.toMap
    val idByIri = byId.toList.flatMap { case (id, dt) => dt.iri :: dt.iris.toList map (_ -> id) }.toMap
  }

  import scala.collection.JavaConverters._
  import scala.collection.concurrent
  private val constructing: concurrent.Map[String, Task[DataType[_]]] =
    new ConcurrentHashMap[String, Task[DataType[_]]]().asScala
  def getOrConstructing(iri: String)(constructTask: Task[DataType[_]]): Task[DataType[_]] =
    constructing.getOrElseUpdate(iri, constructTask.memoize)

  private val constructed: concurrent.Map[String, Coeval[DataType[_]]] =
    new ConcurrentHashMap[String, Coeval[DataType[_]]]().asScala
  def getOrConstructed(iri: String)(constructTask: Coeval[DataType[_]]): Coeval[DataType[_]] =
    constructed.getOrElseUpdate(iri, constructTask.memoize)
  def getConstructed(iri: String): Option[Coeval[DataType[_]]] =
    constructed.get(iri)
}

/**
  *
  * @tparam T
  */
trait DataType[+T] extends ClassType[T] {
//  type CT = DataType[_]

  val iris: Set[String]                              = Set()
  val label: Map[String, String]                     = Map()
  val comment: Map[String, String]                   = Map()
  val _extendedClasses: () => List[_ <: DataType[_]] = () => List()
  val _properties: () => List[Property]              = () => List()
  val base: Option[String]                           = None

  override lazy val extendedClasses: List[_ <: DataType[_]] = _extendedClasses()

  override def toString: String = s"datatype:$iri"
}
