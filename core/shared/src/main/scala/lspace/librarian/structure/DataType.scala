package lspace.librarian.structure

import java.time.{Instant, LocalDate, LocalTime}

import lspace.NS.types
import lspace.NS
import lspace.librarian.datatype._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.types.vector.Geometry

import scala.collection.Traversable
import scala.collection.immutable.ListSet

object DataType {
  lazy val ontology =
    Ontology(NS.types.`@datatype`)(iris = Set(NS.types.schemaDataType),
                                   _extendedClasses = () => List(Ontology.ontology))

  def urlType[T]: IriType[T] = new IriType[T] {
    type Out = T
    val iri: String = NS.types.`@datatype`
  }

  def apply(node: Node): DataType[_] = node match {
    case literalType: LiteralType[_] => literalType
    case node =>
      node.iri match {
        case types.`@id` | types.schemaURL            => IriType
        case types.`@nodeURL`                         => NodeURLType.default
        case types.`@edgeURL`                         => EdgeURLType.default
        case types.`@valueURL`                        => ValueURLType.default
        case types.`@string` | types.schemaText       => TextType.textType
        case types.`@int` | types.schemaInteger       => IntType.intType
        case types.`@double` | types.schemaFloat      => DoubleType.doubleType
        case types.`@long`                            => LongType.longType
        case types.`@date` | types.schemaDate         => LocalDateType.default
        case types.`@datetime` | types.schemaDateTime => DateTimeType.datetimeType
        case types.`@time` | types.schemaTime         => LocalTimeType.default
        case types.`@duration`                        => DurationType
        case types.`@epoch`                           => EpochType
        case types.`@boolean` | types.schemaBoolean   => BoolType.boolType
        case types.`@geojson`                         => GeometricType
        case types.`@color`                           => TextType.textType
        case _ =>
          throw new Exception(s"ontology/property ${node.iri} not known in graph ${node.graph.iri}")
      }
  }

  implicit def clsDatatype[T]: ClassTypeable.Aux[DataType[T], T, DataType[T]] = new ClassTypeable[DataType[T]] {
    type C  = T
    type CT = DataType[T]
    def ct: CT = new DataType[T] { val iri: String = "" }
  }

  object default {
    val uRLType = IriType

    val nodeURLType     = NodeURLType.default
    val edgeURLType     = EdgeURLType.default
    val valueURLType    = ValueURLType.default
    val ontologyURLType = Ontology.urlType
    val propertyURLType = Property.urlType
    val dataTypeURLType = DataType.urlType[DataType[_]]

    val `@string`        = TextType.textType
    val `@number`        = NumericType.numType[AnyVal]
    val `@int`           = IntType.intType
    val `@double`        = DoubleType.doubleType
    val `@long`          = LongType.longType
    val `@date`          = LocalDateType.default
    val `@datetime`      = DateTimeType.datetimeType
    val `@localdatetime` = LocalDateTimeType.localdatetimeType
    val `@time`          = LocalTimeType.default
    val `@temporal`      = CalendarType
    val `@duration`      = DurationType
    val `@quantity`      = QuantityType
    //  val epochType: EpochType = EpochType
    val `@boolean`  = BoolType.boolType
    val `@geo`      = GeometricType
    val `@geopoint` = GeopointType.default
    val `@color`    = ColorType
    val `@graph`    = GraphType.default

    def vectorType[V, VT[+Z] <: ClassType[Z], VTOut <: ClassType[_]](ct: VT[V]) =
      new VectorType(List(ct.asInstanceOf[ClassType[V]]))
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
  lazy val allDataTypes = new {
    import default._
    val datatypes = List(
      uRLType,
      nodeURLType,
      edgeURLType,
      valueURLType,
      ontologyURLType,
      propertyURLType,
      dataTypeURLType,
      `@string`,
      `@int`,
      `@double`,
      `@long`,
      `@date`,
      `@datetime`,
      `@localdatetime`,
      `@time`,
      `@duration`,
      `@boolean`,
      `@geo`,
      `@geopoint`,
      `@graph`
    )
    val byId    = (0l to datatypes.size - 1 toList).zip(datatypes).toMap
    val byIri   = byId.toList.flatMap { case (id, dt) => dt.iri :: dt.iris.toList map (_ -> dt) }.toMap
    val idByIri = byId.toList.flatMap { case (id, dt) => dt.iri :: dt.iris.toList map (_ -> id) }.toMap
  }
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
object LiteralType extends LiteralType[Any] {
  val iri: String = NS.types.`@literal`
  type Out = Any

//  implicit val classTypeable: ClassTypeable.Aux[LiteralType[Any], LiteralType[Any]] =
//    new ClassTypeable[LiteralType[Any]] {
//      type CT = LiteralType[Any]
//      def ct: CT = LiteralType
//    }
  implicit def clsLiteral[T]: ClassTypeable.Aux[LiteralType[T], T, LiteralType[T]] = new ClassTypeable[LiteralType[T]] {
    type C  = T
    type CT = LiteralType[T]
    def ct: CT = new LiteralType[T] { val iri: String = NS.types.`@literal` }
  }
//  implicit def default[T, CT[+Z] <: LiteralType[Z]](implicit ev: CT[T] =:= LiteralType[Any]): LiteralType[T] =
//    LiteralType.asInstanceOf[LiteralType[T]]
//
//  implicit def dt[T, CT[Z] <: LiteralType[Z]](implicit ev: CT[T] <:< LiteralType[T]) = DataType.urlType[CT[T]]
}
trait LiteralType[+T] extends DataType[T]

object StructuredValue {
  lazy val ontology =
    Ontology(NS.types.`@structured`)(_extendedClasses = () => List(DataType.ontology))

  def structuredType[T]: StructuredValue[T] = new StructuredValue[T] {
    type Out = T
    type CT  = StructuredValue[T]
    val iri: String                                             = NS.types.`@structured`
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(DataType.default.default)
  }

  implicit def clsStructured[T]: ClassTypeable.Aux[StructuredValue[T], T, StructuredValue[T]] =
    new ClassTypeable[StructuredValue[T]] {
      type C  = T
      type CT = StructuredValue[T]
      def ct: CT = StructuredValue.structuredType[T]
    }
  implicit def default[T, CT[+Z] <: ClassType[Z]](implicit ev: CT[T] =:= StructuredValue[Any]): StructuredValue[T] =
    structuredType[T]
//  implicit def dt[T, CT[Z] <: StructuredValue[Z]](implicit ev: CT[T] <:< StructuredValue[T]) = DataType.urlType[CT[T]]
}
trait StructuredValue[+T] extends DataType[T]

object CollectionType {

  def default[T] = new CollectionType[Iterable[T]] {
    type Out = Iterable[T]
    val iri: String = NS.types.`@collection`
  }

  lazy val ontology =
    Ontology(NS.types.`@collection`)(_extendedClasses = () => List(StructuredValue.ontology))

  object keys {
    private val valueRangeNode = MemGraphDefault.ns.nodes.upsert("@valueRange")
    valueRangeNode.addLabel(Property.ontology)
    valueRangeNode --- Property.default.`@label` --> "@valueRange" --- Property.default.`@language` --> "en"
    valueRangeNode --- Property.default.`@container` --> types.`@list`
    valueRangeNode --- Property.default.`@range` --> types.`@class`
    val valueRange = Property(valueRangeNode)
  }

  def apply[V](valueRange: List[ClassType[V]]) = new CollectionType[Iterable[V]] {
    val iri: String = s"${NS.types.`@collection`}/${valueRange.map(_.iri).sorted.mkString("+")}"
    //    override lazy val extendedClasses: List[DataType[Iterable[V]]] = List(StructuredValue[V]())
  }

  implicit def clsCollection[T]: ClassTypeable.Aux[CollectionType[T], Iterable[Any], CollectionType[Iterable[Any]]] =
    new ClassTypeable[CollectionType[T]] {
      type C  = Iterable[Any]
      type CT = CollectionType[Iterable[Any]]
      def ct: CT = default[Any]
    }
}
trait CollectionType[+T /*<: Iterable[_]*/ ] extends StructuredValue[T] {
  override val _properties: () => List[Property] = () => List(CollectionType.keys.valueRange)
}

object NumericType {
  def numType[T]: NumericType[T] = new NumericType[T] {
    type Out = T
    val iri: String                = NS.types.`@number`
    override val iris: Set[String] = Set(NS.types.schemaNumber)
  }

  implicit def clsNumeric[T, CT[+Z] <: NumericType[Z]]: ClassTypeable.Aux[CT[T], T, NumericType[T]] =
    new ClassTypeable[CT[T]] {
      type C  = T
      type CT = NumericType[T]
      def ct: CT = NumericType.numType[T]
    }

//  implicit def dt[T, CT[Z] <: NumericType[Z]](implicit ev: CT[T] <:< NumericType[T]) = DataType.urlType[CT[T]]
}
trait NumericType[+T] extends LiteralType[T]
object CalendarType extends CalendarType[Any] {
  val iri: String = NS.types.`@temporal`
  type Out = Any

  implicit val clsCalendar: ClassTypeable.Aux[CalendarType[Any], Any, CalendarType[Any]] =
    new ClassTypeable[CalendarType[Any]] {
      type C  = Any
      type CT = CalendarType[Any]
      def ct: CT = CalendarType
    }

//  implicit def default[T, CT[+Z] <: ClassType[Z]](implicit ev: CT[T] =:= CalendarType[Any]): CalendarType[T] =
//    CalendarType.asInstanceOf[CalendarType[T]]
//  implicit def dt[T, CT[Z] <: CalendarType[Z]](implicit ev: CT[T] <:< CalendarType[T]) = DataType.urlType[CT[T]]
}
trait CalendarType[+T] extends LiteralType[T]

object QuantityType extends QuantityType[Any] {
  val iri: String = NS.types.`@quantity`
  type Out = Any

  implicit val clsQuantity: ClassTypeable.Aux[QuantityType[Any], Any, QuantityType[Any]] =
    new ClassTypeable[QuantityType[Any]] {
      type C  = Any
      type CT = QuantityType[Any]
      def ct: CT = QuantityType
    }
//  implicit def default[T, CT[+Z] <: ClassType[Z]](implicit ev: CT[T] =:= QuantityType[Any]): QuantityType[T] =
//    QuantityType.asInstanceOf[QuantityType[T]]
//  implicit def dt[T, CT[Z] <: QuantityType[Z]](implicit ev: CT[T] <:< QuantityType[T]) = DataType.urlType[CT[T]]
}
trait QuantityType[+T] extends StructuredValue[T]

object GeometricType extends GeometricType[Geometry] {
  val iri: String = NS.types.`@geo`
  type Out = Geometry

  implicit val clsGeometric: ClassTypeable.Aux[GeometricType[Geometry], Geometry, GeometricType[Geometry]] =
    new ClassTypeable[GeometricType[Geometry]] {
      type C  = Geometry
      type CT = GeometricType[Geometry]
      def ct: CT = GeometricType
    }
//  implicit def default[T, CT[+Z] <: ClassType[Z]](implicit ev: CT[T] =:= GeometricType[Any]): GeometricType[T] =
//    GeometricType.asInstanceOf[GeometricType[T]]
//  implicit def dt[T, CT[Z] <: GeometricType[Z]](implicit ev: CT[T] <:< GeometricType[T]) = DataType.urlType[CT[T]]
}
trait GeometricType[+T] extends StructuredValue[T]
object ColorType extends ColorType[Any] { //TODO RgbType, CMYK, PMS, NamedColor
  val iri: String = NS.types.`@color`
  type Out = Any

  implicit val clsColor: ClassTypeable.Aux[ColorType[Any], Any, ColorType[Any]] = new ClassTypeable[ColorType[Any]] {
    type C  = Any
    type CT = ColorType[Any]
    def ct: CT = ColorType
  }
//  implicit def default[T, CT[+Z] <: ClassType[Z]](implicit ev: CT[T] =:= ColorType[Any]): ColorType[T] =
//    ColorType.asInstanceOf[ColorType[T]]
//  implicit def dt[T, CT[Z] <: ColorType[Z]](implicit ev: CT[T] <:< ColorType[T]) = DataType.urlType[CT[T]]
}
trait ColorType[+T] extends StructuredValue[T]

object IriType extends IriType[IriResource] {
  val iri: String = NS.types.schemaURL
  type Out = IriResource

  def apply[T]: IriType[T] = new IriType[T] { val iri: String = "" }

//  implicit val classTypeable: ClassTypeable[IriType[Any]] = new ClassTypeable[IriType[Any]] {
//    type CT = IriType[Any]
//    def ct: CT = IriType
//  }
//  implicit def default[S, E](implicit ev: S =:= Any, ev2: E =:= Any): IriType[Edge[S, E]] =
//    EdgeURLType.edgeUrlType[Edge[S, E]]

  implicit def clsIri[T]: ClassTypeable.Aux[IriType[T], T, IriType[T]] = new ClassTypeable[IriType[T]] {
    type C  = T
    type CT = IriType[T]
    def ct: CT = DataType.urlType[T]
  }

}
trait IriType[+T] extends DataType[T]
