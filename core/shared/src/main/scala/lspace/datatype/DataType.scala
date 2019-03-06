package lspace.datatype

import java.util.concurrent.ConcurrentHashMap

import lspace.NS
import lspace.NS.types
import lspace.structure.util.ClassTypeable
import lspace.structure.OntologyDef
import lspace.structure._
import monix.eval.{Coeval, Task}

import scala.collection.concurrent
import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration

object DataType
    extends OntologyDef(NS.types.`@datatype`,
                        Set(NS.types.`@datatype`, NS.types.schemaDataType),
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

  private def build(node: Node): Coeval[DataType[_]] = {
    if (node.hasLabel(ontology).nonEmpty) {
      Coeval
        .sequence(
          node
            .out(Property.default.`@extends`)
            .headOption
            .collect {
              case nodes: List[_] =>
                nodes.collect {
                  case node: Node if node.hasLabel(DataType.ontology).isDefined =>
                    datatypes
                      .get(node.iri)
                      .getOrElse {
                        datatypes.getOrBuild(node)
                      } //orElse???
                  case iri: String =>
                    datatypes
                      .get(iri)
                      .getOrElse(throw new Exception("@extends looks like an iri but cannot be wrapped by a property"))
                }
              case node: Node if node.hasLabel(DataType.ontology).isDefined =>
                List(datatypes.get(node.iri).getOrElse(datatypes.getOrBuild(node)))
            }
            .toList
            .flatten)
        .flatMap { extended =>
          extended.head match {
            case dt: CollectionType[_] =>
              dt match {
                case dt: ListType[_] =>
                  Coeval
                    .sequence(
                      node
                        .out(ListType.keys.valueRangeClassType)
                        .map(types => Coeval.sequence(types.map(ClassType.classtypes.getOrBuild))))
                    .map { types =>
                      ListType(types.flatten)
                    }
                case dt: ListSetType[_] =>
                  Coeval
                    .sequence(
                      node
                        .out(ListSetType.keys.valueRangeClassType)
                        .map(types => Coeval.sequence(types.map(ClassType.classtypes.getOrBuild))))
                    .map { types =>
                      ListSetType(types.flatten)
                    }
                case dt: SetType[_] =>
                  Coeval
                    .sequence(
                      node
                        .out(SetType.keys.valueRangeClassType)
                        .map(types => Coeval.sequence(types.map(ClassType.classtypes.getOrBuild))))
                    .map { types =>
                      SetType(types.flatten)
                    }
                case dt: VectorType[_] =>
                  Coeval
                    .sequence(
                      node
                        .out(VectorType.keys.valueRangeClassType)
                        .map(types => Coeval.sequence(types.map(ClassType.classtypes.getOrBuild))))
                    .map { types =>
                      VectorType(types.flatten)
                    }
                case dt: MapType[_, _] =>
                  for {
                    keyRange <- Coeval
                      .sequence(
                        node
                          .out(MapType.keys.keyRangeClassType)
                          .map(types => Coeval.sequence(types.map(ClassType.classtypes.getOrBuild))))
                    valueRange <- Coeval
                      .sequence(
                        node
                          .out(MapType.keys.valueRangeClassType)
                          .map(types => Coeval.sequence(types.map(ClassType.classtypes.getOrBuild))))
                  } yield {
                    MapType(keyRange.flatten, keyRange.flatten)
                  }
                case dt: TupleType[_] =>
                  node
                    .out(TupleType.keys._rangeClassType)
                    .map(list =>
                      Coeval.sequence(list.map(types => Coeval.sequence(types.map(ClassType.classtypes.getOrBuild)))))
                    .head
                    .map(TupleType(_))
              }
            case _ => Coeval.raiseError(new Exception(""))
          }
        }
    } else {
      //      new Exception(s"${node.iri} with id ${node.id} is not an ontology, labels: ${node.labels.map(_.iri)}")
      //        .printStackTrace()
      Coeval.raiseError(
        new Exception(s"${node.iri} with id ${node.id} ${node.outE(Property.default.`@id`).head.to.id} " +
          s"${node.graph.values.hasId(node.outE(Property.default.`@id`).head.to.id).isDefined} is not an ontology, labels: ${node.labels
            .map(_.iri)}"))
    }
  }

  object datatypes {

    /**
      * imcomplete...
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
        `@duration`,
        `@boolean`,
        `@geo`,
        `@geopoint`,
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
//        Tuple2Type.datatype,
//        Tuple3Type.datatype,
//        Tuple4Type.datatype
        //      `@class`,
        //      `@property`,
        //      `@datatype`
      )
      if (datatypes.size > 99) throw new Exception("extend default-datatype-id range!")
      val byId    = (0l to datatypes.size - 1 toList).zip(datatypes).toMap
      val byIri   = byId.toList.flatMap { case (id, dt) => dt.iri :: dt.iris.toList map (_ -> dt) }.toMap
      val idByIri = byId.toList.flatMap { case (id, dt) => dt.iri :: dt.iris.toList map (_ -> id) }.toMap
    }
    private[lspace] val byIri: concurrent.Map[String, DataType[_]] =
      new ConcurrentHashMap[String, DataType[_]]().asScala
    private[lspace] val building: concurrent.Map[String, Coeval[DataType[_]]] =
      new ConcurrentHashMap[String, Coeval[DataType[_]]]().asScala

    def all: List[DataType[_]] = byIri.values.toList.distinct
    def get(iri: String): Option[Coeval[DataType[_]]] =
      default.byIri
        .get(iri)
        .orElse(byIri.get(iri))
        .map(o => Coeval.now(o))
        .orElse(building.get(iri))
    def getOrBuild(node: Node): Coeval[DataType[_]] =
      default.byIri
        .get(node.iri)
        .map(Coeval.now(_))
        .getOrElse(building.getOrElseUpdate(node.iri, build(node).map { d =>
          byIri += d.iri -> d
          building.remove(node.iri)
          d.iris.foreach { iri =>
            datatypes.byIri += iri -> d
          }
          d
        }.memoizeOnSuccess))

    def cache(datatype: DataType[_]): Unit = {
      byIri += datatype.iri -> datatype
      datatype.iris.foreach { iri =>
        datatypes.byIri += iri -> datatype
      }
    }
    def cached(long: Long): Option[DataType[_]]  = default.byId.get(long)
    def cached(iri: String): Option[DataType[_]] = default.byIri.get(iri).orElse(byIri.get(iri))

    def remove(iri: String): Unit = byIri.remove(iri)
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
}

/**
  *
  * @tparam T
  */
trait DataType[+T] extends ClassType[T] {
//  type CT = DataType[_]

  val iris: Set[String]                           = Set()
  val label: Map[String, String]                  = Map()
  val comment: Map[String, String]                = Map()
  val _extendedClasses: () => List[DataType[Any]] = () => List()
  val _properties: () => List[Property]           = () => List()
  val base: Option[String]                        = None

  override lazy val extendedClasses: List[DataType[Any]] = _extendedClasses()

  override def toString: String = s"datatype:$iri"
}
