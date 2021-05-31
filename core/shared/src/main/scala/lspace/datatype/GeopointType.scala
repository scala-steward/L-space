package lspace.datatype

import lspace.NS
import lspace.structure._
import lspace.structure.util.ClassTypeable
import lspace.types.geo._

object GeopointType extends DataTypeDef[GeopointType[Point]] {

  lazy val datatype: GeopointType[Point] = new GeopointType[Point] {
    val iri: String                = NS.types.`@geopoint`
    override val iris: Set[String] = Set(NS.types.`@geopoint`)
    labelMap ++= Map("en" -> NS.types.`@geopoint`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(GeometricType.datatype)
  }

  object keys extends GeometricType.Properties
  override lazy val properties: List[Property] = GeometricType.properties
  trait Properties extends GeometricType.Properties

  implicit val defaultClsType: ClassTypeable.Aux[GeopointType[Point], Point, GeopointType[Point]] =
    new ClassTypeable[GeopointType[Point]] {
      type C  = Point
      type CT = GeopointType[Point]
      def ct: CT = datatype
    }
}
trait GeopointType[+T] extends GeometricType[T]

object GeoMultipointType extends DataTypeDef[GeoMultipointType[MultiPoint]] {

  lazy val datatype: GeoMultipointType[MultiPoint] = new GeoMultipointType[MultiPoint] {
    val iri: String = NS.types.`@geomultipoint`
    labelMap ++= Map("en" -> NS.types.`@geomultipoint`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(GeometricType.datatype)
  }

  object keys extends GeometricType.Properties
  override lazy val properties: List[Property] = GeometricType.properties
  trait Properties extends GeometricType.Properties

  implicit val defaultClsType
    : ClassTypeable.Aux[GeoMultipointType[MultiPoint], MultiPoint, GeoMultipointType[MultiPoint]] =
    new ClassTypeable[GeoMultipointType[MultiPoint]] {
      type C  = MultiPoint
      type CT = GeoMultipointType[MultiPoint]
      def ct: CT = datatype
    }
}
trait GeoMultipointType[+T] extends GeometricType[T]

object GeoLineType extends DataTypeDef[GeoLineType[Line]] {

  lazy val datatype: GeoLineType[Line] = new GeoLineType[Line] {
    val iri: String = NS.types.`@geoline`
    labelMap ++= Map("en" -> NS.types.`@geoline`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(GeometricType.datatype)
  }

  object keys extends GeometricType.Properties
  override lazy val properties: List[Property] = GeometricType.properties
  trait Properties extends GeometricType.Properties

  implicit val defaultClsType: ClassTypeable.Aux[GeoLineType[Line], Line, GeoLineType[Line]] =
    new ClassTypeable[GeoLineType[Line]] {
      type C  = Line
      type CT = GeoLineType[Line]
      def ct: CT = datatype
    }
}
trait GeoLineType[+T] extends GeometricType[T]

object GeoMultiLineType extends DataTypeDef[GeoMultiLineType[MultiLine]] {

  lazy val datatype: GeoMultiLineType[MultiLine] = new GeoMultiLineType[MultiLine] {
    val iri: String = NS.types.`@geomultiline`
    labelMap ++= Map("en" -> NS.types.`@geomultiline`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(GeometricType.datatype)
  }

  object keys extends GeometricType.Properties
  override lazy val properties: List[Property] = GeometricType.properties
  trait Properties extends GeometricType.Properties

  implicit val defaultClsType: ClassTypeable.Aux[GeoMultiLineType[MultiLine], MultiLine, GeoMultiLineType[MultiLine]] =
    new ClassTypeable[GeoMultiLineType[MultiLine]] {
      type C  = MultiLine
      type CT = GeoMultiLineType[MultiLine]
      def ct: CT = datatype
    }
}
trait GeoMultiLineType[+T] extends GeometricType[T]

object GeoPolygonType extends DataTypeDef[GeoPolygonType[Polygon]] {

  lazy val datatype: GeoPolygonType[Polygon] = new GeoPolygonType[Polygon] {
    val iri: String = NS.types.`@geopolygon`
    labelMap ++= Map("en" -> NS.types.`@geopolygon`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(GeometricType.datatype)
  }

  object keys extends GeometricType.Properties
  override lazy val properties: List[Property] = GeometricType.properties
  trait Properties extends GeometricType.Properties

  implicit val defaultClsType: ClassTypeable.Aux[GeoPolygonType[Polygon], Polygon, GeoPolygonType[Polygon]] =
    new ClassTypeable[GeoPolygonType[Polygon]] {
      type C  = Polygon
      type CT = GeoPolygonType[Polygon]
      def ct: CT = datatype
    }
}
trait GeoPolygonType[+T] extends GeometricType[T]

object GeoMultiPolygonType extends DataTypeDef[GeoMultiPolygonType[MultiPolygon]] {

  lazy val datatype: GeoMultiPolygonType[MultiPolygon] = new GeoMultiPolygonType[MultiPolygon] {
    val iri: String = NS.types.`@geomultipolygon`
    labelMap ++= Map("en" -> NS.types.`@geomultipolygon`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(GeometricType.datatype)
  }

  object keys extends GeometricType.Properties
  override lazy val properties: List[Property] = GeometricType.properties
  trait Properties extends GeometricType.Properties

  implicit val defaultClsType
    : ClassTypeable.Aux[GeoMultiPolygonType[MultiPolygon], MultiPolygon, GeoMultiPolygonType[MultiPolygon]] =
    new ClassTypeable[GeoMultiPolygonType[MultiPolygon]] {
      type C  = MultiPolygon
      type CT = GeoMultiPolygonType[MultiPolygon]
      def ct: CT = datatype
    }
}
trait GeoMultiPolygonType[+T] extends GeometricType[T]

object GeoMultiGeometryType extends DataTypeDef[GeoMultiGeometryType[MultiGeometry]] {

  lazy val datatype: GeoMultiGeometryType[MultiGeometry] = new GeoMultiGeometryType[MultiGeometry] {
    val iri: String = NS.types.`@geomultigeometry`
    labelMap ++= Map("en" -> NS.types.`@geomultigeometry`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(GeometricType.datatype)
  }

  object keys extends GeometricType.Properties
  override lazy val properties: List[Property] = GeometricType.properties
  trait Properties extends GeometricType.Properties

  implicit val defaultClsType
    : ClassTypeable.Aux[GeoMultiGeometryType[MultiGeometry], MultiGeometry, GeoMultiGeometryType[MultiGeometry]] =
    new ClassTypeable[GeoMultiGeometryType[MultiGeometry]] {
      type C  = MultiGeometry
      type CT = GeoMultiGeometryType[MultiGeometry]
      def ct: CT = datatype
    }
}
trait GeoMultiGeometryType[+T] extends GeometricType[T]
