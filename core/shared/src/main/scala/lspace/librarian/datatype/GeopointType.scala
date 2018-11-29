package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.structure._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.types.vector._

object GeopointType {
  val default: GeopointType[Point] = new GeopointType[Point] { type Out = Point }

  implicit val defaultClsType: ClassTypeable.Aux[GeopointType[Point], Point, GeopointType[Point]] =
    new ClassTypeable[GeopointType[Point]] {
      type C  = Point
      type CT = GeopointType[Point]
      def ct: CT = default
    }
}
trait GeopointType[+T] extends GeometricType[T] {
  val iri: String = NS.types.`@geopoint`

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(GeometricType)
}

object GeoMultipointType extends GeoMultipointType[MultiPoint] {
  type Out = MultiPoint
  implicit def default = GeoMultipointType

  implicit val defaultClsType
    : ClassTypeable.Aux[GeoMultipointType[MultiPoint], MultiPoint, GeoMultipointType[MultiPoint]] =
    new ClassTypeable[GeoMultipointType[MultiPoint]] {
      type C  = MultiPoint
      type CT = GeoMultipointType[MultiPoint]
      def ct: CT = default
    }
}
trait GeoMultipointType[+T] extends GeometricType[T] {
  val iri: String = NS.types.`@geomultipoint`

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(GeometricType)
}

object GeoLineType extends GeoLineType[Line] {
  type Out = Line
  implicit def default = GeoLineType

  implicit val defaultClsType: ClassTypeable.Aux[GeoLineType[Line], Line, GeoLineType[Line]] =
    new ClassTypeable[GeoLineType[Line]] {
      type C  = Line
      type CT = GeoLineType[Line]
      def ct: CT = default
    }
}
trait GeoLineType[+T] extends GeometricType[T] {
  val iri: String = NS.types.`@geoline`

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(GeometricType)
}

object GeoMultiLineType extends GeoMultiLineType[MultiLine] {
  type Out = MultiLine
  implicit def default = GeoMultiLineType

  implicit val defaultClsType: ClassTypeable.Aux[GeoMultiLineType[MultiLine], MultiLine, GeoMultiLineType[MultiLine]] =
    new ClassTypeable[GeoMultiLineType[MultiLine]] {
      type C  = MultiLine
      type CT = GeoMultiLineType[MultiLine]
      def ct: CT = default
    }
}
trait GeoMultiLineType[+T] extends GeometricType[T] {
  val iri: String = NS.types.`@geomultiline`

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(GeometricType)
}

object GeoPolygonType extends GeoPolygonType[Polygon] {
  type Out = Polygon
  implicit def default = GeoPolygonType

  implicit val defaultClsType: ClassTypeable.Aux[GeoPolygonType[Polygon], Polygon, GeoPolygonType[Polygon]] =
    new ClassTypeable[GeoPolygonType[Polygon]] {
      type C  = Polygon
      type CT = GeoPolygonType[Polygon]
      def ct: CT = default
    }
}
trait GeoPolygonType[+T] extends GeometricType[T] {
  val iri: String = NS.types.`@geopolygon`

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(GeometricType)
}

object GeoMultiPolygonType extends GeoMultiPolygonType[MultiPolygon] {
  type Out = MultiPolygon
  implicit def default = GeoMultiPolygonType

  implicit val defaultClsType
    : ClassTypeable.Aux[GeoMultiPolygonType[MultiPolygon], MultiPolygon, GeoMultiPolygonType[MultiPolygon]] =
    new ClassTypeable[GeoMultiPolygonType[MultiPolygon]] {
      type C  = MultiPolygon
      type CT = GeoMultiPolygonType[MultiPolygon]
      def ct: CT = default
    }
}
trait GeoMultiPolygonType[+T] extends GeometricType[T] {
  val iri: String = NS.types.`@geomultipolygon`

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(GeometricType)
}

object GeoMultiGeometryType extends GeoMultiGeometryType[MultiGeometry] {
  type Out = MultiGeometry
  implicit def default = GeoMultiGeometryType

  implicit val defaultClsType
    : ClassTypeable.Aux[GeoMultiGeometryType[MultiGeometry], MultiGeometry, GeoMultiGeometryType[MultiGeometry]] =
    new ClassTypeable[GeoMultiGeometryType[MultiGeometry]] {
      type C  = MultiGeometry
      type CT = GeoMultiGeometryType[MultiGeometry]
      def ct: CT = default
    }
}
trait GeoMultiGeometryType[+T] extends GeometricType[T] {
  val iri: String = NS.types.`@geomultigeometry`

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(GeometricType)
}
