package lspace.codec.json.geojson

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import lspace.codec.json.JsonEncoder
import lspace.types.geo._

object Encoder {
  //  type Aux[Json0] = Decoder { type Json = Json0 }
  def apply[Json](encoder: JsonEncoder[Json]): Encoder[Json] = new Encoder(encoder)
}

class Encoder[Json](val encoder: JsonEncoder[Json]) extends lspace.codec.Encoder {
  import encoder._

  def encodeValue(value: Any): Json = value match {
    case value: Int           => value.asJson
    case value: Double        => value.asJson
    case value: Long          => value.asJson
    case value: Instant       => value.asJson
    case value: LocalDateTime => value.asJson
    case value: LocalDate     => value.asJson
    case value: LocalTime     => value.asJson
    case value: Boolean       => value.asJson
    case value: String        => value.asJson
    case value: Map[_, _]     => value.map { case (key, value) => key.toString -> encodeValue(value) }.asJson
    case value: List[_]       => value.map(encodeValue).asJson
    case _                    => throw new Exception("invalid")
  }

  def encodeFeature(feature: Feature[Geometry]): Json = {
    Map("type" -> "Feature".asJson, "geometry" -> encodeGeometryObject(feature.geometry)) ++
      (if (feature.properties.nonEmpty) Map("properties" -> feature.properties.mapValues(encodeValue).toMap.asJson)
       else Map[String, Json]())
  }.toMap.asJson

  def encodeFeatureCollection(featureCollection: FeatureCollection[Geometry]): Json =
    Map("type" -> "FeatureCollection".asJson, "features" -> featureCollection.features.map(encodeFeature).asJson).asJson

  def encodeGeometry(geometry: Geometry): Json = geometry match {
    case point: Point =>
      encodePoint(point)
    case multiPoint: MultiPoint =>
      encodeMultiPoint(multiPoint)
    case line: Line =>
      encodeLine(line)
    case multiLine: MultiLine =>
      encodeMultiLine(multiLine)
    case polygon: Polygon =>
      encodePolygon(polygon)
    case multiPolygon: MultiPolygon =>
      encodeMultiPolygon(multiPolygon)
    case multiGeometry: MultiGeometry =>
      encodeMultiGeometry(multiGeometry)
    case _ => throw new Exception("invalid")
  }
  def encodeGeometryObject(geometry: Geometry): Json = geometry match {
    case point: Point =>
      encodePointObject(point)
    case multiPoint: MultiPoint =>
      encodeMultiPointObject(multiPoint)
    case line: Line =>
      encodeLineObject(line)
    case multiLine: MultiLine =>
      encodeMultiLineObject(multiLine)
    case polygon: Polygon =>
      encodePolygonObject(polygon)
    case multiPolygon: MultiPolygon =>
      encodeMultiPolygonObject(multiPolygon)
    case multiGeometry: MultiGeometry =>
      encodeMultiGeometryObject(multiGeometry)
    case _ => throw new Exception("invalid")
  }

  def encodePointObject(point: Point): Json =
    Map("type" -> "Point".asJson, "coordinates" -> encodePoint(point)).asJson
  def encodePoint(point: Point): Json =
    List(point.x, point.y).asJson
  def encodeMultiPointObject(multiPoint: MultiPoint): Json =
    Map("type" -> "MultiPoint".asJson, "coordinates" -> encodeMultiPoint(multiPoint)).asJson
  def encodeMultiPoint(multiPoint: MultiPoint): Json =
    multiPoint.vector.toList.map(encodePoint).asJson
  def encodeLineObject(line: Line): Json =
    Map("type" -> "LineString".asJson, "coordinates" -> encodeLine(line)).asJson
  def encodeLine(line: Line): Json =
    line.vector.toList.map(encodePoint).asJson
  def encodeMultiLineObject(multiLine: MultiLine): Json =
    Map(
      "type"        -> "MultiLineString".asJson,
      "coordinates" -> encodeMultiLine(multiLine)
    ).asJson
  def encodeMultiLine(multiLine: MultiLine): Json =
    multiLine.vector.toList
      .map(encodeLine)
      .asJson
  def encodePolygonObject(polygon: Polygon): Json =
    Map("type" -> "Polygon".asJson, "coordinates" -> encodePolygon(polygon)).asJson
  def encodePolygon(polygon: Polygon): Json =
    polygon.vector.toList
      .map(vector => vector.toList.map(encodePoint).asJson)
      .asJson
  def encodeMultiPolygonObject(multiPolygon: MultiPolygon): Json =
    Map(
      "type"        -> "MultiPolygon".asJson,
      "coordinates" -> encodeMultiPolygon(multiPolygon)
    ).asJson
  def encodeMultiPolygon(multiPolygon: MultiPolygon): Json =
    multiPolygon.vector.toList
      .map(encodePolygon)
      .asJson
  def encodeMultiGeometryObject(multiGeometry: MultiGeometry): Json =
    Map("type" -> "GeometryCollection".asJson, "geometries" -> encodeMultiGeometry(multiGeometry)).asJson
  def encodeMultiGeometry(multiGeometry: MultiGeometry): Json =
    multiGeometry.vector.toList.map(encodeGeometryObject).asJson
}
