package lspace.codec.json.geojson

import lspace.codec.exception.FromJsonException
import lspace.codec.json
import lspace.codec.json.JsonDecoder
import lspace.types.geo._
import monix.eval.{Coeval, Task}

object Decoder {
//  type Aux[Json0] = Decoder { type Json = Json0 }
  def apply[Json](decoder: JsonDecoder[Json]): Decoder[Json] = new Decoder(decoder)
}
class Decoder[Json](val decoder: JsonDecoder[Json]) extends lspace.codec.Decoder {
  import decoder._

  def autodiscoverValue(json: Json): Coeval[Any] = {
    json.int
      .orElse(json.double)
      .orElse(json.long)
      .orElse(json.datetime)
      .orElse(json.localdatetime)
      .orElse(json.date)
      .orElse(json.time)
      .orElse(json.boolean)
      .orElse(json.string)
      .map(Coeval.now)
      .orElse(json.list.map(_.map(autodiscoverValue)).map(Coeval.traverse(_)(f => f)))
      .orElse(json.obj
        .map(_.map { case (key, value) => autodiscoverValue(value).map(key -> _) })
        .map(Coeval.traverse(_)(f => f).map(_.toMap)))
//      .orElse(decodeGeometryOption(json))
      .getOrElse(Coeval.raiseError(new Exception("autodiscover not possible")))
  }

  def decodeFeature(map: Map[String, Json]): Task[Feature[Geometry]] = {
    for {
      geometry <- map
        .get("geometry")
        .map(Task.now(_).map(jsonToMap(_).map(decodeGeometry).getOrElse(throw new Exception("geometry not an object"))))
        .getOrElse(Task.raiseError(new Exception("no geometry")))
      properties <- map
        .get("properties")
        .map(
          decoder
            .jsonToMap(_)
            .map(Coeval.now)
            .getOrElse(Coeval.raiseError(new Exception("invalid properties"))))
        .getOrElse(Coeval.now(Map[String, Json]()))
        .flatMap(properties =>
          Coeval
            .traverse(properties.toList)({
              case (key, value) => autodiscoverValue(value).map(key -> _)
            })
            .map(_.toMap))
        .to[Task]
    } yield Feature(geometry, properties)
  }
  def decodeFeatureOption(map: Map[String, Json]): Option[Task[Feature[Geometry]]] = {
    if (map.get("type").exists(decoder.jsonToString(_).contains("Feature"))) Some(decodeFeature(map)) else None
  }
  def decode(json: String): Task[FeatureCollection[Geometry]] = {
    decoder.parse(json).flatMap(decode(_))
  }
  def decode(json: Json): Task[FeatureCollection[Geometry]] = {
    decoder
      .jsonToMap(json)
      .map { map =>
        map.get("type").flatMap(decoder.jsonToString) match {
          case Some(iri) =>
            iri match {
              case "FeatureCollection" =>
                map
                  .get("features")
                  .map(
                    decoder
                      .jsonToList(_)
                      .map(_.map(decoder
                        .jsonToMap(_)
                        .map(decodeFeatureOption(_).getOrElse(Task.raiseError(new Exception("invalid feature"))))
                        .getOrElse(Task.raiseError(new Exception("feature must be an object")))))
                      .map(Task.gather(_))
                      .getOrElse(Task.raiseError(new Exception("features must be an array"))))
                  .getOrElse(Task.raiseError(new Exception("FeatureCollection without 'features' is weird")))
                  .map(FeatureCollection(_))
              case "Feature" =>
                decodeFeature(map).map(List(_)).map(FeatureCollection(_))
              case "GeometryCollection" =>
                Task { FeatureCollection(List(Feature(decodeGeometryCollection(map)))) }
              case _ =>
                Task { FeatureCollection(List(Feature(decodeGeometryCoordinates(iri, decodeCoordinates(map))))) }
            }
          case None => Task.raiseError(new Exception("invalid geojson"))
        }
      }
      .getOrElse(Task.raiseError(new Exception("invalid geojson")))
  }

  def decodeGeometryOption(json: Json): Option[Geometry] =
    decoder
      .jsonToMap(json)
      .map(
        decodeGeometryOption(_).getOrElse(
          throw new Exception("geometry without type")
        ))
  def decodeGeometry(map: Map[String, Json]): Geometry =
    decodeGeometryOption(map).getOrElse(throw new Exception("geometry without type"))
  def decodeGeometryOption(map: Map[String, Json]): Option[Geometry] = {
    map.get("type").flatMap(decoder.jsonToString).map {
      case "GeometryCollection" => decodeGeometryCollection(map)
      case iri                  => decodeGeometryCoordinates(iri, decodeCoordinates(map))
    }
  }

  def decodeGeometryCoordinates(iri: String, coordinates: List[Json]): Geometry = {
    iri match {
      case "Point"           => decodePoint(coordinates)
      case "MultiPoint"      => decodeMultiPoint(coordinates)
      case "LineString"      => decodeLine(coordinates)
      case "MultiLineString" => decodeMultiLine(coordinates)
      case "Polygon"         => decodePolygon(coordinates)
      case "MultiPolygon"    => decodeMultiPolygon(coordinates)
      case _                 => throw FromJsonException(s"not a valid geojson Geometry $iri")
    }
  }

  def decodeCoordinates(map: Map[String, Json]): List[Json] = {
    map
      .get("coordinates")
      .map(decoder.jsonToList(_).getOrElse(throw new Exception("coordinates is expected to be an array")))
      .getOrElse(throw new Exception("geometry without coordinates is invalid"))
  }

  def jsonToPoint(json: Json): Option[Point] = {
    decoder
      .jsonToMap(json)
      .flatMap(map =>
        map.get("type").flatMap(decoder.jsonToString).map {
          case "Point" => decodePoint(decodeCoordinates(map))
      })
  }
  def decodePoint(coordinates: List[Json]): Point = {
    coordinates match {
      case List(lat, lng) =>
        (decoder.jsonToDouble(lat) -> decoder.jsonToDouble(lng)) match {
          case (Some(lat), Some(lng)) => Point(lat, lng)
          case _                      => throw FromJsonException(s"not a valid geojson Point ${coordinates}")
        }
      case _ => throw FromJsonException(s"not a valid geojson Point ${coordinates}")
    }
  }

  def jsonToMultiPoint(json: Json): Option[MultiPoint] = {
    decoder
      .jsonToMap(json)
      .flatMap(map =>
        map.get("type").flatMap(decoder.jsonToString).map {
          case "MultiPoint" => decodeMultiPoint(decodeCoordinates(map))
      })
  }
  def decodeMultiPoint(coordinates: List[Json]): MultiPoint =
    MultiPoint(coordinates.map(decoder.jsonToList).map {
      case Some(coordinates) => decodePoint(coordinates)
      case _                 => throw FromJsonException(s"not a valid geojson MultiPoint ${coordinates}")
    }: _*)

  def jsonToLine(json: Json): Option[Line] = {
    decoder
      .jsonToMap(json)
      .flatMap(map =>
        map.get("type").flatMap(decoder.jsonToString).map {
          case "LineString" => decodeLine(decodeCoordinates(map))
      })
  }
  def decodeLine(coordinates: List[Json]): Line = {
    Line(coordinates.map(decoder.jsonToList).map {
      case Some(coordinates) => decodePoint(coordinates)
      case _                 => throw FromJsonException(s"not a valid geojson Line ${coordinates}")
    }: _*)
  }

  def jsonToMultiLine(json: Json): Option[MultiLine] = {
    decoder
      .jsonToMap(json)
      .flatMap(map =>
        map.get("type").flatMap(decoder.jsonToString).map {
          case "MultiLineString" => decodeMultiLine(decodeCoordinates(map))
      })
  }
  def decodeMultiLine(coordinates: List[Json]): MultiLine =
    MultiLine(coordinates.map(decoder.jsonToList).map {
      case Some(coordinates) => decodeLine(coordinates)
      case _                 => throw FromJsonException(s"not a valid geojson MultiLine ${coordinates}")
    }: _*)

  def jsonToPolygon(json: Json): Option[Polygon] = {
    decoder
      .jsonToMap(json)
      .flatMap(map =>
        map.get("type").flatMap(decoder.jsonToString).map {
          case "Polygon" => decodePolygon(decodeCoordinates(map))
      })
  }
  //TODO: array of polygons (donuts)
  def decodePolygon(coordinates: List[Json]): Polygon = {
    Polygon(coordinates.map(decoder.jsonToList).flatMap {
      case Some(coordinates) =>
        coordinates.headOption.map(decoder.jsonToList).map {
          case Some(coordinates) => decodePoint(coordinates)
          case _                 => throw FromJsonException(s"not a valid geojson Polygon ${coordinates}")
        }
      case _ => throw FromJsonException(s"not a valid geojson Polygon ${coordinates}")
    }: _*)
  }

  def jsonToMultiPolygon(json: Json): Option[MultiPolygon] = {
    decoder
      .jsonToMap(json)
      .flatMap(map =>
        map.get("type").flatMap(decoder.jsonToString).map {
          case "MultiPolygon" => decodeMultiPolygon(decodeCoordinates(map))
      })
  }
  def decodeMultiPolygon(coordinates: List[Json]): MultiPolygon =
    MultiPolygon(coordinates.map(decoder.jsonToList).map {
      case Some(coordinates) => decodePolygon(coordinates)
      case _                 => throw FromJsonException("not a valid geojson MultiPolygon")
    }: _*)

  def jsonToMultiGeometry(json: Json): Option[MultiGeometry] = {
    decoder
      .jsonToMap(json)
      .flatMap(map =>
        map.get("type").flatMap(decoder.jsonToString).map {
          case "GeometryCollection" => decodeGeometryCollection(map)
      })
  }
  def decodeGeometryCollection(map: Map[String, Json]): MultiGeometry =
    map.get("geometries") match {
      case Some(json) =>
        MultiGeometry(
          decoder
            .jsonToList(json)
            .map(decodeMultiGeometry)
            .getOrElse(throw new Exception("invalid MultiGeometry, array expected for geometries")))
      case None => throw FromJsonException("not a valid geojson MultiGeometry, geometries missing")
    }

  def decodeMultiGeometry(geometries: List[Json]): MultiGeometry =
    MultiGeometry(
      geometries
        .map(decoder.jsonToMap(_).getOrElse(throw new Exception("invalid MultiGeometry Geometry")))
        .map(decodeGeometry)
        .toVector)
}
