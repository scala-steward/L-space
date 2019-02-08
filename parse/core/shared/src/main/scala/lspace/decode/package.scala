package lspace

import lspace.codec.exception.FromJsonException
import lspace.types.vector.{Geometry, Point, Polygon}

import scala.util.Try

package object decode {
  def fromGeoJson[Json](json: Json)(implicit decoder: lspace.codec.Decoder[Json]): Try[Geometry] =
    decoder.jsonToMap(json).map(fromGeoJson(_)).getOrElse(throw FromJsonException("not a valid geojson Geometry"))
  def fromGeoJson[Json](obj: Map[String, Json])(implicit decoder: lspace.codec.Decoder[Json]): Try[Geometry] = Try {
    obj
      .get("type")
      .flatMap(decoder.jsonToString)
      .map {
        case "Point" =>
          obj
            .get("coordinates")
            .map(decoder.jsonToList)
            .map {
              case Some(List(lat, lng)) =>
                (decoder.jsonToDouble(lat) -> decoder.jsonToDouble(lng)) match {
                  case (Some(lat), Some(lng)) => Point(lat, lng)
                  case _                      => throw FromJsonException("not a valid geojson Point lat/lng")
                }
              case _ => throw FromJsonException("not a valid geojson Point")
            }
            .getOrElse(throw FromJsonException("not a valid geojson Point"))
        //        case "MultiPoint" =>
        //        case "Line" =>
        //        case "MultiLine" =>
        case "Polygon" =>
          Polygon(
            obj
              .get("coordinates")
              .flatMap(decoder.jsonToList(_).map(_.map(decoder.jsonToList)))
              .map(_.map {
                case Some(List(lat, lng)) =>
                  (decoder.jsonToDouble(lat) -> decoder.jsonToDouble(lng)) match {
                    case (Some(lat), Some(lng)) => Point(lat, lng)
                    case _                      => throw FromJsonException("not a valid geojson Polygon Point lat/lng")
                  }
                case _ => throw FromJsonException("not a valid geojson Polygon")
              }.toVector)
              .getOrElse(throw FromJsonException("not a valid geojson Polygon")))
        //        case "MultiPolygon" =>
        //        case "MultiGeometry" =>
      }
      .getOrElse(throw FromJsonException("not a valid geojson Geometry"))
  }
}
