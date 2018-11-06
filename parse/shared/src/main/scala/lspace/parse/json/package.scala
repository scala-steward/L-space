package lspace.parse

import argonaut._
import Argonaut._
import lspace.types.vector.{Geometry, Point, Polygon}

package object json {
  implicit def GeometryCodecJson: EncodeJson[Geometry] =
    EncodeJson({ (geometry: Geometry) =>
      geometry match {
        case point: Point =>
          Json.obj("type" -> Json.jString("Point"), "coordinates" -> Json.jArray(List(point.x.asJson, point.y.asJson)))
        case polygon: Polygon =>
          Json.obj(
            "type"        -> Json.jString("Polygon"),
            "coordinates" -> Json.jArray(polygon.vector.map(p => Json.jArray(List(p.x.asJson, p.y.asJson))).toList))
      }
    })

  def fromGeoJson(json: Json): Option[Geometry] = {
    json.obj.flatMap { obj =>
      obj.toMap.get("type").flatMap(_.string).map {
        case "Point" =>
          obj.toMap.get("coordinates").get.array.get match {
            case List(lat, lng) => Point(lat.number.get.toDouble.get, lng.number.get.toDouble.get)
          }
        //        case "MultiPoint" =>
        //        case "Line" =>
        //        case "MultiLine" =>
        case "Polygon" =>
          Polygon(
            obj.toMap
              .get("coordinates")
              .get
              .array
              .get
              .flatMap(_.array)
              .map {
                case List(lat, lng) =>
                  Point(lat.number.get.toDouble.get, lng.number.get.toDouble.get)
              }
              .toVector)
        //        case "MultiPolygon" =>
        //        case "MultiGeometry" =>
      }
    }
  }
}
