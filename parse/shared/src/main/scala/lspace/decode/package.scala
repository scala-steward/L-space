package lspace

import argonaut.Json
import lspace.types.vector.{Geometry, Point, Polygon}

package object decode {
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
