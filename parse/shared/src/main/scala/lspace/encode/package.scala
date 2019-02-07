package lspace

import argonaut._
import Argonaut._
import lspace.types.vector.{Geometry, Point, Polygon}

package object encode {
  implicit def GeometryCodecJson =
    (geometry: Geometry) =>
      geometry match {
        case point: Point =>
          Json.obj("type" -> Json.jString("Point"), "coordinates" -> Json.jArray(List(point.x.asJson, point.y.asJson)))
        case polygon: Polygon =>
          Json.obj(
            "type"        -> Json.jString("Polygon"),
            "coordinates" -> Json.jArray(polygon.vector.map(p => Json.jArray(List(p.x.asJson, p.y.asJson))).toList))
    }
}
