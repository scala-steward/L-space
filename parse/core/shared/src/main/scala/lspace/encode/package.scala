package lspace

import lspace.types.vector.{Geometry, Point, Polygon}

package object encode {
  implicit def GeometryCodecJson[Json](geometry: Geometry)(implicit encoder: lspace.codec.Encoder[Json]): Json = {
    import encoder._
    geometry match {
      case point: Point =>
        Map("type" -> ("Point": Json), "coordinates" -> (List(point.x: Json, point.y: Json): Json)): Json
      case polygon: Polygon =>
        Map("type"        -> ("Polygon": Json),
            "coordinates" -> (polygon.vector.map(p => List(p.x: Json, p.y: Json).toList: Json).toList: Json)): Json
    }
  }
}
