package lspace

import lspace.types.geo.{Geometry, Point, Polygon}

package object encode {
  implicit def GeometryCodecJson[Json](geometry: Geometry)(
      implicit encoder: lspace.codec.json.JsonEncoder[Json]): Json = {
    import encoder._
    geometry match {
      case point: Point =>
        Map("type" -> ("Point".asJson), "coordinates" -> (List(point.x.asJson, point.y.asJson).asJson)).asJson
      case polygon: Polygon =>
        Map("type" -> ("Polygon".asJson),
            "coordinates" -> (polygon.vector
              .map(_.map(p => List(p.x.asJson, p.y.asJson).toList.asJson).toList.asJson)
              .toList
              .asJson)).asJson
    }
  }
}
