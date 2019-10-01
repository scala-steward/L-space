package lspace.codec.json.geojson

import lspace.codec.json.{JsonDecoder, JsonEncoder}
import lspace.types.geo._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.Future

abstract class EncoderSpec[Json](val encoder: JsonEncoder[Json]) extends AsyncWordSpec with Matchers {

  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  import encoder._
  def decoder: JsonDecoder[Json]
  def parseRawToJson: String => Json
  implicit class WithString(string: String) {
    def parse: Json = parseRawToJson(string)
  }
  def geojsonencoder: GeoJsonEncoder[Json]

  "An GeoJsonEncoder" should {
    "encode a point" in Future {
      geojsonencoder
        .encodePointObject(Point(102.0, 0.5)) shouldBe """{"type":"Point","coordinates":[102,0.5]}""".parse
    }
    "encode a multipoint" in Future {
      geojsonencoder
        .encodeMultiPointObject(MultiPoint(Point(10, 40), Point(40, 30), Point(20, 20), Point(30, 10))) shouldBe """{"type":"MultiPoint","coordinates":[[10,40],[40,30],[20,20],[30,10]]}""".parse
    }
    "encode a line" in Future {
      geojsonencoder
        .encodeLineObject(Line(Point(102, 0), Point(103, 1), Point(104, 0), Point(105, 1))) shouldBe """{"type":"LineString","coordinates":[[102,0],[103,1],[104,0],[105,1]]}""".parse
    }
    "encode a multiline" in Future {
      geojsonencoder
        .encodeMultiLineObject(
          MultiLine(
            Line(Point(10, 10), Point(20, 20), Point(10, 40)),
            Line(Point(40, 40), Point(30, 30), Point(40, 20), Point(30, 10)))) shouldBe """{"type":"MultiLineString","coordinates":[[[10,10],[20,20],[10,40]],[[40,40],[30,30],[40,20],[30,10]]]}""".parse
    }
    "encode a polygon" in Future {
      geojsonencoder
        .encodePolygonObject(Polygon(Point(100, 0), Point(101, 0), Point(101, 1), Point(100, 1), Point(100, 0))) shouldBe """{"type":"Polygon","coordinates":[[[100,0],[101,0],[101,1],[100,1],[100,0]]]}""".parse
    }
    "encode a multipolygon" in Future {
      geojsonencoder
        .encodeMultiPolygonObject(MultiPolygon(Polygon(Point(30, 20), Point(45, 40), Point(10, 40), Point(30, 20)),
                                               Polygon(
                                                 Point(15, 5),
                                                 Point(40, 10),
                                                 Point(10, 20),
                                                 Point(5, 10),
                                                 Point(15, 5)))) shouldBe """{"type":"MultiPolygon","coordinates":[[[[30,20],[45,40],[10,40],[30,20]]],[[[15,5],[40,10],[10,20],[5,10],[15,5]]]]}""".parse
    }
    "encode a multigeometry" in Future {
      geojsonencoder
        .encodeMultiGeometryObject(
          MultiGeometry(
            Point(40, 10),
            Line(Point(10, 10), Point(20, 20), Point(10, 40)),
            Polygon(Point(40, 40), Point(20, 45), Point(45, 30), Point(40, 40)))) shouldBe """{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[40,10]},{"type":"LineString","coordinates":[[10,10],[20,20],[10,40]]},{"type":"Polygon","coordinates":[[[40,40],[20,45],[45,30],[40,40]]]}]}""".parse
    }
    "encode a feature" in {
      geojsonencoder
        .encodeFeature(Feature(Point(102, 0.5), Map("prop0" -> "value0"))) shouldBe """{"type":"Feature","geometry":{"type":"Point","coordinates":[102,0.5]},"properties":{"prop0":"value0"}}""".parse
    }
    "encode a featurecollection" in {
      geojsonencoder
        .encodeFeatureCollection(
          FeatureCollection(
            List(
              Feature(Point(102, 0.5), Map("prop0" -> "value0")),
              Feature(Line(Point(102, 0), Point(103, 1), Point(104, 0), Point(105, 1)),
                      Map("prop0" -> "value0", "prop1" -> 0.0)),
              Feature(Polygon(Point(100, 0), Point(101, 0), Point(101, 1), Point(100, 1), Point(100, 0)),
                      Map("prop0" -> "value0", "prop1" -> Map("this" -> "that")))
            ))
        ) shouldBe
        """{"type":"FeatureCollection","features":[{"type":"Feature","geometry":{"type":"Point","coordinates":[102,0.5]},"properties":{"prop0":"value0"}},{"type":"Feature","geometry":{"type":"LineString","coordinates":[[102,0],[103,1],[104,0],[105,1]]},"properties":{"prop0":"value0","prop1":0}},{"type":"Feature","geometry":{"type":"Polygon","coordinates":[[[100,0],[101,0],[101,1],[100,1],[100,0]]]},"properties":{"prop0":"value0","prop1":{"this":"that"}}}]}""".parse
    }
  }
}
