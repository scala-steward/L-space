package lspace.codec.json.geojson

import lspace.codec.json.JsonDecoder
import lspace.types.geo._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

abstract class DecoderSpec[Json](val decoder: JsonDecoder[Json]) extends AsyncWordSpec with Matchers {

  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  def geojsondecoder: GeoJsonDecoder[Json]

  "An GeoJsonDecoder" should {
    "decoder a point" in {
      geojsondecoder
        .decode("""{
                  |  "type": "Point",
                  |  "coordinates": [102.0, 0.5]
                  |}""".stripMargin)
        .map(_.features.head.geometry match { case geo: Point => succeed })
        .runToFuture
    }
    "decoder a multipoint" in {
      geojsondecoder
        .decode("""{
                  |  "type": "MultiPoint",
                  |  "coordinates": [
                  |    [10, 40], [40, 30], [20, 20], [30, 10]
                  |  ]
                  |}""".stripMargin)
        .map(_.features.head.geometry match { case geo: MultiPoint => succeed })
        .runToFuture
    }
    "decoder a line" in {
      geojsondecoder
        .decode("""{
                  |  "type": "LineString",
                  |  "coordinates": [
                  |    [102.0, 0.0], [103.0, 1.0], [104.0, 0.0], [105.0, 1.0]
                  |  ]
                  |}""".stripMargin)
        .map(_.features.head.geometry match { case geo: Line => succeed })
        .runToFuture
    }
    "decoder a multiline" in {
      geojsondecoder
        .decode("""{
                  |  "type": "MultiLineString",
                  |  "coordinates": [
                  |   [[10, 10], [20, 20], [10, 40]],
                  |   [[40, 40], [30, 30], [40, 20], [30, 10]]
                  |  ]
                  |}""".stripMargin)
        .map(_.features.head.geometry match { case geo: MultiLine => succeed })
        .runToFuture
    }
    "decoder a polygon" in {
      geojsondecoder
        .decode("""{
                  |  "type": "Polygon",
                  |  "coordinates": [[
                  |    [100.0, 0.0], [101.0, 0.0], [101.0, 1.0],
                  |    [100.0, 1.0], [100.0, 0.0]
                  |  ]]
                  |}""".stripMargin)
        .map(_.features.head.geometry match { case geo: Polygon => succeed })
        .runToFuture
    }
    "decoder a multipolygon" in {
      geojsondecoder
        .decode("""{
                  |  "type": "MultiPolygon",
                  |  "coordinates":
                  |  [[[[30, 20], [45, 40], [10, 40], [30, 20]]],
                  |  [[[15, 5], [40, 10], [10, 20], [5, 10], [15, 5]]]]
                  |}""".stripMargin)
        .map(_.features.head.geometry match { case geo: MultiPolygon => succeed })
        .runToFuture
    }
    "decoder a multigeometry" in {
      geojsondecoder
        .decode("""{
                  |  "type": "GeometryCollection",
                  |  "geometries": [
                  |    {
                  |      "type": "Point",
                  |      "coordinates": [40, 10]
                  |    },
                  |    {
                  |      "type": "LineString",
                  |      "coordinates": [
                  |         [10, 10], [20, 20], [10, 40]
                  |      ]
                  |    },
                  |    {
                  |       "type": "Polygon",
                  |       "coordinates": [
                  |         [[40, 40], [20, 45], [45, 30], [40, 40]]
                  |       ]
                  |    }
                  |  ]
                  |}""".stripMargin)
        .map(_.features.head.geometry match { case geo: MultiGeometry => succeed })
        .runToFuture
    }
    "decoder a feature" in {
      geojsondecoder
        .decode("""{
                  |  "type": "Feature",
                  |  "geometry": {
                  |    "type": "Point",
                  |    "coordinates": [102.0, 0.5]
                  |  },
                  |  "properties": {
                  |    "prop0": "value0"
                  |  }
                  |}""".stripMargin)
        .map(_.features.head.properties match {
          case props: Map[String, Any] if props.get("prop0").contains("value0") => succeed
          case props                                                            => fail()
        })
        .runToFuture
    }
    "decoder a featurecollection" in {
      geojsondecoder
        .decode("""{
                  |  "type": "FeatureCollection",
                  |  "features": [
                  |    {
                  |      "type": "Feature",
                  |      "geometry": {
                  |        "type": "Point",
                  |        "coordinates": [102.0, 0.5]
                  |      },
                  |      "properties": {
                  |        "prop0": "value0"
                  |      }
                  |    },
                  |    {
                  |      "type": "Feature",
                  |      "geometry": {
                  |        "type": "LineString",
                  |        "coordinates": [
                  |          [102.0, 0.0], [103.0, 1.0], [104.0, 0.0], [105.0, 1.0]
                  |        ]
                  |      },
                  |      "properties": {
                  |        "prop0": "value0",
                  |        "prop1": 0.0
                  |      }
                  |    },
                  |    {
                  |      "type": "Feature",
                  |      "geometry": {
                  |        "type": "Polygon",
                  |        "coordinates": [
                  |          [
                  |            [100.0, 0.0], [101.0, 0.0], [101.0, 1.0],
                  |            [100.0, 1.0], [100.0, 0.0]
                  |          ]
                  |        ]
                  |      },
                  |      "properties": {
                  |        "prop0": "value0",
                  |        "prop1": { "this": "that" }
                  |      }
                  |    }
                  |  ]
                  |}""".stripMargin)
        .map(_.features match {
          case features: List[Feature[_]] if features.size == 3 => succeed
          case _                                                => fail()
        })
        .runToFuture
    }
  }
}
