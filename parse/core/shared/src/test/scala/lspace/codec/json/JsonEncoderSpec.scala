package lspace.codec.json

import lspace.types.geo.Point
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait JsonEncoderSpec extends AnyWordSpec with Matchers {
  type Json
  def encoder: lspace.codec.json.JsonEncoder[Json]

  def toMinString(json: Json): String
  implicit class WithJson(json: Json) {
    def minify: String = toMinString(json)
  }

  "An encoder" should {
    "encode literals" in {
      lazy val enc = encoder
      import enc._
      5.asJson.minify shouldBe """5"""
      5.6.asJson.minify shouldBe """5.6"""
      5L.asJson.minify shouldBe """5"""
      true.asJson.minify shouldBe """true"""
      "5.5".asJson.minify shouldBe """"5.5""""
      "abc".asJson.minify shouldBe """"abc""""
      Point(1.1, 2.2).asJson.minify shouldBe """{"type":"Point","coordinates":[1.1,2.2]}"""
      Map("name" -> "Alice".asJson).asJson.minify shouldBe """{"name":"Alice"}"""
      List(4.4.asJson, "Alice".asJson).asJson.minify shouldBe """[4.4,"Alice"]"""
    }
    "encode typed literals" in {
//      encoder.fromAny(5L, Some(LongType.datatype)).json.minify shouldBe """5"""
//      encoder.fromAny(5L).json.minify shouldBe """{"@value":5,"@type":"@long"}"""
    }
    "encode a traversal" ignore {
//      val traversal = MemGraphDefault.g.N.has(SampleGraph.properties.name) //, Some(Traversal.ontology))
//      val jip = encoder.fromNode(traversal.toNode)
//      encoder.fromNode(traversal.toNode).withContext ?? types.`@context` shouldBe true
//      decoder
//        .stringToNode(encoder(traversal.toNode))
//        .runToFuture
//        .flatMap { node =>
//          val parsedTraversal = Traversal.toTraversal(node)(MemGraphDefault)
//          val jip2 = encoder.fromNode(parsedTraversal.toNode)
//          decoder.stringToLabeledNode(encoder(parsedTraversal.toNode), Traversal.ontology).runToFuture.map { node2 =>
//            val parsedTraversal2 = Traversal.toTraversal(node2)(MemGraphDefault)
//            val jip3 = encoder.fromNode(parsedTraversal2.toNode)
//            jip.json shouldBe jip2.json
//            jip.json shouldBe jip2.json
//            jip.json shouldBe jip3.json
//            jip.activeContext shouldBe jip2.activeContext
//            jip2.activeContext shouldBe jip3.activeContext
//          }
//        }
    }

    "encode an ontology" ignore {
//      val name = Property("thing/name", range = DataType.default.`@string` :: Nil)
//      val typedName = name + DataType.default.`@string`
//      val surname =
//        Property("thing/surname", range = DataType.default.`@string` :: Nil, containers = List(types.`@set`))
//      val typedSurname = surname + DataType.default.`@string`
//      val testOntology: Ontology =
//        Ontology("thing", properties = name :: surname :: Nil, extendedClasses = new Ontology("basething") {} :: Nil)
//
//      val jip = encoder.fromOntology(testOntology)
//      val json = jip.json
//      json ?? types.`@id` shouldBe true
//      json(types.`@id`).exists(_.string.contains("thing")) shouldBe true
//
//      json ?? types.`@properties` shouldBe true
//      json(types.`@properties`).get.isArray shouldBe true
//      json(types.`@properties`).get.array.get.exists(_.string.contains("thing/name")) shouldBe true
//      json(types.`@properties`).get.array.get.exists(_.string.contains("thing/surname")) shouldBe true
    }
  }
}
