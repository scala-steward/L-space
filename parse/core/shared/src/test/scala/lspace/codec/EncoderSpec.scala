package lspace.codec

import lspace.NS.types
import lspace.librarian.datatype.{DataType, LongType}
import lspace.librarian.process.traversal.Traversal
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure.{Ontology, Property}
import lspace.librarian.util.SampleGraph
import lspace.types.vector.Point
import org.scalatest.{Matchers, WordSpec}

trait EncoderSpec[Json] extends WordSpec with Matchers {
  def encoder: lspace.codec.Encoder[Json]
  implicit lazy val ac = encoder.getNewActiveContext

  "An encoder" should {
    "encode literals" in {
      encoder.intToJson(5).toString shouldBe """5"""
      encoder.doubleToJson(5.6).toString shouldBe """5.6"""
      encoder.longToJson(5L).toString shouldBe """5"""
      encoder.boolToJson(true).toString shouldBe """true"""
      encoder.textToJson("5.5").toString shouldBe """"5.5""""
      encoder.textToJson("abc").toString shouldBe """"abc""""
      encoder.geoToJson(Point(1.1, 2.2)).toString shouldBe """{"type":"Point","coordinates":[1.1,2.2]}"""
      encoder.mapToJson(Map("name" -> encoder.textToJson("Alice"))).toString shouldBe """{"name":"Alice"}"""
      encoder
        .listToJson(List(encoder.doubleToJson(4.4), encoder.textToJson("Alice")))
        .toString shouldBe """[4.4,"Alice"]"""
    }
    "encode typed literals" in {
      encoder.fromAny(5L, Some(LongType.datatype)).json.toString shouldBe """5"""
      encoder.fromAny(5L).json.toString shouldBe """{"@value":5,"@type":"@long"}"""
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
