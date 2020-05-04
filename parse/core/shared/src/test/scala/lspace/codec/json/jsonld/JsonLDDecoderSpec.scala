package lspace.codec.json.jsonld

import lspace.Label.D._
import lspace._
import lspace.codec.{ActiveContext, ActiveProperty, NamedActiveContext}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
//import scribe.Level
//import scribe.format.Formatter

import scala.collection.immutable.ListMap
import scala.concurrent.duration._

abstract class JsonLDDecoderSpec[Json](val decoder: JsonLDDecoder[Json]) extends AsyncWordSpec with Matchers {

  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

//  scribe.Logger.root
//    .clearHandlers()
//    .clearModifiers()
//    .withHandler(minimumLevel = Some(Level.Trace), formatter = Formatter.enhanced)
//    .replace()

  val graph: Graph = decoder.graph

  "The Decoder" should {
    "decode any ontology".which {
      "is served by schema.org" in {
        decoder
          .toOntology("http://schema.org/Person")(ActiveContext())
          .map { ontology =>
            ontology.iri shouldBe "http://schema.org/Person"
            ontology
              .properties("http://schema.org/additionalName")
              .isDefined shouldBe true
            ontology
              .properties("http://schema.org/additionalName")
              .exists(_.range(`@string`.iri).isDefined) shouldBe true
            ontology.properties("http://schema.org/colleagues").isDefined shouldBe false //superseded
          }
          .timeout(60.seconds)
          .runToFuture
      }
    }
    "decode any property".which {
      "is served by schema.org" in {
        decoder
          .toProperty("http://schema.org/artEdition")(ActiveContext())
          .map { property =>
            property.iri shouldBe "http://schema.org/artEdition"
            property.range().contains(`@string`) shouldBe true
            property.range().contains(`@int`) shouldBe true
            property.range().contains(`@long`) shouldBe false
          }
          .timeout(60.seconds)
          .runToFuture
      }
    }
    "decode a vocabulary".ignore {
      decoder
        .fetchVocabularyGraph("https://schema.org/version/3.5/all-layers.jsonld")(ActiveContext())
        .map { u =>
          val codeSampleType = Property.properties.get("http://schema.org/codeSampleType")
          codeSampleType.isDefined shouldBe true
          val CssSelectorType = Ontology.ontologies.get("http://schema.org/CssSelectorType")
          CssSelectorType.isDefined shouldBe true
          succeed
        }
        .runToFuture
    }
    "decode a traversal" in {
      try {
        val traversal = lspace.g.N.hasLabel(Ontology("mylabel")).count()
        decoder
          .stringToLabeledNode(
            """{"@context":{"0":"https://ns.l-space.eu/librarian/"},"@type":"0:Traversal","0:Traversal/steps":{"@value":[{"@type":"0:step/N"},{"@type":"0:step/HasLabel","0:step/HasLabel/Label":{"@id":"mylabel", "@type":"@class"}},{"@type":"0:step/Count"}],"@type":"@vector(https://ns.l-space.eu/librarian/Step)"}}""",
            Traversal.ontology
          )(ActiveContext())
          .flatMap { node =>
            Traversal.toTraversal(node)
          }
          .map(_ shouldBe traversal)
          .runToFuture
      } catch {
        case e: Throwable =>
          e.printStackTrace()
          fail()
      }
    }
    "decode another traversal" in {
      try {
        val traversal = lspace.g.N
          .hasLabel(Ontology("https://ns.hoorn.nl/Project"))
          .range(0, 10)
          .project(_.iri)
          .by(_.out(Property("http://schema.org/geo").as(`@geo`)))
          .by(_.out(Property("http://schema.org/description").as(`@string`)))
        decoder
          .stringToLabeledNode(
            """{"@context":{"0":"https://ns.l-space.eu/librarian/"},"@type":"0:Traversal","0:Traversal/steps":{"@value":[{"@type":"0:step/N"},{"@type":"0:step/HasLabel","0:step/HasLabel/Label":{"@id":"https://ns.hoorn.nl/Project"}},{"@type":"0:step/Range","0:step/Range/low":{"@value":0,"@type":"@int"},"0:step/Range/high":{"@value":10,"@type":"@int"}},{"@type":"0:step/Project","0:step/Project/by":{"@value":[{"0:Traversal/steps":{"@value":[{"@type":"0:step/Out","0:MoveStep/label":{"@id":"http://schema.org/description"}},{"@type":"0:step/HasLabel","0:step/HasLabel/Label":{"@id":"@string"}}],"@type":"@vector(https://ns.l-space.eu/librarian/Step)"}},{"0:Traversal/steps":{"@value":[{"@type":"0:step/Out","0:MoveStep/label":{"@id":"http://schema.org/geo"}},{"@type":"0:step/HasLabel","0:step/HasLabel/Label":{"@id":"@geo"}}],"@type":"@vector(https://ns.l-space.eu/librarian/Step)"}},{"0:Traversal/steps":{"@value":[{"@type":"0:step/Out","0:MoveStep/label":{"@id":"@id"}},{"@type":"0:step/HasLabel","0:step/HasLabel/Label":{"@id":"@string"}}],"@type":"@vector(https://ns.l-space.eu/librarian/Step)"}}],"@type":"@list(https://ns.l-space.eu/librarian/Traversal)"}}],"@type":"@vector(https://ns.l-space.eu/librarian/Step)"}}""",
            Traversal.ontology
          )(ActiveContext())
          .flatMap { node =>
            Traversal.toTraversal(node)
          }
          .map(_ shouldBe traversal)
          .runToFuture
      } catch {
        case e: Throwable =>
          e.printStackTrace()
          fail()
      }
    }
    "decode any node".which {
      "uses a context if provided" in {
        val person = Ontology.ontologies.getOrCreate("https://example.org/Person")
        val defaultContext = ActiveContext(
          `@prefix` = ListMap("naam" -> "name"),
          definitions = Map(
            "name"    -> ActiveProperty(`@type` = `@string` :: Nil, property = Property("name"))(),
            "nameFor" -> ActiveProperty(`@type` = person :: Nil, `@reverse` = true, property = Property("name"))()
          )
        )
        (for {
          node <- decoder.stringToNode(
            """{"@context":{"naam":{"@id":"name","@type":"@string"},"1":"https://example.org/"},"@id":"DecoderSpec-sample/person/567","@ids":"DecoderSpec-sample/person/567","@type":"1:Person","rate":{"@value":4,"@type":"@int"},"1:birthDate":{"@value":"2002-06-13","@type":"@date"},"1:birthPlace":{"@id":"DecoderSpec-sample/place/123"},"balance":{"@value":300,"@type":"@int"},"1:knows":[{"@id":"DecoderSpec-sample/person/56789"},{"@id":"DecoderSpec-sample/person/34567"}],"naam":"Stan"}""")(
            defaultContext)
          _ = node.out(Property("name")).head shouldBe "Stan"
        } yield succeed).runToFuture
      }
    }
    "decode a Context".which {
      "exists of just a remote context" in {
        val defaultContext =
          ActiveContext(remotes = List(NamedActiveContext("https://remote.example.org", ActiveContext())))
        (for {
          json          <- decoder.parse(""""https://remote.example.org"""")
          activeContext <- decoder.contextProcessing.apply(ActiveContext(), json)
        } yield activeContext shouldBe defaultContext).runToFuture
      }
      "exists of a remote context and a local context" in {
        val defaultContext =
          ActiveContext(`@prefix` = ListMap("name" -> "https://example.com/name"),
                        remotes = List(NamedActiveContext("https://remote.example.org", ActiveContext())))

        (for {
          json          <- decoder.parse("""["https://remote.example.org",{"name":"https://example.com/name"}]""")
          activeContext <- decoder.contextProcessing.apply(ActiveContext(), json)
        } yield activeContext shouldBe defaultContext).runToFuture
      }
      "exists of two remote contexts and a local context" in {
        val defaultContext =
          ActiveContext(
            `@prefix` = ListMap("name" -> "https://example.com/name"),
            remotes = List(NamedActiveContext("https://remote.example.org", ActiveContext()),
                           NamedActiveContext("https://remote2.example.org", ActiveContext()))
          )

        (for {
          json <- decoder.parse(
            """["https://remote.example.org", "https://remote2.example.org",{"name":"https://example.com/name"}]""")
          activeContext <- decoder.contextProcessing.apply(ActiveContext(), json)
        } yield activeContext shouldBe defaultContext).runToFuture
      }
    }
  }
}
