package lspace.codec.turtle

import lspace.structure.Graph
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class DecoderSpec extends AsyncWordSpec with Matchers {

  val graph: Graph                         = Graph("lspace.codec.turtle.DecoderSpec")
  val decoder: lspace.codec.turtle.Decoder = lspace.codec.turtle.Decoder(graph)
  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  val sample =
    """
      |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
      | @prefix dc: <http://purl.org/dc/elements/1.1/> .
      | @prefix ex: <http://example.org/stuff/1.0/> .
      |
      | <http://www.w3.org/TR/rdf-syntax-grammar>
      |   dc:title "RDF/XML Syntax Specification (Revised)" ;
      |   ex:editor [
      |     ex:fullname "Dave Beckett";
      |     ex:homePage <http://purl.org/net/dajobe/>
      |   ] .
    """.stripMargin

  "The Turtle Decoder" should {
    "parse .." in {
      decoder
        .parse(sample)
        .onErrorHandle { f =>
          f.printStackTrace(); throw f
        }
//        .flatMap { turtle =>
//          import decoder._
//          turtle.process
//        }
        .map { turtle =>
          turtle.context.`@prefix`.get("rdf") shouldBe Some("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
          turtle.context.`@prefix`.get("dc") shouldBe Some("http://purl.org/dc/elements/1.1/")
          turtle.context.`@prefix`.get("ex") shouldBe Some("http://example.org/stuff/1.0/")
          turtle.statements.exists(_.subject.iri == "http://www.w3.org/TR/rdf-syntax-grammar") shouldBe true
        }
        .runToFuture
    }
  }
}
