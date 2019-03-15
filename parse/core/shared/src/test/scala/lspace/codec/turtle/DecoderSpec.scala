package lspace.codec.turtle

import lspace.structure.Graph
import monix.eval.Task
import org.scalatest.{AsyncWordSpec, Matchers}

class DecoderSpec extends AsyncWordSpec with Matchers {

  val graph: Graph                         = Graph("lspace.codec.turtle.DecoderSpec")
  val decoder: lspace.codec.turtle.Decoder = lspace.codec.turtle.Decoder(graph)
  import monix.execution.Scheduler.Implicits.global

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
        .map { turtle =>
          println(turtle)
          import decoder._
          turtle.process
          1 shouldBe 1
        }
        .runToFuture
    }
  }
}
