package lspace.services.rest.endpoints

import com.twitter.finagle
import com.twitter.finagle.http.{Request, Response, Status}
import io.finch.{Bootstrap, Input}
import lspace.librarian.process.traversal.{P, Step}
import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure.Graph
import lspace.parse.json.JsonLD
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import shapeless.{:+:, CNil}

class NameSpaceServiceSpec extends WordSpec with Matchers with BeforeAndAfterAll {

  lazy val graph: Graph = MemGraph("https://ns.l-space.eu")
  val jsonld            = JsonLD(graph)
  lazy val nsService    = NameSpaceService(graph)(jsonld)

  P.predicates.map(_.ontology).foreach(graph.ns.ontologies.store)
  Step.steps.map(_.ontology).foreach(graph.ns.ontologies.store)

  import lspace.server.util._
  "The nsService" can {
    "return namespace-resources in applicication/ld+json" in {
      val input = Input
        .get("/librarian/p/Eqv")
        .withHeaders("Accept" -> "application/ld+json")
      nsService.getResource(input).awaitOutput().map { output =>
        output.isRight shouldBe true
        val json = output.right.get.value
        println(json)
      }
    }
  }
}
