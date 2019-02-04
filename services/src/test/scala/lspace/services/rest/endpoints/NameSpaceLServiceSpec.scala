package lspace.services.rest.endpoints

import io.finch.Input
import lspace.librarian.process.traversal.{P, Step}
import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure.Graph
import lspace.parse.JsonLD
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

class NameSpaceLServiceSpec extends WordSpec with Matchers with BeforeAndAfterAll {

  lazy val graph: Graph = MemGraph("https://ns.l-space.eu")
  val jsonld            = JsonLD(graph)
  lazy val nsService    = NameSpaceService(graph)

  P.predicates.map(_.ontology).foreach(graph.ns.ontologies.store)
  Step.steps.map(_.ontology).foreach(graph.ns.ontologies.store)

  "The nsService" can {
    "return namespace-resources in applicication/ld+json" in {
      val input = Input
        .get("/librarian/p/Eqv")
        .withHeaders("Accept" -> "application/ld+json")
      nsService.getResource(input).awaitOutput().map { output =>
        output.isRight shouldBe true
        output.right.get.value.toString shouldBe """{"@label":{"en":"Eqv"},"@extends":["https://ns.l-space.eu/librarian/p/EqP"],"@type":"@class","@properties":"https://ns.l-space.eu/librarian/p/value","@id":"https://ns.l-space.eu/librarian/p/Eqv"}"""
      }
    }
  }
}
