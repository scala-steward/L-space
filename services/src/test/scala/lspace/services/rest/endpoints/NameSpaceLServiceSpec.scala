package lspace.services.rest.endpoints

import io.finch.Input
import lspace._
import lspace.librarian.traversal.Step
import lspace.provider.mem.MemGraph
import lspace.structure.Graph
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

class NameSpaceLServiceSpec extends WordSpec with Matchers with BeforeAndAfterAll {

  lazy val graph: Graph = MemGraph("https://ns.l-space.eu")
  implicit val nencoder = lspace.codec.argonaut.NativeTypeEncoder
  implicit val encoder  = lspace.codec.Encoder(nencoder)
  implicit val ndecoder = lspace.codec.argonaut.NativeTypeDecoder
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
        output.right.get.value.toString shouldBe
          """{"@id":"https://ns.l-space.eu/librarian/p/Eqv","@type":"@class","@label":{"en":"Eqv"},"@extends":["https://ns.l-space.eu/librarian/p/EqP"],"@properties":"https://ns.l-space.eu/librarian/p/value"}"""
      }
    }
  }
}