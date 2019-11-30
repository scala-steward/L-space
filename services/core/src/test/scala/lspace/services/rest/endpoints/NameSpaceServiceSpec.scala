package lspace.services.rest.endpoints

import io.finch.Input
import lspace._
import lspace.codec.{ActiveContext, ActiveProperty}
import lspace.codec.argonaut.{nativeDecoder, nativeEncoder}
import lspace.librarian.traversal.Step
import lspace.provider.detached.DetachedGraph
import lspace.provider.mem.MemGraph
import lspace.structure.Graph
import monix.eval.Task
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, FutureOutcome, Matchers}

import scala.collection.immutable.ListMap
import scala.concurrent.Future
import scala.concurrent.duration._

class NameSpaceServiceSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {

  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  implicit lazy val activeContext = ActiveContext(
    `@prefix` = ListMap(),
    definitions = Map()
  )

  lazy val graph: Graph = MemGraph("https://ns.l-space.eu")
  implicit val encoder  = lspace.codec.json.jsonld.JsonLDEncoder(nativeEncoder)
  implicit val decoder  = lspace.codec.json.jsonld.JsonLDDecoder(DetachedGraph)(nativeDecoder)
  import lspace.Implicits.AsyncGuide.guide
  lazy val nsService = NameSpaceService(graph)

  val initTask = (for {
    _ <- Task.sequence(P.predicates.map(_.ontology).map(graph.ns.ontologies.store))
    _ <- Task.sequence(Step.steps.map(_.ontology).map(graph.ns.ontologies.store))
  } yield ()).memoizeOnSuccess

//  override def afterAll(): Unit = {
//    (for {
//      _ <- graph.close()
//    } yield ()).timeout(5.seconds).runToFuture
//  }

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    new FutureOutcome(initTask.runToFuture flatMap { result =>
      super.withFixture(test).toFuture
    })
  }

  "The nsService" can {
    "return namespace-resources in applicication/ld+json" in {
      Future {
        val input = Input
          .get("/librarian/p/Eqv")
          .withHeaders("Accept" -> "application/ld+json")
        nsService
          .getResource(input)
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            output.right.get.value.toString shouldBe
              """{"@id":"https://ns.l-space.eu/librarian/p/Eqv","@type":"@class","@label":{"en":"Eqv"},"@extends":["https://ns.l-space.eu/librarian/p/EqP"],"@properties":"https://ns.l-space.eu/librarian/p/value"}"""
          }
          .get
      }
    }
  }
}
