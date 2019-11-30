package lspace.services.rest.endpoints

import com.twitter.finagle.http.{Request, Response, Status}
import io.finch.{Application, Bootstrap, Endpoint, Input, Text}
import lspace.codec.argonaut._
import lspace.codec.{ActiveContext, ActiveProperty}
import lspace.provider.detached.DetachedGraph
import lspace.provider.mem.MemGraph
import lspace.services.LApplication
import lspace._
import lspace.Label.D
import lspace.structure.Property.default.`@id`
import lspace.util.SampleGraph
import monix.eval.Task
import org.scalatest.{BeforeAndAfterAll, FutureOutcome}
import shapeless.{:+:, CNil}
import lspace.services.util._

import scala.collection.immutable.ListMap
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class LabeledNodeApiSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {

  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  lazy val sampleGraph: Graph = MemGraph("ApiServiceSpec")
  implicit val encoder        = lspace.codec.json.jsonld.JsonLDEncoder(nativeEncoder)
  implicit val decoder        = lspace.codec.json.jsonld.JsonLDDecoder(DetachedGraph)(nativeDecoder)
  import lspace.Implicits.AsyncGuide.guide

  import lspace.encode.EncodeText._
  import lspace.encode.EncodeJson._
  import lspace.encode.EncodeJsonLD._
  import lspace.services.codecs.Encode._

  val initTask = (for {
    sample <- SampleGraph.loadSocial(sampleGraph)
  } yield sample).memoizeOnSuccess

//  override def afterAll(): Unit = {
//    (for {
//      _ <- sampleGraph.close()
//    } yield ()).timeout(5.seconds).runToFuture
//  }

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    new FutureOutcome(initTask.runToFuture flatMap { result =>
      super.withFixture(test).toFuture
    })
  }

  val person     = SampleGraph.Person
  val personKeys = SampleGraph.Person.keys
  case class Person(name: String, id: Option[String] = None)

  import argonaut._, Argonaut._
  implicit def PersonCodecJson =
    casecodec2(Person.apply, Person.unapply)("name", "id")
  implicit val enc = PersonCodecJson.Encoder

  implicit lazy val activeContext = ActiveContext(
    `@prefix` = ListMap(
      "naam"      -> "name",
      "naam_naam" -> "name"
    ),
    definitions = Map(
      "knows"         -> ActiveProperty(person.keys.knows, `@type` = person.ontology :: Nil)(),
      "name"          -> ActiveProperty(person.keys.name, `@type` = D.`@string` :: Nil)(),
      "waardering"    -> ActiveProperty(person.keys.rate, `@type` = D.`@int` :: Nil)(),
      "geboortedatum" -> ActiveProperty(person.keys.birthDate, `@type` = D.`@date` :: Nil)()
    )
  )
  lazy val personApiService: LabeledNodeApi[_] = LabeledNodeApi(sampleGraph, person)

  val toCC = { node: Node =>
    Person(node.out(person.keys.nameString).headOption.getOrElse(""), node.out(`@id` as D.`@string`).headOption)
  }
  val toNode = { cc: Person =>
    for {
      node <- DetachedGraph.nodes.create(person)
      _    <- Task.sequence(cc.id.toList.map(node --- Property.default.`@id` --> _))
//      _    <- node --- person.keys.name --> cc.name
      _ <- node --- person.keys.name --> cc.name
    } yield node
  }

  lazy val service: com.twitter.finagle.Service[Request, Response] = Bootstrap
    .configure(enableMethodNotAllowed = false, enableUnsupportedMediaType = true)
    .serve[LApplication.JsonLD :+: Application.Json :+: CNil](personApiService.api)
    .serve[Text.Html :+: LApplication.JsonLD :+: Application.Json :+: CNil](personApiService.context)
    .toService

  "An LabeledNodeApi" should {
    "serve an active context" in {
      Task {
        val input = Input
          .get("/context")
          .withHeaders("Accept" -> "application/ld+json")
        personApiService
          .context(input)
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Ok
          }
          .getOrElse(fail("endpoint does not match"))
      }.runToFuture
    }
    "support GET with application/ld+json" in {
      Task {
        val input = Input
          .get("/123")
          .withHeaders("Accept" -> "application/ld+json")
        personApiService
          .list(input)
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Ok
            response.value.t.nonEmpty shouldBe true
            val node = response.value.t.head
            node.asInstanceOf[Node].out(person.keys.nameString).head shouldBe "Yoshio"
          }
          .getOrElse(fail("endpoint does not match"))
        personApiService
          .list(
            Input
              .get("/123/naam")
              .withHeaders("Accept" -> "application/ld+json"))
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Ok
            response.value.t.nonEmpty shouldBe true
            response.value.t.head shouldBe "Yoshio"
          }
          .getOrElse(fail("endpoint does not match"))
        personApiService
          .byId(Input
            .get("/0000"))
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.NotFound
          }
          .getOrElse(fail("endpoint does not match"))
      }.runToFuture
    }
    "support POST with application/ld+json" in {
      (for {
        alice <- toNode(Person("Alice"))
        ali   <- toNode(Person("Ali"))
        input = Input
          .post("/")
          .withBody[LApplication.JsonLD](alice)
          .withHeaders("Accept" -> "application/ld+json")
      } yield {
        personApiService
          .create(input)
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Created
            val createdNode = response.value.t
            createdNode.labels should contain only person.ontology
            createdNode.out(person.keys.nameString).head shouldBe "Alice"

            personApiService
              .create(
                Input
                  .post("/")
                  .withBody[LApplication.JsonLD](ali)
                  .withHeaders("Accept" -> "application/ld+json"))
              .awaitOutput()
              .map { output =>
                output.isRight shouldBe true
                val response = output.right.get
                response.status shouldBe Status.Created
                val node = response.value.t
                createdNode.iri should not be node.iri
                node.out(person.keys.nameString).head shouldBe "Ali"
              }
              .getOrElse(fail("endpoint does not match"))
          }
          .getOrElse(fail("endpoint does not match"))
      }).runToFuture
    }
    "support PUT with application/ld+json" in {
      (for {
        alice       <- toNode(Person("Alice"))
        ali         <- toNode(Person("Ali"))
        countBefore <- lspace.g.N.hasLabel(person).count().withGraph(sampleGraph).headF
        response = {
          personApiService
            .create(
              Input
                .post("/")
                .withBody[LApplication.JsonLD](alice)
                .withHeaders("Accept" -> "application/ld+json"))
            .awaitOutput()
            .map { output =>
              output.isRight shouldBe true
              val response = output.right.get
              response.status shouldBe Status.Created
              val createdNode = response.value.t

              personApiService
                .replaceById(
                  Input
                    .put(s"/${createdNode.iri.reverse.takeWhile(_ != '/').reverse}")
                    .withBody[LApplication.JsonLD](ali)
                    .withHeaders("Accept" -> "application/ld+json"))
                .awaitOutput()
                .map { output =>
                  output.isRight shouldBe true
                  val response = output.right.get
                  response.status shouldBe Status.Ok
                  val node = response.value.t
                  createdNode.iri shouldBe node.iri
                  createdNode.id shouldBe node.id
                  node.out(person.keys.nameString).head shouldBe "Ali"
                  createdNode.out(person.keys.nameString).head shouldBe "Ali"
                }
                .getOrElse(fail("endpoint does not match"))
            }
            .getOrElse(fail("endpoint does not match"))
        }
        countAfter <- lspace.g.N.hasLabel(person).count().withGraph(sampleGraph).headF
      } yield {
        response
        countBefore should be < countAfter
        succeed
      }).runToFuture
    }
    "support PATCH with application/ld+json" in {
      (for {
        alice <- toNode(Person("Alice"))
        ali   <- toNode(Person("Ali"))
      } yield {
        personApiService
          .create(
            Input
              .post("/")
              .withBody[LApplication.JsonLD](alice)
              .withHeaders("Accept" -> "application/ld+json"))
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Created
            val createdNode = response.value.t

            personApiService
              .updateById(
                Input
                  .patch(s"/${createdNode.iri.reverse.takeWhile(_ != '/').reverse}")
                  .withBody[LApplication.JsonLD](ali)
                  .withHeaders("Accept" -> "application/ld+json"))
              .awaitOutput()
              .map { output =>
                output.isRight shouldBe true
                val response = output.right.get
                response.status shouldBe Status.Ok
                val node = response.value.t
                createdNode.iri shouldBe node.iri
                createdNode.out(person.keys.nameString).head shouldBe "Ali"
              }
              .getOrElse(fail("endpoint does not match"))
          }
          .getOrElse(fail("endpoint does not match"))
      }).runToFuture
    }
    "support DELETE with application/ld+json" in {
      (for {
        alice <- toNode(Person("Alice"))
      } yield {
        personApiService
          .create(
            Input
              .post("/")
              .withBody[LApplication.JsonLD](alice)
              .withHeaders("Accept" -> "application/ld+json"))
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Created
            val createdNode = response.value.t

            personApiService
              .removeById(Input
                .delete(s"/${createdNode.iri.reverse.takeWhile(_ != '/').reverse}"))
              .awaitOutput()
              .map { output =>
                output.isRight shouldBe true
                val response = output.right.get
                response.status shouldBe Status.NoContent
              }
              .getOrElse(fail("endpoint does not match"))
          }
          .getOrElse(fail("endpoint does not match"))
      }).runToFuture
    }
    "support PUT with application/json" in {
      (for {
        alice2 <- toNode(Person("Alice"))
        ali    <- toNode(Person("Ali"))
      } yield {
        val input = Input
          .post("/")
          .withBody[Application.Json](alice2)
          .withHeaders("Accept" -> "application/json")
        personApiService
          .create(input)
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Created
            val createdNode = response.value.t

            personApiService
              .replaceById(
                Input
                  .put(s"/${createdNode.iri.reverse.takeWhile(_ != '/').reverse}")
                  .withBody[LApplication.JsonLD](ali)
                  .withHeaders("Accept" -> "application/json"))
              .awaitOutput()
              .map { output =>
                output.isRight shouldBe true
                val response = output.right.get
                response.status shouldBe Status.Ok
                val node = response.value.t
                createdNode.iri shouldBe node.iri
                createdNode.id shouldBe node.id
                node.out(person.keys.nameString).head shouldBe "Ali"
                createdNode.out(person.keys.nameString).head shouldBe "Ali"
              }
              .getOrElse(fail("endpoint does not match"))
          }
          .getOrElse(fail("endpoint does not match"))
      }).runToFuture
    }
    "support PATCH with application/json" in {
      (for {
        alice <- toNode(Person("Alice"))
        ali   <- toNode(Person("Ali"))
      } yield {
        personApiService
          .create(
            Input
              .post("/")
              .withBody[LApplication.JsonLD](alice)
              .withHeaders("Accept" -> "application/ld+json"))
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Created
            val createdNode = response.value.t

            personApiService
              .updateById(
                Input
                  .patch(s"/${createdNode.iri.reverse.takeWhile(_ != '/').reverse}")
                  .withBody[LApplication.JsonLD](ali)
                  .withHeaders("Accept" -> "application/ld+json"))
              .awaitOutput()
              .map { output =>
                output.isRight shouldBe true
                val response = output.right.get
                response.status shouldBe Status.Ok
                val node = response.value.t
                createdNode.iri shouldBe node.iri
                createdNode.out(person.keys.nameString).head shouldBe "Ali"
              }
              .getOrElse(fail("endpoint does not match"))
          }
          .getOrElse(fail("endpoint does not match"))
      }).runToFuture
    }
    "support DELETE with application/json" in {
      (for {
        alice <- toNode(Person("Alice"))
      } yield {
        personApiService
          .create(
            Input
              .post("/")
              .withBody[LApplication.JsonLD](alice)
              .withHeaders("Accept" -> "application/ld+json"))
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Created
            val createdNode = response.value.t

            personApiService
              .removeById(Input
                .delete(s"/${createdNode.iri.reverse.takeWhile(_ != '/').reverse}"))
              .awaitOutput()
              .map { output =>
                output.isRight shouldBe true
                val response = output.right.get
                response.status shouldBe Status.NoContent
              }
              .getOrElse(fail("endpoint does not match"))
          }
          .getOrElse(fail("endpoint does not match"))
      }).runToFuture
    }
  }

  "A compiled LabeledNodeApi service" should {
    "serve an active context" in {
      val input = Input
        .get("/context")
        .withHeaders("Accept" -> "application/ld+json")

      service(input.request).map { r =>
        r.contentType shouldBe Some("application/ld+json")
        r.status shouldBe Status.Ok
      }
    }
    "serve an active @context" in {
      val input = Input
        .get("/@context")
        .withHeaders("Accept" -> "application/ld+json")

      service(input.request).map { r =>
        r.contentType shouldBe Some("application/ld+json")
        r.status shouldBe Status.Ok
        r.contentString shouldBe """{"@context":{"naam":{"@id":"name","@type":"@string"},"naam_naam":"name","knows":{"@id":"https://example.org/knows","@type":"https://example.org/Person"},"waardering":{"@id":"rate","@type":"@int"},"geboortedatum":{"@id":"https://example.org/birthDate","@type":"@date"}}}"""
      }
    }
    "support GET with application/json" in {
      val input = Input
        .get("/")
        .withHeaders("Accept" -> "application/json")
      service(input.request).map { r =>
        r.contentType shouldBe Some("application/json")
        r.status shouldBe Status.Ok
        r.contentString should not be """{"@context":{"naam":{"@id":"name","@type":"@string"},"naam_naam":"name","knows":{"@id":"https://example.org/knows","@type":"https://example.org/Person"},"waardering":{"@id":"rate","@type":"@int"},"geboortedatum":{"@id":"https://example.org/birthDate","@type":"@date"}}}"""
      }
    }
    "not support GET with text/html" in {
      val input = Input
        .get("/context")
        .withHeaders("Accept" -> "text/html")
      service(input.request).map { r =>
        r.contentString shouldBe """{"@context":{"naam":{"@id":"name","@type":"@string"},"naam_naam":"name","knows":{"@id":"https://example.org/knows","@type":"https://example.org/Person"},"waardering":{"@id":"rate","@type":"@int"},"geboortedatum":{"@id":"https://example.org/birthDate","@type":"@date"}}}"""
        r.contentType shouldBe Some("text/html")
        r.status shouldBe Status.Ok
      }
    }
    "support GET /123/naam_naam" in {
      service(
        Input
          .get("/123/naam_naam")
          .withHeaders("Accept" -> "application/json")
          .request)
        .map { output =>
          output.status shouldBe Status.Ok
          output.contentString shouldBe """[{"@value":"Yoshio","@type":"@string"}]"""
        }
    }
    "support GET with application/ld+json" in {
      val input = Input
        .get("/")
        .withHeaders("Accept" -> "application/ld+json")
      service(input.request).map { r =>
        r.contentType shouldBe Some("application/ld+json")
        r.status shouldBe Status.Ok
      }
    }
    "support POST with application/json" in {
      (for {
        alice2 <- toNode(Person("Alice"))
        input = Input
          .post("/")
          .withBody[Application.Json](alice2)
          .withHeaders("Accept" -> "application/json")
        _ <- Task.deferFuture(service(input.request)).map { r =>
          r.status shouldBe Status.Created
        }
      } yield succeed).runToFuture
    }
  }
}
