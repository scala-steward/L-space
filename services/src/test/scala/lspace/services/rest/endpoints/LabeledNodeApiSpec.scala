package lspace.services.rest.endpoints

import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.util.Await
import io.finch.{Application, Bootstrap, Input}
import lspace.encode.EncodeJsonLD
import lspace.services.codecs.{Application => LApplication}
import lspace.datatype.TextType
import lspace.provider.detached.DetachedGraph
import lspace.provider.mem.MemGraph
import lspace.structure._
import lspace.structure.Property.default.`@id`
import lspace.util.SampleGraph
import monix.eval.Task
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, FutureOutcome, Matchers}
import shapeless.{:+:, CNil}
import lspace.services.util._

import scala.concurrent.Future
import scala.concurrent.duration._

class LabeledNodeApiSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {

  import lspace.Implicits.Scheduler.global
//  override def executionContext = lspace.Implicits.Scheduler.global

  lazy val sampleGraph: Graph = MemGraph("ApiServiceSpec")
  implicit val nencoder       = lspace.codec.argonaut.NativeTypeEncoder
  implicit val encoder        = lspace.codec.Encoder(nencoder)
  implicit val ndecoder       = lspace.codec.argonaut.NativeTypeDecoder

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

  lazy val personApiService = LabeledNodeApi(person)(sampleGraph, ndecoder)
  val toCC = { node: Node =>
    Person(node.out(person.keys.nameString).headOption.getOrElse(""), node.out(`@id` as TextType).headOption)
  }
  val toNode = { cc: Person =>
    for {
      node <- DetachedGraph.nodes.create(person)
      _    <- Task.sequence(cc.id.toList.map(node --- Property.default.`@id` --> _))
      _    <- node --- person.keys.name --> cc.name
    } yield node
  }

  import lspace.encode.EncodeJson._
  import lspace.encode.EncodeJsonLD._
  import lspace.services.codecs.Encode._

  lazy val service: com.twitter.finagle.Service[Request, Response] = Bootstrap
    .configure(enableMethodNotAllowed = true, enableUnsupportedMediaType = true)
    .serve[LApplication.JsonLD :+: Application.Json :+: CNil](personApiService.api)
    .toService

  "An LabeledNodeApi" should {
    "support GET with application/ld+json" in {
      Task {
        val input = Input
          .get("/123")
        personApiService
          .byId(input)
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Ok
            val node = response.value
            node.out(person.keys.nameString).head shouldBe "Yoshio"
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
      } yield {
        personApiService
          .create(input)
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Created
            val createdNode = response.value
            createdNode.out(person.keys.nameString).head shouldBe "Alice"

            personApiService
              .create(Input
                .post("/")
                .withBody[LApplication.JsonLD](ali))
              .awaitOutput()
              .map { output =>
                output.isRight shouldBe true
                val response = output.right.get
                response.status shouldBe Status.Created
                val node = response.value
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
        alice <- toNode(Person("Alice"))
        ali   <- toNode(Person("Ali"))
      } yield {
        personApiService
          .create(
            Input
              .post("/")
              .withBody[LApplication.JsonLD](alice))
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Created
            val createdNode = response.value

            personApiService
              .replaceById(Input
                .put(s"/${createdNode.iri.reverse.takeWhile(_ != '/').reverse}")
                .withBody[LApplication.JsonLD](ali))
              .awaitOutput()
              .map { output =>
                output.isRight shouldBe true
                val response = output.right.get
                response.status shouldBe Status.Ok
                val node = response.value
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
    "support PATCH with application/ld+json" in {
      (for {
        alice <- toNode(Person("Alice"))
        ali   <- toNode(Person("Ali"))
      } yield {
        personApiService
          .create(
            Input
              .post("/")
              .withBody[LApplication.JsonLD](alice))
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Created
            val createdNode = response.value

            personApiService
              .updateById(Input
                .patch(s"/${createdNode.iri.reverse.takeWhile(_ != '/').reverse}")
                .withBody[LApplication.JsonLD](ali))
              .awaitOutput()
              .map { output =>
                output.isRight shouldBe true
                val response = output.right.get
                response.status shouldBe Status.Ok
                val node = response.value
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
              .withBody[LApplication.JsonLD](alice))
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Created
            val createdNode = response.value

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
    "support GET with application/json" in {
      val input = Input
        .get("/")
        .withHeaders("Accept" -> "application/json")
      Await.result(service(input.request).map { r =>
        r.contentType shouldBe Some("application/json")
        r.status shouldBe Status.Ok
      })
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
            val createdNode = response.value

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
                val node = response.value
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
              .withBody[LApplication.JsonLD](alice))
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Created
            val createdNode = response.value

            personApiService
              .updateById(Input
                .patch(s"/${createdNode.iri.reverse.takeWhile(_ != '/').reverse}")
                .withBody[LApplication.JsonLD](ali))
              .awaitOutput()
              .map { output =>
                output.isRight shouldBe true
                val response = output.right.get
                response.status shouldBe Status.Ok
                val node = response.value
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
              .withBody[LApplication.JsonLD](alice))
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Created
            val createdNode = response.value

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

  "A LabeledNodeApi service" should {}
}
