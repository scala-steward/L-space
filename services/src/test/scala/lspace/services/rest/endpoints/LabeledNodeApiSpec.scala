package lspace.services.rest.endpoints

import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.util.Await
import io.finch.{Application, Bootstrap, Input}
import lspace.encode.EncodeJsonLD
import lspace.services.codecs.{Application => LApplication}
import lspace.librarian.datatype.TextType
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default.`@id`
import lspace.librarian.util.SampleGraph
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import shapeless.{:+:, CNil}

object LabeledNodeApiSpec {}
class LabeledNodeApiSpec extends WordSpec with Matchers with BeforeAndAfterAll {

  lazy val sampleGraph: Graph = MemGraph("ApiServiceSpec")
  implicit val encoder        = lspace.codec.argonaut.Encoder
//  implicit val decoder        = lspace.codec.argonaut.Decode(sampleGraph)

  override def beforeAll(): Unit = {
    SampleGraph.loadSocial(sampleGraph)
  }

  val person     = SampleGraph.Person
  val personKeys = SampleGraph.Person.keys
  case class Person(name: String, id: Option[String] = None)

  import argonaut._, Argonaut._
  implicit def PersonCodecJson =
    casecodec2(Person.apply, Person.unapply)("name", "id")
  implicit val enc = PersonCodecJson.Encoder

  lazy val personApiService = LabeledNodeApi(person)(sampleGraph)
  val toCC = { node: Node =>
    Person(node.out(person.keys.nameString).headOption.getOrElse(""), node.out(`@id` as TextType).headOption)
  }
  val toNode = { cc: Person =>
    val node = DetachedGraph.nodes.create(person)
    cc.id.foreach(node --- Property.default.`@id` --> _)
    node --- person.keys.name --> cc.name
    node
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
    }
    "support POST with application/ld+json" in {
      val input = Input
        .post("/")
        .withBody[LApplication.JsonLD](toNode(Person("Alice")))
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
              .withBody[LApplication.JsonLD](toNode(Person("Ali"))))
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
    }
    "support PUT with application/ld+json" in {
      personApiService
        .create(
          Input
            .post("/")
            .withBody[LApplication.JsonLD](toNode(Person("Alice"))))
        .awaitOutput()
        .map { output =>
          output.isRight shouldBe true
          val response = output.right.get
          response.status shouldBe Status.Created
          val createdNode = response.value

          personApiService
            .replaceById(Input
              .put(s"/${createdNode.iri.reverse.takeWhile(_ != '/').reverse}")
              .withBody[LApplication.JsonLD](toNode(Person("Ali"))))
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
    }
    "support PATCH with application/ld+json" in {
      personApiService
        .create(
          Input
            .post("/")
            .withBody[LApplication.JsonLD](toNode(Person("Alice"))))
        .awaitOutput()
        .map { output =>
          output.isRight shouldBe true
          val response = output.right.get
          response.status shouldBe Status.Created
          val createdNode = response.value

          personApiService
            .updateById(Input
              .patch(s"/${createdNode.iri.reverse.takeWhile(_ != '/').reverse}")
              .withBody[LApplication.JsonLD](toNode(Person("Ali"))))
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
    }
    "support DELETE with application/ld+json" in {
      personApiService
        .create(
          Input
            .post("/")
            .withBody[LApplication.JsonLD](toNode(Person("Alice"))))
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
      val input = Input
        .post("/")
        .withBody[Application.Json](toNode(Person("Alice2")))
        .withHeaders("Accept" -> "application/json")
      Await.result(service(input.request).map { r =>
        r.status shouldBe Status.Created
      })
    }
    "support PUT with application/json" in {
      val input = Input
        .post("/")
        .withBody[Application.Json](toNode(Person("Alice2")))
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
                .withBody[LApplication.JsonLD](toNode(Person("Ali")))
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
    }
    "support PATCH with application/json" in {
      personApiService
        .create(
          Input
            .post("/")
            .withBody[LApplication.JsonLD](toNode(Person("Alice"))))
        .awaitOutput()
        .map { output =>
          output.isRight shouldBe true
          val response = output.right.get
          response.status shouldBe Status.Created
          val createdNode = response.value

          personApiService
            .updateById(Input
              .patch(s"/${createdNode.iri.reverse.takeWhile(_ != '/').reverse}")
              .withBody[LApplication.JsonLD](toNode(Person("Ali"))))
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
    }
    "support DELETE with application/json" in {
      personApiService
        .create(
          Input
            .post("/")
            .withBody[LApplication.JsonLD](toNode(Person("Alice"))))
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
    }
  }
}
