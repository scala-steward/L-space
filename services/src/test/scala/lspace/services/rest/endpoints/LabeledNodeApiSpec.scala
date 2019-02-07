package lspace.services.rest.endpoints

import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.server.TwitterServer
import com.twitter.util.Await
import io.finch.{Application, Bootstrap, Input, Text}
import lspace.encode.EncodeJsonLD
import lspace.services.codecs.{Application => LApplication}
import lspace.librarian.datatype.TextType
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default.`@id`
import lspace.librarian.util.SampleGraph
import lspace.parse.ActiveContext
import org.scalatest.{BeforeAndAfterAll, Failed, Matchers, WordSpec}
import shapeless.{:+:, CNil, Poly1}

object LabeledNodeApiSpec {}
class LabeledNodeApiSpec extends WordSpec with Matchers with BeforeAndAfterAll {

  lazy val sampleGraph: Graph = MemGraph("ApiServiceSpec")
  implicit val encoder        = lspace.codec.argonaut.Encode
  implicit val decoder        = lspace.codec.argonaut.Decode(sampleGraph)

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

  import argonaut._
  import argonaut.Argonaut._
  import io.finch.argonaut.preserveOrder._
  import lspace.services.codecs
  import lspace.services.codecs.Encode._

//  implicit val nodeToJson: lspace.encode.EncodeJson[Node] =
//    EncodeJson { node: Node =>
//      node
//        .outEMap()
//        .map {
//          case (property, edges) =>
//            property.label("en") -> (edges match {
//              case List(e) => encoder.fromAny(e.to, Some(e.to.labels.head))(ActiveContext()).json
//            })
//        }
//        .asJson
//    }

//  implicit val labeledNodeToJson: EncodeJson[(Ontology, Node)] =
//    EncodeJson {
//      case (ontology: Ontology, node: Node) =>
//        Json.jString("ab")
//    }

//  implicit val labeledPagedResultToJson: EncodeJson[(Ontology, List[Node])] =
//    EncodeJson {
//      case (ontology: Ontology, nodes: List[Node]) =>
//        nodes.map(_.asJson).asJson
//    }
//  implicit val pagedResultToJson: EncodeJson[PagedResult] =
//    EncodeJson { pr: PagedResult =>
//      pr.result.map(_.asJson).asJson
//    }

//  import lspace.parse.JsonInProgress._
//  implicit def pagedResultToJsonLD = new EncodeJsonLD[(Ontology, List[Node])] {
//    val encode: ((Ontology, List[Node])) => Json = {
//      case (ontology: Ontology, nodes: List[Node]) =>
//        Json.jObject(encoder.fromAny(nodes)(ActiveContext()).withContext)
//    }
//  }

//  object labelResult extends Poly1 {
//    implicit def caseNode        = at[Node](node => (person.ontology, node))
//    implicit def casePagedResult = at[List[Node]](nodes => (person.ontology, nodes))
//  }

//  implicit def nodeToJsonLD(implicit jsonld: lspace.parse.JsonLD) = new EncodeJsonLD[(Ontology, Node)] {
//    val encode: ((Ontology, Node)) => Json = {
//      case (ontology: Ontology, node: Node) => jsonld.encode(node)
//    }
//  }

  lazy val service: com.twitter.finagle.Service[Request, Response] = Bootstrap
    .configure(enableMethodNotAllowed = true, enableUnsupportedMediaType = true)
    .serve[LApplication.JsonLD :+: Application.Json :+: CNil](personApiService.api)
    .toService

  "The personApiService" should {
    "return a Ok(Person) for a known id on a get-request" in {
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
    }
    "return NotFound for an unknown id on a get-request" in {
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
    "create a person with new id on a post-request" in {
      import EncodeJsonLD._
      import lspace.encode.EncodeJsonLD._
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
    "replace a person with on a put-request" in {
      import EncodeJsonLD._
      import lspace.encode.EncodeJsonLD._
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
    "update a person with on a patch-request" in {
      import EncodeJsonLD._
      import lspace.encode.EncodeJsonLD._
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
    "remove a person on a delete-request" in {
      import EncodeJsonLD._
      import lspace.encode.EncodeJsonLD._
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
    "update a person with on a patch-request for Content-Type application/json" in {
      import EncodeJsonLD._
      import lspace.encode.EncodeJsonLD._
      val input = Input
        .post("/")
        .withBody[Application.Json](toNode(Person("Alice")))
      personApiService
        .create(input)
        .awaitOutput()
        .map { output =>
          if (output.isLeft) println(output.left.get.getMessage)
          output.isRight shouldBe true
          val response = output.right.get
          response.status shouldBe Status.Created
//          val createdNode = response.value

//          personApiService
//            .updateById(Input
//              .patch(s"/${createdNode.iri.reverse.takeWhile(_ != '/').reverse}")
//              .withBody[Application.Json](toNode(Person("Ali"))))
//            .awaitOutput()
//            .map { output =>
//              output.isRight shouldBe true
//              val response = output.right.get
//              response.status shouldBe Status.Ok
//              val node = response.value
//              createdNode.iri shouldBe node.iri
//              createdNode.out(person.keys.nameString).head shouldBe "Ali"
//            }
//            .getOrElse(fail("endpoint does not match"))
        }
        .getOrElse(fail("endpoint does not match"))
    }
    "s test" in {
      val input = Input
        .post("/")
        .withBody[Application.Json](toNode(Person("Alice2")))
      Await.result(service(input.request).map { r =>
        r.status shouldBe Status.Created
      })
    }
  }
}
