package lspace.services.rest.endpoints

import com.twitter.finagle.http.{Request, Response, Status}
import io.finch.{Application, Bootstrap, Input, Text}
import lspace.encode.EncodeJsonLD
import lspace.librarian.datatype.TextType
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default.`@id`
import lspace.librarian.util.SampleGraph
import lspace.parse.ActiveContext
import lspace.parse.JsonLD
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import shapeless.{:+:, CNil, Poly1}

class ApiServiceSpec extends WordSpec with Matchers with BeforeAndAfterAll {

  lazy val sampleGraph: Graph = MemGraph("ApiServiceSpec")
  implicit val jsonld         = JsonLD(sampleGraph)

  override def beforeAll(): Unit = {
    SampleGraph.loadSocial(sampleGraph)
  }

  val person     = SampleGraph.Person
  val personKeys = SampleGraph.Person.keys
  case class Person(name: String, id: Option[String])

  import argonaut._, Argonaut._
  implicit def PersonCodecJson =
    casecodec2(Person.apply, Person.unapply)("name", "id")
  implicit val enc = PersonCodecJson.Encoder

  lazy val personApiService = ApiService(person)(sampleGraph)
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
  import JsonLDModule.Encode._

  implicit val nodeToJson: EncodeJson[Node] =
    EncodeJson { node: Node =>
      Json.jString("ab")
    }

  implicit val labeledNodeToJson: EncodeJson[(Ontology, Node)] =
    EncodeJson {
      case (ontology: Ontology, node: Node) =>
        Json.jString("ab")
    }

  implicit val labeledPagedResultToJson: EncodeJson[(Ontology, PagedResult)] =
    EncodeJson {
      case (ontology: Ontology, pr: PagedResult) =>
        pr.result.map(_.asJson).asJson
    }
//  implicit val pagedResultToJson: EncodeJson[PagedResult] =
//    EncodeJson { pr: PagedResult =>
//      pr.result.map(_.asJson).asJson
//    }

  implicit def pagedResultToJsonLD(implicit jsonld: lspace.parse.JsonLD) = new EncodeJsonLD[(Ontology, PagedResult)] {
    val encode: ((Ontology, PagedResult)) => Json = {
      case (ontology: Ontology, pr: PagedResult) =>
        Json.jObject(jsonld.encode.fromAny(pr.result)(ActiveContext()).withContext)
    }
  }

  object labelResult extends Poly1 {
    implicit def caseNode        = at[Node](node => (person.ontology, node))
    implicit def casePagedResult = at[PagedResult](pagedResult => (person.ontology, pagedResult))
  }

  implicit def nodeToJsonLD(implicit jsonld: lspace.parse.JsonLD) = new EncodeJsonLD[(Ontology, Node)] {
    val encode: ((Ontology, Node)) => Json = {
      case (ontology: Ontology, node: Node) => jsonld.encode(node)
    }
  }
//  import EncodeJsonLD._
//  import lspace.encode.EncodeJsonLD._

  lazy val service: com.twitter.finagle.Service[Request, Response] = Bootstrap
    .configure(enableMethodNotAllowed = true, enableUnsupportedMediaType = true)
    .serve[JsonLDModule.JsonLD :+: Application.Json :+: CNil](
      personApiService.api.mapOutput(p => io.finch.Ok(p map labelResult)).handle {
        case e: Exception => io.finch.InternalServerError(e)
        case e            => io.finch.BadRequest(new Exception("bad request"))
      })
    .toService

  "The personApiService" should {
    "return a Ok(Person) for a known id on a get-request" in {
      val input = Input
        .get("/123")
      personApiService.getById(input).awaitOutput().map { output =>
        output.isRight shouldBe true
        val response = output.right.get
        response.status shouldBe Status.Ok
        val node = response.value
        node.out(person.keys.nameString).head shouldBe "Yoshio"
      }
    }
    "return NotFound for an unknown id on a get-request" in {
      val input = Input
        .get("/0000")
      personApiService.getById(input).awaitOutput().map { output =>
        output.isRight shouldBe true
        val response = output.right.get
        response.status shouldBe Status.NotFound
      }
    }
  }
}
