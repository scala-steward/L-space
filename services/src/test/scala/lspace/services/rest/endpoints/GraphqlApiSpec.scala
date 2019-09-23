package lspace.services.rest.endpoints

import com.twitter.finagle.http.{Request, Response, Status}
import io.finch.{Application, Bootstrap, Endpoint, Input, Text}
import lspace.codec.{ActiveContext, ActiveProperty}
import lspace.Label.D
import lspace.{codec, Graph, Ontology}
import monix.eval.Task
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, Matchers}
import lspace.services.util._
import lspace.util.SampleGraph
import shapeless.{:+:, CNil}

import scala.collection.immutable.ListMap

class GraphqlApiSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {

  val person = SampleGraph.Person

  implicit val activeContext = ActiveContext(
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
  import lspace.codec.argonaut._
  implicit val encoderJsonLD  = codec.json.jsonld.JsonLDEncoder.apply
  implicit val decoderGraphQL = codec.graphql.Decoder()
//  implicit val enc = lspace.encode.EncodeJson.queryResultToJson(encoderJsonLD)
  import lspace.services.codecs.Encode._
  import lspace.encode.EncodeJson._
  import lspace.Implicits.AsyncGuide.guide
  import lspace.Implicits.Scheduler.global
  lazy val graphqlService = GraphqlApi(Graph("GraphqlApiSpec"))
  lazy val service: com.twitter.finagle.Service[Request, Response] = Bootstrap
    .configure(enableMethodNotAllowed = true, enableUnsupportedMediaType = true)
    .serve[Application.Json :+: CNil](graphqlService
      .list(Ontology("Person")))
    .toService

  "support POST with application/graphql" in {
    (for {
      graphql <- Task.now(" { name geboortedatum waardering knows { name } } ")
      input = Input
        .post("/")
        .withBody[Text.Plain](graphql)
        .withHeaders("Accept" -> "application/json", "Content-Type" -> "application/graphql")
      _ <- Task.deferFuture(service(input.request)).map { r =>
        //          println(r.contentString)
        r.status shouldBe Status.Ok
      }
    } yield succeed).runToFuture
  }
}
