package lspace.server

import argonaut.Parse
import com.twitter.finagle
import com.twitter.finagle.http.{Request, Response, Status}
import io.finch.{Bootstrap, Input, Output}
import lspace.codec.Decoder
import lspace._
import lspace.librarian.traversal.Collection
import lspace.provider.detached.DetachedGraph
import lspace.provider.mem.MemGraph
import lspace.structure.{Graph, Node}
import lspace.services.SimpleGraphServer
import lspace.services.rest.endpoints.{NameSpaceService, TraversalService}
import lspace.util.SampleGraph
import org.scalatest._
import org.scalatest.Matchers
import shapeless.{:+:, CNil}

import scala.concurrent.Future
//import play.api.libs.json._

class SimpleGraphServerSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {

  import lspace.codec.argonaut._
  implicit val graph   = MemGraph("SimpleGraphServerSpec")
  implicit val encoder = lspace.codec.Encoder(nativeEncoder)
  implicit val decoder = Decoder(DetachedGraph)(nativeDecoder)

  import lspace.encode.EncodeJsonLD._

  val server: SimpleGraphServer = SimpleGraphServer(graph)

  override def beforeAll(): Unit = {
    SampleGraph.loadSocial(graph)
  }

  override def afterAll(): Unit = {
    server.service.close()
    graph.close()
  }

  import lspace.services.util._
  "a graph-server" should {
    "execute a traversal only on a POST request" in {

      val traversal = g.N.has(SampleGraph.properties.balance, P.gt(300)).count
      import lspace.encode.EncodeJson._
      import lspace.encode.EncodeJsonLD._
      import lspace.services.codecs
      import lspace.services.codecs.Encode._
      val input = Input
        .post("/traverse")
        .withBody[lspace.services.codecs.Application.JsonLD](traversal.toNode)
        .withHeaders("Accept" -> "application/ld+json")
      val res: Future[Response] = server.service(input.request)

      res.flatMap[org.scalatest.Assertion] { response =>
        val headers = response.headerMap
        response.status shouldBe Status.Ok
        response.contentType shouldBe Some("application/ld+json")
        decoder
          .parse(response.getContentString())
          .flatMap { json =>
            decoder
              .toNode(json)
              .map { node =>
                Collection.wrap(node).item shouldBe List(2)
              }
          }
          .runToFuture(monix.execution.Scheduler.global)
      }
    }
    "get all labels" in {
      val input = Input
        .get("/label")
      val res = server.service(input.request)

      res.map { response =>
        response.status shouldBe Status.Ok
      }
    }
  }
}