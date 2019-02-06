package lspace.server

import argonaut.Parse
import com.twitter.finagle
import com.twitter.finagle.http.{Request, Response, Status}
import io.finch.{Bootstrap, Input, Output}
import lspace.librarian.process.traversal.{Collection, P}
import lspace.librarian.provider.mem.{MemGraph, MemGraphDefault}
import lspace.librarian.structure.{Graph, Node}
import lspace.librarian.util.SampleGraph
import lspace.parse.JsonLD
import lspace.services.SimpleGraphServer
import lspace.services.rest.endpoints.{NameSpaceService, TraversalService}
import org.scalatest._
import org.scalatest.Matchers
import shapeless.{:+:, CNil}

import scala.concurrent.Future
//import play.api.libs.json._

class SimpleGraphServerSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {

  implicit val graph = MemGraph("SimpleGraphServerSpec")
  val jsonld         = JsonLD(graph)

  val server = new SimpleGraphServer(graph)

  //  val remoteGraph = RemoteGraph("SimpleGraphServerSpec")(server)

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

      val traversal = MemGraphDefault.g.N.has(SampleGraph.properties.balance, P.gt(300)).count
      import lspace.encode.EncodeJsonLD._
      import lspace.services.codecs.JsonLDModule
      import lspace.services.codecs.JsonLDModule.Encode._
      val input = Input
        .post("/traverse")
        .withBody[JsonLDModule.JsonLD](traversal.toNode)
        .withHeaders("Accept" -> "application/ld+json")
      val res: Future[Response] = server.service(input.request)

      res.flatMap[org.scalatest.Assertion] { response =>
        val headers = response.headerMap
        response.status shouldBe Status.Ok
        response.contentType shouldBe Some("application/ld+json")
        jsonld.decode
          .toNode(Parse.parse(response.getContentString()).right.get)
          .runToFuture(monix.execution.Scheduler.global)
          .map { node =>
            Collection.wrap(node).item shouldBe List(2)
          }
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
