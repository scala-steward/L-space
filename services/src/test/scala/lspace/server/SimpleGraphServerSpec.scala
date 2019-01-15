package lspace.server

import argonaut.Parse
import com.twitter.finagle
import com.twitter.finagle.http.{Request, Response, Status}
import io.finch.{Bootstrap, Input, Output}
import lspace.app.server.SimpleGraphServer
import lspace.librarian.process.traversal.{Collection, P}
import lspace.librarian.provider.mem.{MemGraph, MemGraphDefault}
import lspace.librarian.structure.{Graph, Node}
import lspace.librarian.util.SampleGraph
import lspace.parse.json.JsonLD
import lspace.services.rest.endpoints.{JsonLDModule, NameSpaceService, TraversalService}
import org.scalatest._
import org.scalatest.Matchers
import shapeless.{:+:, CNil}
//import play.api.libs.json._

class SimpleGraphServerSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {

  val graph  = MemGraph("SimpleGraphServerSpec")
  val jsonld = JsonLD(graph)

  val server = new SimpleGraphServer(graph)

  //  val remoteGraph = RemoteGraph("SimpleGraphServerSpec")(server)

  override def beforeAll(): Unit = {
    SampleGraph.loadSocial(graph)
  }

  override def afterAll(): Unit = {
    server.service.close()
    graph.close()
  }

  import util._
  "a graph-server" should {
    "execute a traversal only on a POST request" in {

      val traversal = MemGraphDefault.g.N.has(SampleGraph.properties.balance, P.gt(300)).count
      import JsonLDModule.Encode._
      val input = Input
        .post("/traverse")
        .withBody[JsonLDModule.JsonLD](traversal.toNode)
        .withHeaders("Accept" -> "application/ld+json")
      val res = server.service(input.request)

      res.map { response =>
        val headers = response.headerMap

        response.status shouldBe Status.Ok
        response.contentType shouldBe Some("application/ld+json")
        jsonld
          .resource(Parse.parse(response.getContentString()).right.get.obj.get)
          .toOption
          .map(_.asInstanceOf[Node])
          .map(Collection.wrap)
          .map(_.item) shouldBe Some(List(2))
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
