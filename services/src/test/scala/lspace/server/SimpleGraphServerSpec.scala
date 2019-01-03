package lspace.server

import argonaut.Parse
import com.twitter.finagle.http.Status
import com.twitter.util.Await
import com.twitter.util.Duration
import io.finch.{Input, Output}
import lspace.app.server.SimpleGraphServer
import lspace.client.provider.remote.RemoteGraph
import lspace.librarian.process.traversal.{Collection, P}
import lspace.librarian.provider.mem.{MemGraph, MemGraphDefault}
import lspace.librarian.structure.{Graph, Node}
import lspace.librarian.util.SampleGraph
import lspace.parse.json.JsonLD
import lspace.services.rest.endpoints.JsonLDModule
import org.scalatest._
import org.scalatest.Matchers
//import play.api.libs.json._

class SimpleGraphServerSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {

  val graph          = MemGraph("SimpleGraphServerSpec")
  val jsonld         = JsonLD(graph)
  private val _graph = graph
  val server = new SimpleGraphServer {
    lazy val graph: Graph = _graph
    lazy val context      = "a"

    lazy val port: Int = 9989
  }
  //  val remoteGraph = RemoteGraph("SimpleGraphServerSpec")(server)
  println(server.graphService.printRoutes)

  override def beforeAll(): Unit = {
    SampleGraph.loadSocial(graph)
  }

  override def afterAll(): Unit = {
    Await.result(server.close(), Duration.fromSeconds(2))
  }

  import shapeless.Witness
  type Jsonld = Witness.`"application/ld+json"`.T

  import com.twitter.util.{Future => TwFuture}
  import scala.concurrent.{Future => ScFuture, Promise => ScPromise}
  implicit def twFutureToScala[T](twFuture: TwFuture[T]): ScFuture[T] = {
    val prom = ScPromise[T]()
    twFuture.onSuccess { res: T =>
      prom.success(res)
    }
    twFuture.onFailure { t: Throwable =>
      prom.failure(t)
    }
    prom.future
  }

  "a graph-server" should {
    "execute a traversal only on a POST request" in {
      val traversal = MemGraphDefault.g.N.has(SampleGraph.properties.balance, P.gt(300)).count
      import JsonLDModule.Encode._
      val input = Input
        .post("/a/v1/traverse")
        .withBody[JsonLDModule.JsonLD](traversal.toNode)
      val res = server.api(input.request)

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
        .get("/a/v1/label")
      val res = server.api(input.request)

      res.map { response =>
        response.status shouldBe Status.Ok
      }
    }
  }
}
