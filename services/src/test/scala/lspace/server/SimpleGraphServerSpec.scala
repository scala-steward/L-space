package lspace.server

import com.twitter.finagle.http.Status
import com.twitter.util

import scala.concurrent.duration._
import io.finch.Input
import io.finch.argonaut.preserveOrder._
import lspace.app.server.SimpleGraphServer
import lspace.client.provider.remote.RemoteGraph
import lspace.librarian.process.traversal.P
import lspace.librarian.provider.mem.{MemGraph, MemGraphDefault}
import lspace.librarian.structure.Graph
import lspace.librarian.util.SampleGraph
import lspace.parse.json.JsonLD
import org.scalatest._
import org.scalatest.Matchers
//import play.api.libs.json._

class SimpleGraphServerSpec extends WordSpec with Matchers with BeforeAndAfterAll {

  val graph          = MemGraph("SimpleGraphServerSpec")
  val jsonld         = JsonLD(graph)
  private val _graph = graph
  val server = new SimpleGraphServer {
    lazy val graph: Graph = _graph

    lazy val port: Int = 9989
  }
  //  val remoteGraph = RemoteGraph("SimpleGraphServerSpec")(server)

  override def beforeAll(): Unit = {
    SampleGraph.loadSocial(graph)
  }

  override def afterAll(): Unit = {
    server.server.close(util.Duration.fromSeconds(5))
  }

  import shapeless.Witness
  type Jsonld = Witness.`"application/ld+json"`.T

  "a graph-server" should {
    "execute a traversal only on a POST request" in {
      val traversal = MemGraphDefault.g.N.has(SampleGraph.properties.balance, P.gt(300)).count
      val json      = jsonld.nodeToJsonWithContext(traversal.self)._1

      println(json)

      val input = Input
        .post("/traverse")
        .withBody[io.finch.Application.Json](json)
        .withHeaders("Content-type" -> "application/json")
      val res = server.graphService.traverse(input)
      println(server.graphService.traverse.toString)
      println(input.toString)
      println(res.toString)
      println(res.awaitOutputUnsafe(5 seconds).get.value)
      res.awaitOutputUnsafe(5 seconds).map(_.status) shouldBe Some(Status.Ok)
    }
  }
  //  def request(r: DefaultHttpRequest): Request = new Request {
  //    val httpRequest = r
  //    lazy val remoteSocketAddress = new java.net.InetSocketAddress(0)
  //  }
  //
  //  def GET(path: String): Request =
  //    request(new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, path))
  //
  //  val context = s"/${config.getString("app.context")}"
  //
  //  "API" should "allow GET on status route" in {
  //    await(GET(context)).status shouldBe Status.Ok
  //  }
  //
  //  it should "return NotFound status for unknown route: /foo/bar" in {
  //    await(GET("/foo/bar")).status shouldBe Status.NotFound
  //  }
}
