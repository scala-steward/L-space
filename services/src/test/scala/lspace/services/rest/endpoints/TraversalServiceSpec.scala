package lspace.services.rest.endpoints

import io.finch.Input
import lspace.librarian.process.traversal.P
import lspace.librarian.provider.mem.{MemGraph, MemGraphDefault}
import lspace.librarian.util.SampleGraph
import lspace.server.util
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

class TraversalServiceSpec extends WordSpec with Matchers with BeforeAndAfterAll {

  implicit val graph    = MemGraph("GraphServiceSpec")
  lazy val graphService = TraversalService(graph)

  override def beforeAll(): Unit = {
    SampleGraph.loadSocial(graph)
  }

  override def afterAll(): Unit = {
    graph.close()
  }

  import util._
  "a traversal-service" should {
    "execute a traversal only on a POST request" in {
      val traversal = MemGraphDefault.g.N.has(SampleGraph.properties.balance, P.gt(300)).count
      import JsonLDModule.Encode._
      import lspace.encode.EncodeJsonLD._
      val input = Input
        .post("/traverse")
        .withBody[JsonLDModule.JsonLD](traversal.toNode)
        .withHeaders("Accept" -> "text/plain")
      graphService.traverse(input).awaitOutput().map { output =>
        output.isRight shouldBe true
        val collection = output.right.get.value
        collection.item shouldBe List(2)
      }
    }
    "get all labels" ignore {
      val input = Input
        .get("/label")
      val res = graphService.getLabels(input).awaitOutput().map { output =>
        if (output.isLeft) println(output.left.get.getMessage)
        if (output.isLeft) println(output.left.get.printStackTrace())
        output.isRight shouldBe true
        val collection = output.right.get.value
        println(collection.item.size)
        collection.item.nonEmpty shouldBe true
      }
    }
  }
}
