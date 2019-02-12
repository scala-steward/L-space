package lspace.services.rest.endpoints

import io.finch.Input
import lspace.services.codecs.Application
import lspace.librarian.process.traversal.P
import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.util.SampleGraph
import lspace.services.util
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

class TraversalLServiceSpec extends WordSpec with Matchers with BeforeAndAfterAll {

  implicit val graph    = MemGraph("GraphServiceSpec")
  implicit val nencoder = lspace.codec.argonaut.NativeTypeEncoder
  implicit val encoder  = lspace.codec.Encoder(nencoder)
  implicit val ndecoder = lspace.codec.argonaut.NativeTypeDecoder
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
      val traversal = graph.g.N.has(SampleGraph.properties.balance, P.gt(300)).count
      import lspace.services.codecs
      import lspace.services.codecs.Encode._
      import lspace.encode.EncodeJsonLD._
      val input = Input
        .post("/traverse")
        .withBody[Application.JsonLD](traversal.toNode)
        .withHeaders("Accept" -> "text/plain")
      graphService.traverse(input).awaitOutput().map { output =>
        if (output.isLeft) println(output.left.get.getMessage)
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
