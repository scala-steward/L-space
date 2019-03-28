package lspace.services.rest.endpoints

import com.twitter.finagle.http.Status
import io.finch.Input
import lspace._
import lspace.services.codecs.Application
import lspace.provider.mem.MemGraph
import lspace.services.util
import lspace.util.SampleGraph
import monix.eval.Task
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, Matchers}

import scala.concurrent.duration._

class TraversalLServiceSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {

  import lspace.Implicits.Scheduler.global

  implicit val graph    = MemGraph("GraphServiceSpec")
  implicit val nencoder = lspace.codec.argonaut.NativeTypeEncoder
  implicit val encoder  = lspace.codec.Encoder(nencoder)
  implicit val ndecoder = lspace.codec.argonaut.NativeTypeDecoder
  lazy val graphService = TraversalService(graph)

//  override def beforeAll(): Unit = {
//    SampleGraph.loadSocial(graph)
//  }
  scala.concurrent.Await.ready((for {
    _ <- SampleGraph.loadSocial(graph)
  } yield ()).runToFuture, 5.seconds)

  override def afterAll(): Unit = {
    graph.close()
  }

  import util._
  "a traversal-service" should {
    "execute a traversal only on a POST request" in {
      import lspace.services.codecs
      import lspace.services.codecs.Encode._
      import lspace.encode.EncodeJsonLD._

      val traversal = lspace.g.N.has(SampleGraph.properties.balance, P.gt(300)).count
      (for {
        node <- traversal.toNode
        json = encoder.apply(node)
        input = Input
          .post("/traverse")
          .withBody[Application.JsonLD](node)
          .withHeaders("Accept" -> "text/plain")
        _ <- Task
          .fromIO(
            graphService
              .traverse(input)
              .output
              .get)
          .map { output =>
//          if (output.isLeft) println(output.left.get.getMessage)
            output.status shouldBe Status.Ok
            val collection = output.value
            collection.item shouldBe List(2)
          }
      } yield succeed).runToFuture
    }
    "get all labels" ignore {
      Task {
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
        res.get
      }.runToFuture
    }
  }
}
