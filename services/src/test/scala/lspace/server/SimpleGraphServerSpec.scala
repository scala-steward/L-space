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
import lspace.structure.{ClassType, Graph, Node}
import lspace.services.SimpleGraphServer
import lspace.services.rest.endpoints.{NameSpaceService, TraversalService}
import lspace.util.SampleGraph
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest._
import org.scalatest.Matchers
import shapeless.{:+:, CNil}

import scala.concurrent.Future
import scala.concurrent.duration._

class SimpleGraphServerSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {

  import lspace.Implicits.Scheduler.global

  import lspace.codec.argonaut._
  implicit val graph   = MemGraph("SimpleGraphServerSpec")
  implicit val encoder = lspace.codec.Encoder(nativeEncoder)
  implicit val decoder = Decoder(DetachedGraph)(nativeDecoder)

  import lspace.services.util._
  import lspace.encode.EncodeJsonLD._

  val server: SimpleGraphServer = SimpleGraphServer(graph)

//  override def beforeAll(): Unit = {
//    SampleGraph.loadSocial(graph)
//  }

  scala.concurrent.Await.ready((for {
    _ <- SampleGraph.loadSocial(graph)
  } yield ()).runToFuture, 15.seconds)

  override def afterAll(): Unit = {
    (for {
      _ <- Task.fromFuture(server.service.close())
      _ <- Task.fromFuture(graph.close())
    } yield (println("afterAll done"))).timeout(5.seconds).runToFuture
  }

  import lspace.services.util._
  "a graph-server" should {
    "execute a traversal only on a POST request" in {

      import lspace.encode.EncodeJson._
      import lspace.encode.EncodeJsonLD._
      import lspace.services.codecs
      import lspace.services.codecs.Encode._

      val traversal = g.N.has(SampleGraph.properties.balance, P.gt(300)).count
      (for {
        node <- traversal.toNode
        input = Input
          .post("/traverse")
          .withBody[lspace.services.codecs.Application.JsonLD](node)
          .withHeaders("Accept" -> "application/ld+json")
        response <- Task.fromFuture(server.service(input.request))
        _ <- {
          val headers = response.headerMap
          response.status shouldBe Status.Ok
          response.contentType shouldBe Some("application/ld+json")
          decoder
            .parse(response.getContentString())
            .flatMap { json =>
              decoder
                .toNode(json)
                .map { node =>
                  Collection[Any, ClassType[Any]](
                    Nil,
                    Nil,
                    node.out(Label.P.`@graph`).head.asInstanceOf[List[Any]],
                    Some(ClassType.stubAny)
                  ).item shouldBe List(2)
                }
            }
        }
      } yield succeed).timeout(5.seconds).runToFuture

    }
    "get all labels" in {
      val input = Input
        .get("/label")
      val res = server.service(input.request)

      Task
        .fromFuture(res.map { response =>
          response.status shouldBe Status.Ok
        })
        .timeout(5.seconds)
        .runToFuture
    }
  }
}
