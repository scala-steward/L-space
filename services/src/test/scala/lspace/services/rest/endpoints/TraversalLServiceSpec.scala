package lspace.services.rest.endpoints

import java.time.Instant
import java.util.concurrent.TimeUnit

import cats.effect.Effect
import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.finagle.param.Stats
import com.twitter.server.TwitterServer
import com.twitter.util.{Await, Awaitable}
import io.finch._
import lspace._
import lspace.librarian.traversal.Collection
import lspace.services.codecs.Application
import lspace.provider.mem.MemGraph
import lspace.provider.remote.RemoteGraph
import lspace.services.util
import lspace.structure.ClassType
import lspace.util.SampleGraph
import monix.eval.Task
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, FutureOutcome, Matchers}
import shapeless.{:+:, CNil}

import scala.concurrent.duration._

class TraversalLServiceSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {

  import lspace.Implicits.Scheduler.global
  import lspace.Implicits.AsyncGuide.guide
//  override def executionContext = lspace.Implicits.Scheduler.global

  implicit val graph    = MemGraph("GraphServiceSpec")
  implicit val nencoder = lspace.codec.argonaut.NativeTypeEncoder
  implicit val encoder  = lspace.codec.Encoder(nencoder)
  implicit val ndecoder = lspace.codec.argonaut.NativeTypeDecoder
  lazy val graphService = TraversalService(graph)

  import lspace.services.codecs
  import lspace.services.codecs.Encode._
  import lspace.encode.EncodeJson._
  import lspace.encode.EncodeJsonLD._

  lazy val service: Service[Request, Response] = Bootstrap
    .configure(enableMethodNotAllowed = true, enableUnsupportedMediaType = true)
    .serve[lspace.services.codecs.Application.JsonLD :+: CNil](graphService.api)
    .toService

  object Main extends TwitterServer {

    def main(): Unit = {
      val server = Http.server
//        .configured(Stats(statsReceiver))
        .serve(":8082", service)

      onExit { server.close() }

//      Await.ready(adminHttpServer)
    }
  }
  Main.main()
//  Main.ready(com.twitter.util.Duration(5, TimeUnit.SECONDS))
//  Await.ready(Main, com.twitter.util.Duration(5, TimeUnit.SECONDS))

//  override def beforeAll(): Unit = {
//    SampleGraph.loadSocial(graph)
//  }

  val initTask = (for {
    _ <- SampleGraph.loadSocial(graph)
  } yield ()).memoizeOnSuccess

  import util._
  override def afterAll(): Unit = {
    (for {
      _ <- graph.close()
      _ <- Task.fromFuture(Main.close())
    } yield ()).timeout(5.seconds).runToFuture
  }

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    new FutureOutcome(initTask.runToFuture flatMap { result =>
      super.withFixture(test).toFuture
    })
  }

  import util._
  "a traversal-service" should {
    "execute a traversal only on a POST request" ignore {
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
          .withHeaders("Accept" -> "application/ld+json")
        _ <- {
          println(input)
          Task
            .fromIO(
              graphService
                .traverse(input)
                .output
                .get)
            .map { output =>
              //          if (output.isLeft) println(output.left.get.getMessage)
              output.status shouldBe Status.Ok
              val collection = output.value
              //            collection.item shouldBe List(2)
              println(output.value.head)
            }
        }
      } yield succeed).runToFuture
    }
    "be usable with RemoteGraph" in {
      val graph = RemoteGraph.apply("abc", "http://localhost", 8082, "traverse")
      (for {
        count <- graph.*>(lspace.g.N.has(SampleGraph.properties.balance, P.gt(500)).count).headF
      } yield count shouldBe 2l).runToFuture
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
