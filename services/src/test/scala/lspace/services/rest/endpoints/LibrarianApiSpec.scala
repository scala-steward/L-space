package lspace.services.rest.endpoints

import java.time.Instant
import java.util.concurrent.TimeUnit

import cats.effect.Effect
import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.finagle.param.Stats
import com.twitter.util.{Await, Awaitable}
import io.finch._
import lspace._
import lspace.codec.ActiveContext
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

class LibrarianApiSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {

  import lspace.Implicits.Scheduler.global
  import lspace.Implicits.AsyncGuide.guide
  override def executionContext = lspace.Implicits.Scheduler.global

  implicit val graph    = MemGraph("LibrarianApiSpec")
  implicit val nencoder = lspace.codec.argonaut.Encoder
  implicit val encoder  = lspace.codec.json.jsonld.JsonLDEncoder(nencoder)
  implicit val ndecoder = lspace.codec.argonaut.Decoder
  lazy val graphService = LibrarianApi(graph)

  import lspace.services.codecs
  import lspace.services.codecs.Encode._
  import lspace.encode.EncodeJson._
  import lspace.encode.EncodeJsonLD._

  lazy val service: Service[Request, Response] = Endpoint.toService(graphService.compiled)

  Http.server.serve(":8082", service)
//  Main.ready(com.twitter.util.Duration(5, TimeUnit.SECONDS))
//  Await.ready(Main, com.twitter.util.Duration(5, TimeUnit.SECONDS))

//  override def beforeAll(): Unit = {
//    SampleGraph.loadSocial(graph)
//  }

  val initTask = (for {
    _ <- SampleGraph.loadSocial(graph)
  } yield ()).memoizeOnSuccess

  import lspace.services.util._
  override def afterAll(): Unit = {
    (for {
      _ <- graph.close()
//      _ <- Task.fromFuture(Main.close())
    } yield ()).timeout(5.seconds).runToFuture
  }

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    new FutureOutcome(initTask.runToFuture flatMap { result =>
      super.withFixture(test).toFuture
    })
  }

  implicit val activeContext: ActiveContext = ActiveContext()

  import util._
  "a librarian-api" should {
    "execute a traversal only on a POST request" in {
      import lspace.services.codecs
      import lspace.services.codecs.Encode._
      import lspace.encode.EncodeJsonLD._

      val traversal = lspace.g.N.has(SampleGraph.properties.balance, P.gt(300)).count
      (for {
        node <- traversal.toNode
        json = encoder.apply(node)
//        _    = println(json)
        input = Input
          .post("/@graph")
          .withBody[Application.JsonLD](node)
          .withHeaders("Accept" -> "application/ld+json")
        _ <- {
          Task
            .from(
              graphService
                .query(input)
                .output
                .get)
            .flatMap { output =>
              //          if (output.isLeft) println(output.left.get.getMessage)
              output.status shouldBe Status.Ok
              val collection = output.value.compile.toList
              Task.from(output.value.compile.toList).map(_.head.t.item shouldBe List(2))
            }
        }
      } yield succeed).runToFuture
    }
    "be usable with RemoteGraph" in {
      val graph = RemoteGraph.apply("abc", "http://localhost", 8082, "@graph")
      (for {
        count <- graph.*>(lspace.g.N.has(SampleGraph.properties.balance, P.gt(500)).count).headF
      } yield count shouldBe 2l).runToFuture
    }
  }
}
