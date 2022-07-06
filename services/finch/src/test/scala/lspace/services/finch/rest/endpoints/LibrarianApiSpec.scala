package lspace.services.finch.rest.endpoints

import com.twitter.finagle.http.{Request, Response, Status}
import io.finch._
import lspace._
import lspace.codec.argonaut._
import lspace.codec.ActiveContext
import lspace.provider.detached.DetachedGraph
import lspace.services.codecs.{Application => LApplication}
import lspace.provider.mem.MemGraph
import lspace.util.SampleGraph
import monix.eval.Task
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, FutureOutcome, Matchers}

import scala.concurrent.duration._

class LibrarianApiSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {

  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  implicit val graph          = MemGraph("LibrarianApiSpec")
  implicit val encoderJsonLD  = lspace.codec.json.jsonld.JsonLDEncoder(nativeEncoder)
  implicit val decoderJsonLD  = lspace.codec.json.jsonld.JsonLDDecoder(DetachedGraph)(nativeDecoder)
  implicit val decoderGraphQL = codec.graphql.Decoder()
  import lspace.Implicits.AsyncGuide.guide
  implicit val activeContext: ActiveContext = ActiveContext()
  lazy val graphService                     = LibrarianApi(graph)
  import lspace.services.codecs.Encode._
  import lspace.encode.EncodeJsonLD._
  lazy val service = graphService.list.toServiceAs[LApplication.JsonLD]

//  override def beforeAll(): Unit = {
//    SampleGraph.loadSocial(graph)
//  }

  val initTask = (for {
    _ <- SampleGraph.loadSocial(graph)
  } yield ()).memoizeOnSuccess

  override def afterAll(): Unit =
    (for {
      _ <- graph.close()
//      _ <- Task.fromFuture(Main.close())
    } yield ()).timeout(5.seconds).runToFuture

  override def withFixture(test: NoArgAsyncTest): FutureOutcome =
    new FutureOutcome(initTask.runToFuture.flatMap { result =>
      super.withFixture(test).toFuture
    })

  "a librarian-api" should {
    "execute a traversal only on a POST request" in {
      import lspace.services.codecs.Encode._
      import lspace.encode.EncodeJsonLD._

      val traversal = lspace.g.N.has(SampleGraph.properties.balance, P.gt(300)).count
      (for {
        node <- traversal.toNode
        json = encoderJsonLD.apply(node)
        input = Input
          .post("/@graph")
          .withBody[LApplication.JsonLD](node)
          .withHeaders("Accept" -> "application/ld+json")
//        _ = println(input.request.contentString)
        _ <-
          Task
            .from(graphService.stream(input).output.get)
            .flatMap { output =>
              //          if (output.isLeft) println(output.left.get.getMessage)
              output.status shouldBe Status.Ok
              val collection = output.value.compile.toList
              Task.from(output.value.compile.toList).map(_.head.t.item shouldBe List(2))
            }
      } yield succeed).runToFuture
    }
    "as service" in {
      import lspace.services.codecs.Encode._
      import lspace.encode.EncodeJsonLD._
      import lspace.services.util._

      val traversal = lspace.g.N.has(SampleGraph.properties.balance, P.gt(300)).count
      (for {
        node <- traversal.toNode
        json = encoderJsonLD.apply(node)
        input = Input
          .post("/@graph")
          .withBody[LApplication.JsonLD](node)
          .withHeaders("Accept" -> "application/ld+json")
//        _ = println(input.request.contentString)
        _ <-
          Task
            .fromFuture(service(input.request))
            .map { output =>
              //          if (output.isLeft) println(output.left.get.getMessage)
              val contentString = output.contentString
//              println(contentString)
              output.status shouldBe Status.Ok
//              contentString shouldBe """"""
            }
      } yield succeed).runToFuture
    }
    "as service for a projection-result-like traversal" in {
      import lspace.services.codecs.Encode._
      import lspace.encode.EncodeJsonLD._
      import lspace.services.util._

      val traversal = lspace.g.N
        .has(SampleGraph.properties.balance, P.gt(300))
        .project(_.iri)
        .by(_.out(SampleGraph.properties.balanceDouble))
      (for {
        node <- traversal.toNode
        json = encoderJsonLD.apply(node)
        input = Input
          .post("/@graph")
          .withBody[LApplication.JsonLD](node)
          .withHeaders("Accept" -> "application/ld+json")
        //        _ = println(input.request.contentString)
        _ <-
          Task
            .fromFuture(service(input.request))
            .map { output =>
              //          if (output.isLeft) println(output.left.get.getMessage)
              val contentString = output.contentString
//              println(contentString)
              output.status shouldBe Status.Ok
              //              contentString shouldBe """"""
            }
      } yield succeed).runToFuture
    }
    "as service for a projection-result-like traversal return json" in {
      import lspace.services.codecs.Encode._
      import lspace.encode.EncodeJsonLD._
      import lspace.services.util._

      val traversal = lspace.g.N
        .has(SampleGraph.properties.balance, P.gt(300))
        .project(_.iri)
        .by(_.out(SampleGraph.properties.balanceDouble))
        .by(_.out("unknown"))
      (for {
        node <- traversal.toNode
        json = encoderJsonLD.apply(node)
        input = Input
          .post("/@graph")
          .withBody[LApplication.JsonLD](node)
          .withHeaders("Accept" -> "application/json")
        //        _ = println(input.request.contentString)
        _ <-
          Task
            .fromFuture(service(input.request))
            .map { output =>
              //          if (output.isLeft) println(output.left.get.getMessage)
              val contentString = output.contentString
//              println(contentString)
              output.status shouldBe Status.Ok
              //              contentString shouldBe """"""
            }
      } yield succeed).runToFuture
    }
  }
}
