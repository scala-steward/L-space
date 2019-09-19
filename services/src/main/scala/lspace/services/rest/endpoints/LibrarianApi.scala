package lspace.services.rest.endpoints

import java.time.Instant

import cats.effect.IO
import io.finch.{Application, Bootstrap, Endpoint, Ok}
import lspace._
import lspace.codec.json.{JsonDecoder, JsonEncoder}
import lspace.codec.json.jsonld
import lspace.codec.json.jsonld.JsonLDEncoder
import lspace.codec.{ActiveContext, ContextedT}
import lspace.librarian.traversal.Collection
import lspace.provider.detached.DetachedGraph
import lspace.services.LApplication
import monix.eval.Task
import shapeless.{:+:, CNil, HList}
import scribe._

object LibrarianApi {
  def apply[Json](graph0: Graph, activeContext0: ActiveContext = ActiveContext())(
      implicit baseDecoder0: JsonDecoder[Json],
      baseEncoder0: JsonEncoder[Json]): LibrarianApi[Json] =
    new LibrarianApi[Json] {
      val graph: Graph                                     = graph0
      implicit val activeContext                           = activeContext0
      implicit override def baseDecoder: JsonDecoder[Json] = baseDecoder0
      implicit override def baseEncoder: JsonEncoder[Json] = baseEncoder0
    }
}

trait LibrarianApi[JSON] extends ExecutionApi {
  def graph: Graph

  implicit def baseDecoder: JsonDecoder[JSON]
  implicit def baseEncoder: JsonEncoder[JSON]
  implicit def activeContext: ActiveContext

  import lspace.services.codecs.Decode._
  import lspace.decode.DecodeJsonLD.jsonldToTraversal
  implicit lazy val decoder = lspace.codec.json.jsonld.Decoder(DetachedGraph)
  import Implicits.AsyncGuide.guide
  import Implicits.Scheduler.global

  def query: Endpoint[IO, _root_.fs2.Stream[IO, ContextedT[Collection[Any, ClassType[Any]]]]] = {
    import io.finch.internal.HttpContent
    import cats.effect._, _root_.fs2._
    import io.finch.fs2._
    import _root_.fs2.interop.reactivestreams._
    import scala.concurrent.ExecutionContext
    implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    post(
      "@graph" :: body[Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]],
                       lspace.services.codecs.Application.JsonLD]).mapOutputAsync {
      traversalTask: Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]] =>
        traversalTask
          .map { traversal =>
            //            println(s"executing ${traversal.prettyPrint}")
            val start = Instant.now()
            traversal.untyped
              .withGraph(graph)
              .apply()
              .bufferTumbling(100)
              .map { values =>
                val collection: Collection[Any, ClassType[Any]] = Collection(start, Instant.now(), values.toList)
                collection.logger.debug("result count: " + values.size.toString)
                ContextedT(collection)
              }
              .toReactivePublisher
              .toStream[IO]()
          }
          .map(Ok(_))
          .to[IO]
    }
  }
//  def query2: Endpoint[IO, ContextedT[Collection[Any, ClassType[Any]]]] = {
//    post(
//      "@graph" :: body[Task[Traversal[ClassType[Any], ClassType[Any], HList]],
//                       lspace.services.codecs.Application.JsonLD]).mapOutputAsync {
//      traversalTask: Task[Traversal[ClassType[Any], ClassType[Any], HList]] =>
//        traversalTask.flatMap { traversal =>
//          val start = Instant.now()
//          traversal.untyped
//            .withGraph(graph)
//            .toListF
//            .map { values =>
//              val collection: Collection[Any, ClassType[Any]] = Collection(start, Instant.now(), values.toList)
//              collection.logger.debug("result count: " + values.size.toString)
//              Ok(ContextedT(collection))
//            }
//        }.toIO
//    }
//  }
  def mutate: Endpoint[IO, Unit] = ???
  def ask: Endpoint[IO, Boolean] = {
    post(
      "@graph" :: body[Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]],
                       lspace.services.codecs.Application.JsonLD]).mapOutputAsync {
      traversalTask: Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]] =>
        traversalTask
          .flatMap { traversal =>
            val start = Instant.now()
            traversal.untyped
              .withGraph(graph)
              .headOptionF
              .map(_.isDefined)
              .map(Ok)
          }
          .to[IO]
    }
  }
  def subscribe: Endpoint[IO, List[Node]] = ???

  import lspace.services.codecs.Encode._
  import lspace.encode.EncodeJsonLD._

  implicit lazy val encoder: JsonLDEncoder[JSON] = jsonld.Encoder(baseEncoder)

  def raw = query

  def compiled: Endpoint.Compiled[IO] =
    Bootstrap
      .configure(enableMethodNotAllowed = true, enableUnsupportedMediaType = true)
      .serve[LApplication.JsonLD](query)
      .compile
}
