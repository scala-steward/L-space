package lspace.services.rest.endpoints

import java.time.Instant

import cats.effect.IO
import io.finch.{Endpoint, Ok}
import lspace._
import lspace.codec.json.jsonld.JsonLDDecoder
import lspace.codec.{ActiveContext, ContextedT}
import lspace.librarian.task.AsyncGuide
import lspace.librarian.traversal.Collection
import monix.eval.Task
import monix.execution.Scheduler
import shapeless.HList

object LibrarianApi {
  def apply[JSON](graph: Graph)(implicit activeContext: ActiveContext = ActiveContext(),
                                decoder: JsonLDDecoder[JSON],
                                guide: AsyncGuide,
                                scheduler: Scheduler): LibrarianApi[JSON] = new LibrarianApi(graph)
}
class LibrarianApi[JSON](graph: Graph)(implicit val activeContext: ActiveContext,
                                       decoder: JsonLDDecoder[JSON],
                                       guide: AsyncGuide,
                                       scheduler: Scheduler)
    extends Api {

  import lspace.services.codecs.Decode._
  import lspace.decode.DecodeJsonLD.jsonldToTraversal

  def stream: Endpoint[IO, _root_.fs2.Stream[IO, ContextedT[Collection[Any, ClassType[Any]]]]] = {
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
                ContextedT(collection)
              }
              .toReactivePublisher
              .toStream[IO]()
          }
          .map(Ok(_))
          .to[IO]
    }
  }

  /**
    * GET /
    * BODY ld+json: https://ns.l-space.eu/librarian/Traversal
    */
  def list: Endpoint[IO, ContextedT[Collection[Any, ClassType[Any]]]] = {
    post(
      path("@graph") :: body[Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]],
                             lspace.services.codecs.Application.JsonLD]) {
      traversalTask: Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]] =>
        traversalTask
          .flatMap { traversal =>
            val start = Instant.now()
            traversal.untyped
              .withGraph(graph)
              .toListF
              .map { values =>
                val collection: Collection[Any, ClassType[Any]] = Collection(start, Instant.now(), values.toList)
                ContextedT(collection)
              }
              .map(Ok)
          }
          .to[IO]
    }
  }

  object filtered {

    def stream(ontology: Ontology): Endpoint[IO, _root_.fs2.Stream[IO, ContextedT[List[Node]]]] = {
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
              traversal.untyped
                .withGraph(graph)
                .apply()
                .collect { case node: Node if node.hasLabel(ontology).isDefined => node }
                .bufferTumbling(100)
                .map(_.toList)
                .map(ContextedT(_))
                .toReactivePublisher
                .toStream[IO]()
            }
            .map(Ok(_))
            .to[IO]
      }
    }

    /**
      * GET /
      * BODY ld+json: https://ns.l-space.eu/librarian/Traversal
      */
    def list(ontology: Ontology): Endpoint[IO, ContextedT[List[Node]]] = {
      post(
        "@graph" :: body[Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]],
                         lspace.services.codecs.Application.JsonLD]) {
        traversalTask: Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]] =>
          traversalTask
            .flatMap { traversal =>
              traversal.untyped
                .withGraph(graph)
                .toListF
                .map(_.collect { case node: Node if node.hasLabel(ontology).isDefined => node })
                .map(_.toList)
                .map(ContextedT(_))
                .map(Ok)
            }
            .to[IO]
      }
    }
  }
}
