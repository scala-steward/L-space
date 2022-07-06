package lspace.services.rest.endpoints

import java.nio.charset.StandardCharsets
import java.time.Instant

import cats.effect.IO
import lspace._
import lspace.codec.json.jsonld.JsonLDDecoder
import lspace.codec.{ActiveContext, ContextedT}
import lspace.decode.DecodeJsonLD
import lspace.encode.EncodeJsonLD
import lspace.librarian.task.AsyncGuide
import lspace.librarian.traversal.Collection
import lspace.services.codecs.Application
import lspace.services.codecs.Application.{GraphQLCodec, JsonLDCodec}
import monix.eval.Task
import monix.execution.Scheduler
import shapeless.HList
import sttp.client._
import sttp.model.MediaType
import sttp.tapir.Codec.fromDecodeAndMeta
import sttp.tapir.CodecFormat.TextPlain
import sttp.tapir._

object LibrarianApi {
  def apply[JSON](graph: Graph)(implicit
    activeContext: ActiveContext = ActiveContext(),
    decoder: JsonLDDecoder[JSON],
    guide: AsyncGuide,
    scheduler: Scheduler
  ): LibrarianApi[JSON] = new LibrarianApi(graph)
}
class LibrarianApi[JSON](graph: Graph)(implicit
  val activeContext: ActiveContext,
  decoder: JsonLDDecoder[JSON],
  guide: AsyncGuide,
  scheduler: Scheduler
) extends Api {

  import lspace.services.codecs.Decode._
  import lspace.decode.DecodeJsonLD.jsonldToTraversal

  lspace.client.io.HttpClientAsyncHttp.backend
  implicit val jsonLDCodec: JsonLDCodec[String] =
    Codec.id[String, Application.JsonLD](Application.JsonLD(), Some(Schema(SchemaType.SString)))
  implicit val graphQLCodec: GraphQLCodec[String] =
    Codec.id[String, Application.GraphQL](Application.GraphQL(), Some(Schema(SchemaType.SString)))

  def traverse =
    endpoint
      .errorOut(stringBody)
      .in(
        anyFromUtf8StringBody(implicitly[Codec[String, String, Application.JsonLD]])
          .description("The Librarian traversal to execute")
      )
      .in(
        anyFromUtf8StringBody(implicitly[Codec[String, String, Application.GraphQL]])
          .description("The GraphQL traversal to execute")
      )
      .out(stringBody)
      .out(stringBody)
  traverse

  def openapiYamlDocumentation: String = {
    import sttp.tapir.docs.openapi._
    import sttp.tapir.openapi.circe.yaml._

    // interpreting the endpoint description to generate yaml openapi documentation
    val docs = List(traverse).toOpenAPI("The Tapir Library", "1.0")
    docs.toYaml
  }

  def stream: Endpoint[IO, _root_.fs2.Stream[IO, ContextedT[Collection[Any, ClassType[Any]]]]] = {
    import io.finch.internal.HttpContent
    import cats.effect._, _root_.fs2._
    import io.finch.fs2._
    import _root_.fs2.interop.reactivestreams._
    import scala.concurrent.ExecutionContext
    implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    post(
      "@graph" :: body[Task[
        Traversal[ClassType[Any], ClassType[Any], _ <: HList]
      ], lspace.services.codecs.Application.JsonLD]
    ).mapOutputAsync { traversalTask: Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]] =>
      traversalTask
        .map { traversal =>
          //            println(s"executing ${traversal.prettyPrint}")
          val start = Instant.now()
          traversal.untyped
            .withGraph(graph)
            .apply()
            .bufferTumbling(100)
            .map { values =>
              val collection: Collection[Any, ClassType[Any]] =
                Collection(start, Instant.now(), values.toList) // , Some(traversal.et))
              ContextedT(collection)
            }
            .toReactivePublisher
            .toStream[IO]()
        }
        .map(Ok(_))
        .to[IO]
    }
  }

  /** GET / BODY ld+json: https://ns.l-space.eu/librarian/Traversal
    */
  def list: Endpoint[IO, ContextedT[Collection[Any, ClassType[Any]]]] =
    post(
      path("@graph") :: body[Task[
        Traversal[ClassType[Any], ClassType[Any], _ <: HList]
      ], lspace.services.codecs.Application.JsonLD]
    ) { traversalTask: Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]] =>
      traversalTask
        .flatMap { traversal =>
          val start = Instant.now()
          traversal.untyped
            .withGraph(graph)
            .toListF
            .map { values =>
              val collection: Collection[Any, ClassType[Any]] =
                Collection(start, Instant.now(), values.toList) // , Some(traversal.et))
              ContextedT(collection)
            }
            .map(Ok)
        }
        .to[IO]
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
        "@graph" :: body[Task[
          Traversal[ClassType[Any], ClassType[Any], _ <: HList]
        ], lspace.services.codecs.Application.JsonLD]
      ).mapOutputAsync { traversalTask: Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]] =>
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

    /** GET / BODY ld+json: https://ns.l-space.eu/librarian/Traversal
      */
    def list(ontology: Ontology): Endpoint[IO, ContextedT[List[Node]]] =
      post(
        "@graph" :: body[Task[
          Traversal[ClassType[Any], ClassType[Any], _ <: HList]
        ], lspace.services.codecs.Application.JsonLD]
      ) { traversalTask: Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]] =>
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
