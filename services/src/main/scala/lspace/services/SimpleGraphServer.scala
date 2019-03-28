package lspace.services

import com.twitter.concurrent.AsyncStream
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response}
import com.twitter.io.Buf
import com.twitter.util.Promise
import io.finch.{Bootstrap, ServerSentEvent}
import lspace.codec.{Encoder, NativeTypeDecoder, NativeTypeEncoder}
import lspace.encode.EncodeJsonLD
import lspace.encode.EncodeJson
import lspace.structure.Graph
import lspace.services.rest.endpoints.{NameSpaceService, TraversalService}
import lspace.services.rest.security.WithSse
import shapeless._

object SimpleGraphServer {
  def apply[Json0](graph0: Graph, port0: Int = 8080)(implicit baseDecoder0: NativeTypeDecoder.Aux[Json0],
                                                     baseEncoder0: NativeTypeEncoder.Aux[Json0]): SimpleGraphServer =
    new SimpleGraphServer {
      override val graph: Graph = graph0
      override val port: Int    = port0
      type Json = Json0
      implicit override def baseDecoder: NativeTypeDecoder.Aux[Json] = baseDecoder0
      implicit override def baseEncoder: NativeTypeEncoder.Aux[Json] = baseEncoder0
    }
}
trait SimpleGraphServer extends LService {
  def graph: Graph
  def port: Int
  type Json
  implicit def baseDecoder: NativeTypeDecoder.Aux[Json]
  implicit def baseEncoder: NativeTypeEncoder.Aux[Json]

  lazy val graphService = TraversalService(graph)

//  val policy: Cors.Policy = Cors.Policy(allowsOrigin = _ => Some("*"),
//                                        allowsMethods = _ => Some(Seq("GET", "POST")),
//                                        allowsHeaders = _ => Some(Seq("Accept")))

  import lspace.services.codecs
  import lspace.services.codecs.Encode._
  import lspace.encode.EncodeJson._
  import lspace.encode.EncodeJsonLD._

  implicit lazy val encoder: Encoder = Encoder(baseEncoder)

//  implicit val _graph = graph
//  implicit val labeledNodeToJson: EncodeJsonLD[String] =
//    EncodeJson { string: String =>
//      string
//    }

//  implicit def encodeArgonautText[A](implicit e: EncodeJsonLD[A]): JsonLDText[A] = {
//    io.finch.Encode.instance[A, Text.Plain]((a, cs) =>
//      Buf.ByteArray.Owned(printer.pretty(e.encode(a)).getBytes(cs.name)))
//  }
  lazy val service: Service[Request, Response] = Bootstrap
    .configure(enableMethodNotAllowed = true, enableUnsupportedMediaType = true)
    .serve[lspace.services.codecs.Application.JsonLD :+: CNil](
      TraversalService(graph).api :+: NameSpaceService(graph).api)
    .toService
//  lazy val corsApi = new Cors.HttpFilter(policy).andThen(api)

//  def main(): Unit = {
//    val server = Http.server
//      .configured(Stats(statsReceiver))
//      .serve(
//        s":$port",
//        service
//      )
//
//    onExit {
//      println(s"close graph-server for ${graphService.graph.iri}")
//      server.close()
//    }
//
//    Await.ready(adminHttpServer)
//  }

  def someStream(session: WithSse): AsyncStream[ServerSentEvent[argonaut.Json]] = {
    val p = Promise[ServerSentEvent[argonaut.Json]]()
    session.sse = Some(p)
    AsyncStream.fromFuture(p.map { e =>
      session.sse = None
      e
    }) ++ someStream(session)
  }
}
