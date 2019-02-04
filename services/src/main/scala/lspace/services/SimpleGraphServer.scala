package lspace.services

import argonaut._
import com.twitter.concurrent.AsyncStream
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response}
import com.twitter.io.Buf
import com.twitter.util.Promise
import io.finch.{Bootstrap, _}
import io.finch.sse.ServerSentEvent
import lspace.encode.EncodeJsonLD
import lspace.librarian.structure.Graph
import lspace.services.rest.endpoints.{JsonLDModule, NameSpaceService, TraversalService}
import lspace.services.rest.security.WithSse
import shapeless._

class SimpleGraphServer(graph: Graph, port: Int = 8080) extends LService {

  lazy val graphService = TraversalService(graph)

//  val policy: Cors.Policy = Cors.Policy(allowsOrigin = _ => Some("*"),
//                                        allowsMethods = _ => Some(Seq("GET", "POST")),
//                                        allowsHeaders = _ => Some(Seq("Accept")))

  type JsonLDText[A] = io.finch.Encode.Aux[A, Text.Plain]
  private val printer = PrettyParams.nospace.copy(preserveOrder = true)
  import JsonLDModule.Encode._
  import lspace.encode.EncodeJsonLD._
  implicit val _graph = graph

  implicit def encodeArgonautText[A](implicit e: EncodeJsonLD[A]): JsonLDText[A] = {
    io.finch.Encode.instance[A, Text.Plain]((a, cs) =>
      Buf.ByteArray.Owned(printer.pretty(e.encode(a)).getBytes(cs.name)))
  }
  lazy val service: Service[Request, Response] = Bootstrap
    .configure(enableMethodNotAllowed = true, enableUnsupportedMediaType = true)
    .serve[JsonLDModule.JsonLD :+: Text.Plain :+: CNil](TraversalService(graph).api :+: NameSpaceService(graph).api)
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

  def someStream(session: WithSse): AsyncStream[ServerSentEvent[Json]] = {
    val p = Promise[ServerSentEvent[Json]]()
    session.sse = Some(p)
    AsyncStream.fromFuture(p.map { e =>
      session.sse = None
      e
    }) ++ someStream(session)
  }
}
