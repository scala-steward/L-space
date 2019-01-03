package lspace.app.server

import argonaut._
import com.twitter.concurrent.AsyncStream
import com.twitter.finagle.Service
import com.twitter.finagle.Http
import com.twitter.finagle.http.filter.Cors
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.param.Stats
import com.twitter.server.TwitterServer
import com.twitter.util.{Await, Promise}
import io.finch.Bootstrap
import io.finch.sse.ServerSentEvent
import lspace.services.rest.endpoints.GraphService
import lspace.services.rest.security.WithSse
import shapeless._
import io.finch._
import lspace.services.rest.endpoints.JsonLDModule
import lspace.librarian.structure.Graph

trait SimpleGraphServer extends TwitterServer {
  def context: String
  def graph: Graph
  def port: Int

  lazy val graphService = GraphService(context, graph)

  val policy: Cors.Policy = Cors.Policy(allowsOrigin = _ => Some("*"),
                                        allowsMethods = _ => Some(Seq("GET", "POST")),
                                        allowsHeaders = _ => Some(Seq("Accept")))

  import JsonLDModule.Encode._
  lazy val api: Service[Request, Response] = Bootstrap
    .configure(enableMethodNotAllowed = true, enableUnsupportedMediaType = true)
    .serve[JsonLDModule.JsonLD :+: CNil](graphService.api)
    .toService
//  lazy val corsApi = new Cors.HttpFilter(policy).andThen(api)

  def main(): Unit = {
    val server = Http.server
      .configured(Stats(statsReceiver))
      .serve(
        s":$port",
        api
      )

    onExit {
      println(s"close graph-server for ${graphService.graph.iri}")
      server.close()
    }

    Await.ready(adminHttpServer)
  }

  def someStream(session: WithSse): AsyncStream[ServerSentEvent[Json]] = {
    val p = Promise[ServerSentEvent[Json]]()
    session.sse = Some(p)
    AsyncStream.fromFuture(p.map { e =>
      session.sse = None
      e
    }) ++ someStream(session)
  }
}
