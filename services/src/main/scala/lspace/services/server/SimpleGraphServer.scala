package lspace.app.server

import argonaut.Json
import com.twitter.concurrent.AsyncStream
import com.twitter.finagle.Service
import com.twitter.finagle.Http
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.param.Stats
import com.twitter.util.{Future, Promise}
import io.finch.sse.ServerSentEvent
import lspace.client.Client
import lspace.services.rest.endpoints.GraphService
import lspace.services.rest.security.WithSse
//import com.twitter.server.TwitterServer
import com.twitter.util.Await
import io.finch.argonaut.preserveOrder._
import lspace.librarian.structure.Graph

trait SimpleGraphServer extends App {
  def context: String
  def graph: Graph
  def port: Int

  lazy val graphService                    = GraphService(context, graph)
  lazy val api: Service[Request, Response] = graphService.api.toService

  //  def main(): Unit = {
  lazy val server = Http.server
  //      .configured(Stats(statsReceiver))
    .serve(s":$port", api)

  //    onExit { server.close() }

  //  Await.ready(server)
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
