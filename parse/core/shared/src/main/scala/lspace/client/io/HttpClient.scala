package lspace.client.io

import java.util.concurrent.ConcurrentHashMap
import monix.eval.Task
import sttp.capabilities.WebSockets
import sttp.capabilities.monix.MonixStreams
import sttp.client3._
import sttp.client3.SttpBackend
import sttp.model.Header

import scala.collection.concurrent
import scala.jdk.CollectionConverters._
import scala.concurrent.duration._

trait HttpClient {
  type WS[_]
  def backend: Task[SttpBackend[Task, MonixStreams with WebSockets]]

  protected lazy val wip: concurrent.Map[String, Task[String]] =
    new ConcurrentHashMap[String, Task[String]](16, 0.9f, 32).asScala

  private def getWithAccept(iri: String, accept: String): Task[String] =
    wip.getOrElseUpdate(
      iri + accept,
      backend.flatMap { implicit backend =>
        Task.defer {

          //        scribe.trace(s"HTTP-GET: $iri")
          basicRequest
            .get(uri"$iri")
            .headers(Header.accept(accept))
            .send()
            .flatMap { response =>
              response.body match {
                case Right(r) =>
                  if (response.contentType.exists(_.contains(accept))) Task.now(r)
                  else Task.raiseError(new Exception(s"Unexpected content-type ${response.contentType}"))
                case Left(l) => Task.raiseError(new Exception(s"Error-code $l: could not get resource $iri"))
              }
            }
            .doOnFinish {
              case None =>
                Task
                  .delay(wip.remove(iri + accept))
                  .delayExecution(30.seconds)
                  .startAndForget // this should prevent fetching the same resource(-representation) multiple times within a small period of time
              case Some(e) => Task(wip.remove(iri + accept))
            }
        }
      }.memoizeOnSuccess
    )

  object application {
    object ldjson {
      def get(iri: String): Task[String] = getWithAccept(iri, "application/ld+json")
    }

    object json {
      def get(iri: String): Task[String] = getWithAccept(iri, "application/json")
    }

    object pdf {
      def get(iri: String): Task[String] = getWithAccept(iri, "application/pdf")
    }

    object graphql {
      def get(iri: String): Task[String] = getWithAccept(iri, "application/graphql")
    }
  }

  object text {
    object plain {
      def get(iri: String): Task[String] = getWithAccept(iri, "text/plain")
    }

    object csv {
      def get(iri: String): Task[String] = getWithAccept(iri, "text/csv")
    }

    object html {
      def get(iri: String): Task[String] = getWithAccept(iri, "text/html")
    }
  }
  object image {
    object png {
      def get(iri: String): Task[String] = getWithAccept(iri, "image/png")
    }

    object jpeg {
      def get(iri: String): Task[String] = getWithAccept(iri, "image/jpeg")
    }

    object gif {
      def get(iri: String): Task[String] = getWithAccept(iri, "image/gif")
    }
  }
}
