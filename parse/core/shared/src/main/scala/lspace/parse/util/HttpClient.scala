package lspace.parse.util

import java.nio.ByteBuffer
import java.util.concurrent.ConcurrentHashMap

import com.softwaremill.sttp.{SttpBackend, sttp, _}
import monix.eval.Task
import monix.reactive.Observable

import scala.collection.concurrent
import scala.collection.JavaConverters._
import scala.concurrent.duration._

trait HttpClient {
  implicit def backend: SttpBackend[Task, Observable[ByteBuffer]]

  protected lazy val wip: concurrent.Map[String, Task[String]] =
    new ConcurrentHashMap[String, Task[String]](16, 0.9f, 32).asScala

  private def getWithAccept(iri: String, accept: String): Task[String] = {
    wip.getOrElseUpdate(
      iri + accept,
      Task.defer {
        scribe.trace(s"HTTP-GET: $iri")
        sttp
          .get(uri"$iri")
          .headers(Map("Accept" -> accept))
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
                .delayExecution(30 seconds)
                .forkAndForget //this should prevent fetching the same resource(-representation) multiple times within a small period of time
            case Some(e) => Task(wip.remove(iri + accept))
          }
      }.memoizeOnSuccess
    )
  }

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
