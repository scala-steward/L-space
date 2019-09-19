package lspace.services.rest.endpoints

import argonaut.Json
import cats.effect.IO
import com.twitter.finagle.http.Response
import io.finch._
import io.finch.Endpoint
import lspace.codec.{ActiveContext}
import lspace.codec.json
import lspace.codec.json.{JsonDecoder, JsonEncoder}
import lspace.codec.json.jsonld
import lspace.codec.json.jsonld.{JsonLDDecoder, JsonLDEncoder}
import lspace.structure.{Graph, Lspace}
import monix.eval.Task

import scala.collection.mutable

object NameSpaceService {
  def apply[Json](graph0: Graph)(implicit baseDecoder0: JsonDecoder[Json],
                                 baseEncoder0: JsonEncoder[Json]): NameSpaceService[Json] =
    new NameSpaceService[Json] {
      val graph: Graph                                     = graph0
      implicit override def baseDecoder: JsonDecoder[Json] = baseDecoder0
      implicit override def baseEncoder: JsonEncoder[Json] = baseEncoder0
    }
}

trait NameSpaceService[Json] extends Api {
  def graph: Graph
  implicit def baseDecoder: JsonDecoder[Json]
  implicit def baseEncoder: JsonEncoder[Json]

  import lspace.Implicits.Scheduler.global

  val encoder = jsonld.Encoder(baseEncoder)

  val headersAll = root.map(_.headerMap.toMap)
  val cache      = mutable.HashMap[String, mutable.HashMap[String, String]]()

  /**
    * retrieve a single resource
    * @return
    */
  val getResource: Endpoint[IO, String] = get(paths[String]) { (paths: List[String]) =>
    val path = paths.mkString("/")
    cache
      .get(graph.iri + "/" + path)
      .flatMap(_.get("application/ld+json"))
      .map(Task.now)
      .getOrElse(
        graph.ns.nodes
          .hasIri(graph.iri + "/" + path)
          .headL
          .map(encoder(_)(ActiveContext()))
          .map { json =>
            cache += (graph.iri + "/" + path)                                                                -> (cache
              .getOrElse(graph.iri + "/" + path, mutable.HashMap[String, String]()) += "application/ld+json" -> json)
            json
          })
      .map(Ok)
      .map(_.withHeader("Content-Type", "application/ld+json"))
      .onErrorHandle(f => io.finch.NotFound(new Exception("unknown path")))
      .to[IO]
  }

  val byIri: Endpoint[IO, String] = get(param[String]("iri")) { iri: String =>
    cache
      .get(iri)
      .flatMap(_.get("application/ld+json"))
      .map(Task.now)
      .getOrElse(
        graph.ns.nodes
          .hasIri(iri)
          .headL
          .map(encoder(_)(ActiveContext()))
          .map { json =>
            cache += iri                                                                  -> (cache
              .getOrElse(iri, mutable.HashMap[String, String]()) += "application/ld+json" -> json)
            json
          })
      .map(Ok)
      .map(_.withHeader("Content-Type", "application/ld+json"))
      .onErrorHandle(f => io.finch.NotFound(new Exception("unknown iri")))
      .to[IO]
  }

  val api = getResource :+: byIri
}
