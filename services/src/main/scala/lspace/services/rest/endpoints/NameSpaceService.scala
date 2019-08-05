package lspace.services.rest.endpoints

import argonaut.Json
import cats.effect.IO
import com.twitter.finagle.http.Response
import io.finch._
import io.finch.Endpoint
import lspace.codec.{jsonld, ActiveContext, NativeTypeDecoder, NativeTypeEncoder}
import lspace.structure.{Graph, Lspace}
import monix.eval.Task

import scala.collection.mutable

object NameSpaceService {
  def apply[Json0](graph0: Graph)(implicit baseDecoder0: NativeTypeDecoder.Aux[Json0],
                                  baseEncoder0: NativeTypeEncoder.Aux[Json0]): NameSpaceService =
    new NameSpaceService {
      val graph: Graph = graph0
      type Json = Json0
      implicit override def baseDecoder: NativeTypeDecoder.Aux[Json] = baseDecoder0
      implicit override def baseEncoder: NativeTypeEncoder.Aux[Json] = baseEncoder0
    }
}

trait NameSpaceService extends Api {
  def graph: Graph
  type Json
  implicit def baseDecoder: NativeTypeDecoder.Aux[Json]
  implicit def baseEncoder: NativeTypeEncoder.Aux[Json]

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
