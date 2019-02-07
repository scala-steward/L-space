package lspace.services.rest.endpoints

import argonaut.Json
import cats.effect.IO
import io.finch._
import io.finch.Endpoint
import lspace.librarian.structure.Graph

import scala.collection.mutable

case class NameSpaceService(graph: Graph) extends Api {
  implicit val _graph  = graph
  implicit val encoder = lspace.codec.argonaut.Encode
  implicit val decoder: lspace.codec.Decode[Any, Any] = lspace.codec.argonaut
    .Decode(graph)
    .asInstanceOf[lspace.codec.Decode[Any, Any]] //todo JsonLD context per client-session

  val headersAll = root.map(_.headerMap.toMap)
  val cache      = mutable.HashMap[String, mutable.HashMap[String, String]]()

  /**
    * retrieve a single resource
    * @param uri
    * @return
    */
  val getResource: Endpoint[IO, String] = get(paths[String]) { (paths: Seq[String]) =>
    val path = paths.mkString("/")
    cache
      .get(graph.iri + "/" + path)
      .flatMap(_.get("application/ld+json"))
      .orElse(
        graph.ns.nodes
          .hasIri(graph.iri + "/" + path)
          .headOption
          .map(encoder(_))
          .map { json =>
            cache += (graph.iri + "/" + path)                                                                -> (cache
              .getOrElse(graph.iri + "/" + path, mutable.HashMap[String, String]()) += "application/ld+json" -> json)
            json
          })
      .map(Ok) //.withHeader("Content-Type", "application/ld+json"))
      .getOrElse(io.finch.NotFound(new Exception("unknown path")))
  }

  val byIri: Endpoint[IO, String] = get(param[String]("iri")) { iri: String =>
    cache
      .get(iri)
      .flatMap(_.get("application/ld+json"))
      .orElse(
        graph.ns.nodes
          .hasIri(iri)
          .headOption
          .map(encoder(_))
          .map { json =>
            cache += iri                                                                  -> (cache
              .getOrElse(iri, mutable.HashMap[String, String]()) += "application/ld+json" -> json)
            json
          })
      .map(Ok) //.withHeader("Content-Type", "application/ld+json"))
      .getOrElse(io.finch.NotFound(new Exception("unknown iri")))
  }

  val api = getResource :+: byIri
}
