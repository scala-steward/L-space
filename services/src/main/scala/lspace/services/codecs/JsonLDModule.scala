package lspace.services.codecs

import java.util.UUID

import argonaut.Argonaut._
import argonaut._
import cats.effect.IO
import cats.syntax.either._
import com.twitter.io.Buf
import io.finch.internal.HttpContent
import io.finch.{Endpoint, NotAcceptable, Ok}
import lspace.encode.EncodeJsonLD
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure._
import lspace.parse.util.NotAcceptableException
import monix.eval.Task
import scribe._
import shapeless.Witness

object JsonLDModule {
  type JsonLD = Witness.`"application/ld+json"`.T

  object Encode {
    type JsonLD[A] = io.finch.Encode.Aux[A, JsonLDModule.JsonLD]

    private val printer = PrettyParams.nospace.copy(preserveOrder = true)

    implicit def encodeArgonautJsonLD[A](implicit e: EncodeJsonLD[A]): JsonLD[A] = {
      io.finch.Encode.instance[A, JsonLDModule.JsonLD]((a, cs) =>
        Buf.ByteArray.Owned(printer.pretty(e.encode(a)).getBytes(cs.name)))
    }
  }

  object Decode {
    type JsonLD[A] = io.finch.Decode.Aux[A, JsonLDModule.JsonLD]

    implicit def decodeJson[A](implicit e: DecodeJson[A]): JsonLD[A] = {
      io.finch.Decode.instance[A, JsonLDModule.JsonLD] { (b, cs) =>
        Parse.parse(b.asString(cs)).flatMap(_.as[A].result.leftMap(_._1)) match {
          case Right(result) => Right(result)
          case Left(error)   => Left(new Exception(error))
        }
      }
    }
  }
}

trait JsonLDModule extends Endpoint.Module[IO] {

  import JsonLDModule.Decode._
  import monix.eval.Task.catsEffect
  import monix.execution.Scheduler.global

  def bodyJsonLDTyped[T](label: Ontology,
                         nodeToT: Node => T,
                         allowedProperties: List[Property] = List()): Endpoint[IO, T] =
    bodyJsonLDLabeled(label)
      .map { node =>
        if (allowedProperties.nonEmpty) {
          val fNode = DetachedGraph.nodes.create()
          node.outE(allowedProperties: _*).foreach(e => fNode --- e.key --> e.to)
          fNode
        } else node
      }
      .mapOutputAsync(node =>
        Task(nodeToT(node))
          .map(Ok)
          .onErrorHandle {
            case e =>
              e.logger.debug(e.getMessage)
              NotAcceptable(new Exception(s"not a valid ${label.iri}"))
          }
          .toIO(catsEffect(global)))

  def bodyJsonLDLabeled(label: Ontology, allowedProperties: List[Property] = List()): Endpoint[IO, Node] =
    body[Json, JsonLDModule.JsonLD]
      .handle {
        case t => NotAcceptable(new Exception("body is invalid 'application/ld+json'"))
      }
      .mapOutputAsync { json =>
        val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
        val jsonld      = lspace.parse.JsonLD(resultGraph)
        jsonld.decode
          .toLabeledNode(json, label)
          .map { node =>
            if (allowedProperties.nonEmpty) {
              val fNode = DetachedGraph.nodes.create()
              node.outE(allowedProperties: _*).foreach(e => fNode --- e.key --> e.to)
              fNode
            } else node
          }
          .map(Ok)
          .onErrorHandle {
            case e: NotAcceptableException =>
              e.logger.debug(e.getMessage)
              NotAcceptable(new Exception(s"Body does not look like a ${label.iri}"))
          }
          .toIO(catsEffect(global))
      }

  def bodyJsonLD(allowedProperties: List[Property] = List()): Endpoint[IO, Node] =
    body[Json, JsonLDModule.JsonLD]
      .handle {
        case t => NotAcceptable(new Exception("body is invalid 'application/ld+json'"))
      }
      .mapOutputAsync { json =>
        val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
        val jsonld      = lspace.parse.JsonLD(resultGraph)
        jsonld.decode
          .toNode(json)
          .map { node =>
            if (allowedProperties.nonEmpty) {
              val fNode = DetachedGraph.nodes.create()
              node.outE(allowedProperties: _*).foreach(e => fNode --- e.key --> e.to)
              fNode
            } else node
          }
          .map(Ok)
          .onErrorHandle {
            case e: NotAcceptableException =>
              NotAcceptable(new Exception(s"Body does not look like a node"))
            case e =>
              NotAcceptable(new Exception("not a valid node"))
          }
          .toIO(catsEffect(global))
      }
}
