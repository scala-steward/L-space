package lspace.services.rest.endpoints

import argonaut._
import argonaut.Argonaut._
import cats.effect.IO
import com.twitter.io.Buf
import io.finch.{Endpoint, NotAcceptable, Ok}
import shapeless.{HList, Witness}
import cats.syntax.either._
import io.finch.internal.HttpContent
import lspace.decode.DecodeJsonLD
import lspace.encode.EncodeJsonLD
import lspace.librarian.process.traversal.Traversal
import lspace.librarian.structure._
import lspace.parse.util.{FromJsonException, NotAcceptableException}
import org.slf4j.Logger

import scala.util.{Failure, Success}

object JsonLDModule {
  type JsonLD = Witness.`"application/ld+json"`.T

  object Encode {
    type JsonLD[A] = io.finch.Encode.Aux[A, JsonLDModule.JsonLD]

    private val printer = PrettyParams.nospace.copy(preserveOrder = true)

//    trait EncodeJsonLD[A] {
//      def encode(a: A): Json
//    }
//    implicit def nodeToJsonLD[T <: Node] = new EncodeJsonLD[T] {
//      def encode(a: T): Json = lspace.parse.json.JsonLD.default.nodeToJsonWithContext(a)._1
//    }
//    implicit val encodeJsonLDJson = new EncodeJsonLD[Json] {
//      def encode(a: Json): Json = a
//    }
    implicit def encodeArgonautJsonLD[A](implicit e: EncodeJsonLD[A]): JsonLD[A] = {
      io.finch.Encode.instance[A, JsonLDModule.JsonLD]((a, cs) =>
        Buf.ByteArray.Owned(printer.pretty(e.encode(a)).getBytes(cs.name)))
    }
//    val encPagedResult = EncodeJson { result: PagedResult =>
//      result.result.map(_.asJson).asJson
//    }
//    implicit def encodeArgonautPagedResult: JsonLD[PagedResult] = {
//      io.finch.Encode.instance[PagedResult, JsonLDModule.JsonLD]((a, cs) =>
//        Buf.ByteArray.Owned(printer.pretty(e.encode(a)).getBytes(cs.name)))
//    }
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
  def log: Logger

  import JsonLDModule.Decode._

  import monix.eval.Task.catsEffect
  import monix.execution.Scheduler.global

  def bodyJsonLDTraversal(implicit graph: Graph,
                          jsonld: lspace.parse.JsonLD): Endpoint[IO, Traversal[ClassType[Any], ClassType[Any], HList]] =
    body[Json, JsonLDModule.JsonLD]
      .handle {
        case t => NotAcceptable(new Exception("body is invalid 'application/ld+json'"))
      }
      .mapOutputAsync { json =>
        jsonld.decode
          .toLabeledNode(json, Traversal.ontology)
          .map { traversalNode =>
            Ok(Traversal.toTraversal(traversalNode)(graph))
          }
          .onErrorHandle {
            case e: NotAcceptableException =>
              log.debug(e.getMessage)
              NotAcceptable(new Exception(s"Body does not look like a ${Traversal.ontology.iri}"))
            case e =>
              log.debug(e.getMessage)
              NotAcceptable(new Exception("not a valid traversal"))
          }
          .toIO(catsEffect(global))
      }

  def bodyJsonLDNode(implicit graph: Graph, jsonld: lspace.parse.JsonLD): Endpoint[IO, Node] =
    body[Json, JsonLDModule.JsonLD]
      .handle {
        case t => NotAcceptable(new Exception("body is invalid 'application/ld+json'"))
      }
      .mapOutputAsync { json =>
        jsonld.decode
          .toNode(json)
          .map(Ok)
          .onErrorHandle {
            case e: NotAcceptableException =>
              NotAcceptable(new Exception(s"Body does not look like a ${Traversal.ontology.iri}"))
            case e =>
              NotAcceptable(new Exception("not a valid traversal"))
          }
          .toIO(catsEffect(global))
      }
}
