package lspace.services.rest.endpoints

import argonaut._
import argonaut.Argonaut._
import cats.effect.IO
import com.twitter.io.Buf
import io.finch.{Endpoint, NotAcceptable, Ok}
import shapeless.{HList, Witness}
import cats.syntax.either._
import io.finch.internal.HttpContent
import lspace.librarian.process.traversal.Traversal
import lspace.librarian.structure._
import lspace.parse.json.JsonLD
import org.slf4j.Logger

import scala.util.{Failure, Success}

object JsonLDModule {
  type JsonLD = Witness.`"application/ld+json"`.T

  object Encode {
    type JsonLD[A] = io.finch.Encode.Aux[A, JsonLDModule.JsonLD]

    private val printer = PrettyParams.nospace.copy(preserveOrder = true)

    implicit def NodeEncodeJsonLD[T <: Node]: EncodeJson[T] =
      EncodeJson { node: Node =>
        lspace.parse.json.JsonLD.default.nodeToJsonWithContext(node)._1
      }

//    implicit def ResourceEncodeJsonLD: EncodeJson[Resource[_]] =
//      EncodeJson { resource: Resource[_] =>
//        lspace.parse.json.JsonLD.default.resourceToJsonWithContext(resource)._1
//      }

    implicit def ListEncodeJsonLD: EncodeJson[List[_]] =
      EncodeJson { list: List[_] =>
        lspace.parse.json.JsonLD.default.anyToJson(list)._1
      }

    implicit def encodeArgonaut[A](implicit e: EncodeJson[A]): JsonLD[A] = {
      io.finch.Encode.instance[A, JsonLDModule.JsonLD]((a, cs) =>
        Buf.ByteArray.Owned(printer.pretty(e.encode(a)).getBytes(cs.name)))
    }
  }

  object Decode {
    type JsonLD[A] = io.finch.Decode.Aux[A, JsonLDModule.JsonLD]

    implicit def decodeArgonaut[A](implicit e: DecodeJson[A]): JsonLD[A] = {
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

  import JsonLDModule.Decode.decodeArgonaut

  def jsonldTraversal(graph: Graph, jsonld: JsonLD): Endpoint[IO, Traversal[ClassType[Any], ClassType[Any], HList]] =
    body[Json, JsonLDModule.JsonLD].mapOutput { json =>
      json.obj
        .map { obj =>
          jsonld.resource(obj).map {
            case traversalNode: Node =>
              try {
                Ok(Traversal.toTraversal(traversalNode)(graph))
              } catch {
                case e: Throwable => NotAcceptable(new Exception("not a valid traversal"))
              }
            case _ =>
              log.debug(s"traversal is not a ${Traversal.ontology.iri}")
              NotAcceptable(new Exception(s"traversal is not a ${Traversal.ontology.iri}"))
          } match {
            case Success(result) => result
            case Failure(error) =>
              log.error(error.getMessage)
              NotAcceptable(new Exception("body is invalid 'application/ld+json'"))
          }
        }
        .getOrElse(NotAcceptable(new Exception("body is invalid json")))
    }
}
