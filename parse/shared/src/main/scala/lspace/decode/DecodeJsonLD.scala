package lspace.decode

import argonaut.{Json, Parse}
import lspace.librarian.process.traversal.Traversal
import lspace.librarian.structure.{ClassType, Graph, Node, Ontology}
import lspace.parse.JsonLD
import monix.eval.Task
import shapeless.HList

import scala.util.{Failure, Success}

trait DecodeJsonLD[A] {
  def decode(json: Json): Task[A]
}

object DecodeJsonLD {

  case class InvalidJsonLD(message: String)       extends DecodeException(message)
  case class NotAcceptableJsonLD(message: String) extends NotAcceptable(message)

  implicit def toNode(implicit parser: JsonLD) = new DecodeJsonLD[Node] {
    def decode(json: Json): Task[Node] =
      parser.decode
        .toNode(json)
  }

  //.onErrorHandle( t => Task.raiseError(InvalidJsonLD("body with type 'application/ld+json' is not an object")))
//      json.obj
//        .map(parser.fromJsonLD(_))
//        .getOrElse(throw InvalidJsonLD("body with type 'application/ld+json' is not an object")) match {
//        case Success(result: Node) => result
//        case Success(result)       => throw NotAcceptableJsonLD("not a node")
//        case Failure(error)        => throw error
//      }

  implicit def toTraversal(implicit parser: JsonLD,
                           graph: Graph): DecodeJsonLD[Traversal[ClassType[Any], ClassType[Any], HList]] =
    new DecodeJsonLD[Traversal[ClassType[Any], ClassType[Any], HList]] {
      def decode(json: Json): Task[Traversal[ClassType[Any], ClassType[Any], HList]] =
        parser.decode
          .toLabeledNode(json, Traversal.ontology)
          .map { node =>
            Traversal.toTraversal(node)(graph)
          }
//        try {
//          Traversal.toTraversal(decoder.decode(json))(graph)
//        } catch {
//          case e => throw NotAcceptableJsonLD("not a valid traversal")
//        }
    }

}
