package lspace.encode

import argonaut.{Json, PrettyParams}
import lspace.librarian.structure.{Graph, Node, Property, Resource}
import lspace.parse.ActiveContext

trait EncodeJsonLD[A] extends Encode[A] {
  def encode: A => String
}

object EncodeJsonLD {

  import lspace.parse.JsonInProgress._
  private val printer = PrettyParams.nospace.copy(preserveOrder = true)

  implicit def nodeToJsonLD[T <: Node] = new EncodeJsonLD[T] {
    def encode = (node: T) => lspace.codec.argonaut.Encode(node)
  }
  //  implicit def pagedResultToJsonLD = new EncodeJsonLD[(Ontology, List[Node])] {
  //    val encode: ((Ontology, List[Node])) => Json = {
  //      case (ontology: Ontology, nodes: List[Node]) =>
  //        Json.jObject(encoder.fromAny(nodes)(ActiveContext()).withContext)
  //    }
  //  }

  implicit def nodesToJsonLD[T <: Node] = new EncodeJsonLD[List[T]] {
    def encode: List[T] => String =
      (node: List[T]) =>
        printer.pretty(Json.jObject(lspace.codec.argonaut.Encode.fromAny(node)(ActiveContext()).withContext))
  }

  implicit val encodeJsonLDJson = new EncodeJsonLD[String] {
    val encode = (json: String) => json
  }
}
