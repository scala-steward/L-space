package lspace.encode

import argonaut.{Json, PrettyParams}
import lspace.librarian.structure.Node
import lspace.parse.ActiveContext

trait EncodeJson[A] extends Encode[A] {
  def encode: A => String
}

object EncodeJson {

  import lspace.parse.JsonInProgress._
  private val printer = PrettyParams.nospace.copy(preserveOrder = true)

  implicit def nodeToJson[T <: Node] = new EncodeJson[T] {
    import argonaut.Argonaut._

    val encode = (node: T) => //lspace.codec.argonaut.Encode(node)
      printer.pretty(
        node
          .outEMap()
          .map {
            case (property, edges) =>
              property.label.get("en").getOrElse(property.iri) -> (edges match {
                case List(e) => lspace.codec.argonaut.Encode.fromAny(e.to, e.to.labels.headOption)(ActiveContext()).json
              })
          }
          .asJson)
  }

  implicit def nodeToJsonLD[T <: Node] = new EncodeJson[List[T]] {
    def encode: List[T] => String =
      (nodes: List[T]) =>
        printer.pretty(Json.jObject(lspace.codec.argonaut.Encode.fromAny(nodes)(ActiveContext()).withContext))
  }

  //  implicit def nodeToJsonLD[T <: Node] = new EncodeJsonLD[List[T]] {
  //    val encode = node => lspace.parse.json.JsonLD.default.nodeToJson(node)._1
  //  }

//  implicit val encodeJsonJson = new EncodeJson[Json] {
//    val encode = (json: Json) => printer.pretty(json)
//  }

//  implicit val encodeJsonLDJson = new EncodeJson[Json] {
//    val encode = (json: Json) => json.toString()
//  }
  implicit val encodeJsonJson = new EncodeJson[String] {
    def encode = (json: String) => json
  }
}
