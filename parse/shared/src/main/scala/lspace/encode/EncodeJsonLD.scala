package lspace.encode

import argonaut.Json
import lspace.librarian.structure.{Graph, Node, Property, Resource}

trait EncodeJsonLD[A] {
  val encode: A => Json
}

object EncodeJsonLD {

  implicit def nodeToJsonLD[T <: Node] = new EncodeJsonLD[T] {
    val encode = (node: T) => lspace.parse.JsonLD.detached.encode(node)
  }

//  implicit def nodeToJsonLD[T <: Node] = new EncodeJsonLD[List[T]] {
//    val encode = node => lspace.parse.json.JsonLD.default.nodeToJson(node)._1
//  }

  implicit val encodeJsonLDJson = new EncodeJsonLD[Json] {
    val encode: Json => Json = json => json
  }
}
