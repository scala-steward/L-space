package lspace.encode

import lspace.librarian.structure.Node

trait EncodeJsonLD[A] extends Encode[A] {
  def encode: A => String
}

object EncodeJsonLD {

  import lspace.codec.JsonInProgress._

  implicit def nodeToJsonLD[T <: Node, Json](implicit encoder: lspace.codec.Encoder[Json]) = new EncodeJsonLD[T] {
    def encode = (node: T) => encoder(node)
  }

  implicit def nodesToJsonLD[T <: Node, Json](implicit encoder: lspace.codec.Encoder[Json]) =
    new EncodeJsonLD[List[T]] {
      def encode: List[T] => String =
        (nodes: List[T]) => encoder.fromAny(nodes)(encoder.getNewActiveContext).withContext.toString
    }

  implicit val encodeJsonLDJson = new EncodeJsonLD[String] {
    val encode = (json: String) => json
  }
}
