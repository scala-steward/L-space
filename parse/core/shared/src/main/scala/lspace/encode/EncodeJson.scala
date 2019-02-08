package lspace.encode

import lspace.librarian.structure.Node

trait EncodeJson[A] extends Encode[A] {
  def encode: A => String
}

object EncodeJson {

  private def _nodeToJsonMap[Json](node: Node)(implicit encoder: lspace.codec.Encoder[Json]): Json = {
    encoder.mapToJson(
      node
        .outEMap()
        .map {
          case (property, edges) =>
            property.label.get("en").getOrElse(property.iri) -> (edges match {
              case List(edge) =>
                encoder.fromAny(edge.to, edge.to.labels.headOption)(encoder.getNewActiveContext).json
              case edges =>
                encoder.listToJson(edges
                  .map(edge => encoder.fromAny(edge.to, edge.to.labels.headOption)(encoder.getNewActiveContext).json))
            })
        })
  }

  implicit def nodeToJson[T <: Node, Json](implicit encoder: lspace.codec.Encoder[Json]) = new EncodeJson[T] {

    val encode = (node: T) => _nodeToJsonMap(node).toString()
  }

  implicit def nodeToJsonLD[T <: Node, Json](implicit encoder: lspace.codec.Encoder[Json]) = new EncodeJson[List[T]] {
    def encode: List[T] => String =
      (nodes: List[T]) => encoder.listToJson(nodes.map(_nodeToJsonMap(_))).toString
  }

  implicit val encodeJsonJson = new EncodeJson[String] {
    def encode = (string: String) => string
  }
}
