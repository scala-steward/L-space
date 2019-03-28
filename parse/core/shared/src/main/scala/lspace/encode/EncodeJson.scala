package lspace.encode

import lspace.codec.ActiveContext
import lspace.librarian.traversal.Collection
import lspace.structure.{ClassType, Node}

trait EncodeJson[A] extends Encode[A] {
  def encode: A => String
}

object EncodeJson {

  private def _nodeToJsonMap(node: Node)(implicit encoder: lspace.codec.Encoder): encoder.Json = {
    import encoder._
    encoder.mapToJson(
      node
        .outEMap()
        .map {
          case (property, edges) =>
            property.label("en").getOrElse(property.iri) -> (edges match {
              case List(edge) =>
                encoder.fromAny(edge.to, edge.to.labels.headOption)(ActiveContext()).json
              case edges =>
                encoder.listToJson(edges
                  .map(edge => encoder.fromAny(edge.to, edge.to.labels.headOption)(ActiveContext()).json))
            })
        })
  }

  implicit def nodeToJson[T <: Node](implicit encoder: lspace.codec.Encoder) = new EncodeJson[T] {
    val encode = (node: T) => _nodeToJsonMap(node).toString()
  }

  implicit def nodesToJson[T <: Node](implicit encoder: lspace.codec.Encoder) = new EncodeJson[List[T]] {
    def encode: List[T] => String =
      (nodes: List[T]) => encoder.listToJson(nodes.map(_nodeToJsonMap(_).asInstanceOf[encoder.Json])).toString
  }

  implicit def collectionToJson[T, CT <: ClassType[_]](implicit encoder: lspace.codec.Encoder) =
    new EncodeJson[Collection[T, CT]] {
      def encode: Collection[T, CT] => String =
        (collection: Collection[T, CT]) =>
          encoder.fromAny(collection.item, collection.ct)(ActiveContext()).json.toString
    }

  implicit val encodeJsonJson = new EncodeJson[String] {
    def encode = (string: String) => string
  }
}
