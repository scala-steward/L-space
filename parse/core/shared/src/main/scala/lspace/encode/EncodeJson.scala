package lspace.encode

import lspace.NS.types
import lspace.codec.ActiveContext
import lspace.codec.jsonld.Encoder
import lspace.librarian.traversal.Collection
import lspace.structure.{ClassType, Node}

trait EncodeJson[A] extends Encode[A] {
  def encode: A => String
}

object EncodeJson {

  private def _nodeToJsonMap(node: Node)(implicit encoder: Encoder, context: ActiveContext): encoder.Json = {
    import encoder._
    encoder.mapToJson(
      node
        .outEMap()
        .map {
          case (property, edges) =>
            property.label("en").getOrElse(property.iri) -> (edges match {
              case List(edge) =>
                encoder.fromAny(edge.to, edge.to.labels.headOption).json
              case edges =>
                encoder.listToJson(edges
                  .map(edge => encoder.fromAny(edge.to, edge.to.labels.headOption).json))
            })
        })
  }

  implicit def nodeToJson[T <: Node](implicit encoder: Encoder, activeContext: ActiveContext) = new EncodeJson[T] {
    val encode = (node: T) => _nodeToJsonMap(node).toString()
  }

  implicit def nodesToJson[T <: Node](implicit encoder: Encoder, activeContext: ActiveContext) =
    new EncodeJson[List[T]] {
      def encode: List[T] => String =
        (nodes: List[T]) => encoder.listToJson(nodes.map(_nodeToJsonMap(_).asInstanceOf[encoder.Json])).toString
    }

  implicit def collectionToJson[T, CT <: ClassType[_]](implicit encoder: Encoder, activeContext: ActiveContext) =
    new EncodeJson[Collection[T, CT]] {
      def encode: Collection[T, CT] => String =
        (collection: Collection[T, CT]) => encoder.fromAny(collection.item, collection.ct).json.toString
    }

  implicit def activeContextToJson(implicit encoder: Encoder) = {
    import encoder._

    new EncodeJson[ActiveContext] {
      def encode: ActiveContext => String =
        (activeContext: ActiveContext) =>
          types.`@context` + ": " + encoder.fromActiveContext(activeContext).getOrElse("{}".asJson).noSpaces
    }
  }

  implicit val encodeJsonJson = new EncodeJson[String] {
    def encode = (string: String) => string
  }
}
