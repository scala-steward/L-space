package lspace.encode

import lspace.NS.types
import lspace.codec.{ActiveContext, ContextedT}
import lspace.codec.jsonld.Encoder
import lspace.librarian.traversal.Collection
import lspace.structure.{ClassType, Node}

trait EncodeJson[A] extends Encode[A] {
  def encode(implicit activeContext: ActiveContext): A => String
}

object EncodeJson {

  implicit def contextedTToJsonLD[T](implicit en: EncodeJson[T]) = new EncodeJson[ContextedT[T]] {
    def encode(implicit activeContext: ActiveContext) =
      (ct: ContextedT[T]) => en.encode(activeContext ++ ct.activeContext)(ct.t)
  }

  private def _nodeToJsonMap(node: Node)(implicit encoder: Encoder, activeContext: ActiveContext): encoder.Json = {
    import encoder._
    encoder.mapToJson(
      node
        .outEMap()
        .map {
          case (property, edges) =>
            activeContext.compactIri(property.iri) -> (edges match {
              case List(edge) =>
                encoder.fromAny(edge.to, edge.to.labels.headOption).json
              case edges =>
                encoder.listToJson(edges
                  .map(edge => encoder.fromAny(edge.to, edge.to.labels.headOption).json))
            })
        })
  }

  implicit def nodeToJson[T <: Node](implicit encoder: Encoder) = new EncodeJson[T] {
    import encoder._
    def encode(implicit activeContext: ActiveContext) =
      (node: T) => _nodeToJsonMap(node).asInstanceOf[encoder.Json].noSpaces
  }

  implicit def nodesToJson[T <: Node](implicit encoder: Encoder) =
    new EncodeJson[List[T]] {
      import encoder._
      def encode(implicit activeContext: ActiveContext): List[T] => String =
        (nodes: List[T]) => encoder.listToJson(nodes.map(_nodeToJsonMap(_).asInstanceOf[encoder.Json])).noSpaces
    }

  implicit def collectionToJson[T, CT <: ClassType[_]](implicit encoder: Encoder) =
    new EncodeJson[Collection[T, CT]] {
      import encoder._
      def encode(implicit activeContext: ActiveContext): Collection[T, CT] => String =
        (collection: Collection[T, CT]) => encoder.fromAny(collection.item, collection.ct).json.noSpaces
    }

  implicit def activeContextToJson(implicit encoder: Encoder) = {
    import encoder._

    new EncodeJson[ActiveContext] {
      def encode(implicit activeContext: ActiveContext): ActiveContext => String =
        (activeContext: ActiveContext) =>
          types.`@context` + ": " + encoder
            .fromActiveContext(activeContext)
            .getOrElse(Map[String, encoder.Json]().asJson)
            .noSpaces
    }
  }

  implicit val encodeJsonJson = new EncodeJson[String] {
    def encode(implicit activeContext: ActiveContext) = (string: String) => string
  }
  implicit val encodeBooleanJson = new EncodeJson[Boolean] {
    def encode(implicit activeContext: ActiveContext) = (value: Boolean) => value.toString
  }
  implicit val encodeIntJson = new EncodeJson[Int] {
    def encode(implicit activeContext: ActiveContext) = (value: Int) => value.toString
  }
  implicit val encodeDoubleJson = new EncodeJson[Double] {
    def encode(implicit activeContext: ActiveContext) = (value: Double) => value.toString
  }
  implicit val encodeLongJson = new EncodeJson[Long] {
    def encode(implicit activeContext: ActiveContext) = (value: Long) => value.toString
  }
}
