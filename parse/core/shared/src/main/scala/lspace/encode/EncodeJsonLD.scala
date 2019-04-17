package lspace.encode

import lspace.NS.types
import lspace.codec.jsonld.Encoder
import lspace.codec.{ActiveContext, ActiveProperty, NativeTypeEncoder}
import lspace.librarian.traversal.Collection
import lspace.structure.{ClassType, Node}

import scala.collection.immutable.ListMap

trait EncodeJsonLD[A] extends Encode[A] {
  def encode: A => String
}

object EncodeJsonLD {

  import lspace.codec.JsonInProgress._

  implicit def nodeToJsonLD[T <: Node](implicit encoder: Encoder, activeContext: ActiveContext) = new EncodeJsonLD[T] {
    def encode = (node: T) => encoder(node)
  }

  implicit def nodesToJsonLD[T <: Node](implicit encoder: Encoder, activeContext: ActiveContext) = {
    implicit val bd: NativeTypeEncoder.Aux[encoder.Json] =
      encoder.baseEncoder.asInstanceOf[NativeTypeEncoder.Aux[encoder.Json]]
    import encoder._

    new EncodeJsonLD[List[T]] {
      def encode: List[T] => String =
        (nodes: List[T]) => encoder.fromAny(nodes).withContext.noSpaces
    }
  }

  implicit def collectionToJsonLD[T, CT <: ClassType[_]](implicit encoder: Encoder, activeContext: ActiveContext) =
    new EncodeJsonLD[Collection[T, CT]] {
//      implicit val nte = encoder.baseEncoder
//      import nte._
      import encoder._
      def encode: Collection[T, CT] => String =
        (collection: Collection[T, CT]) => {
          val jip            = encoder.fromAny(collection.item, collection.ct)
          val (startIri, ac) = jip.activeContext.compactIri(Collection.keys.start.property)
          val (endIri, ac2)  = ac.compactIri(Collection.keys.end.property)
          val context = ac2
            .copy(
              definitions = jip.activeContext.definitions ++ Map(
                Collection.keys.start.property.iri -> ActiveProperty(`@type` = lspace.Label.D.`@datetime` :: Nil,
                                                                     property = Collection.keys.start.property),
                Collection.keys.end.property.iri -> ActiveProperty(`@type` = lspace.Label.D.`@datetime` :: Nil,
                                                                   property = Collection.keys.end.property)
              ))

          ListMap(
            types.`@context`                  -> context.asJson.get,
            types.`@type`                     -> Collection.ontology.iri.asJson,
            startIri                          -> collection.startDateTime.asJson,
            endIri                            -> collection.endDateTime.asJson,
            Collection.keys.item.property.iri -> jip.json
          ).asJson.noSpaces
        }
    }

  implicit def activeContextToJsonLD(implicit encoder: Encoder) = {
    import encoder._

    new EncodeJsonLD[ActiveContext] {
      def encode: ActiveContext => String =
        (activeContext: ActiveContext) =>
          types.`@context` + ": " + encoder.fromActiveContext(activeContext).getOrElse("{}".asJson).noSpaces
    }
  }

  implicit val encodeJsonLDJson = new EncodeJsonLD[String] {
    val encode = (json: String) => json
  }
}
