package lspace.encode

import lspace.NS.types
import lspace.codec.{ActiveContext, ActiveProperty, NativeTypeEncoder}
import lspace.librarian.traversal.Collection
import lspace.structure.{ClassType, Node}

import scala.collection.immutable.ListMap

trait EncodeJsonLD[A] extends Encode[A] {
  def encode: A => String
}

object EncodeJsonLD {

  import lspace.codec.JsonInProgress._

  implicit def nodeToJsonLD[T <: Node](implicit encoder: lspace.codec.Encoder) = new EncodeJsonLD[T] {
    def encode = (node: T) => encoder(node)
  }

  implicit def nodesToJsonLD[T <: Node](implicit encoder: lspace.codec.Encoder) = {
    implicit val bd: NativeTypeEncoder.Aux[encoder.Json] =
      encoder.baseEncoder.asInstanceOf[NativeTypeEncoder.Aux[encoder.Json]]

    new EncodeJsonLD[List[T]] {
      def encode: List[T] => String =
        (nodes: List[T]) => encoder.fromAny(nodes)(ActiveContext()).withContext.toString
    }
  }

  implicit def collectionToJsonLD[T, CT <: ClassType[_]](implicit encoder: lspace.codec.Encoder) =
    new EncodeJsonLD[Collection[T, CT]] {
//      implicit val nte = encoder.baseEncoder
//      import nte._
      import encoder._
      def encode: Collection[T, CT] => String =
        (collection: Collection[T, CT]) => {
          val jip            = encoder.fromAny(collection.item, collection.ct)(ActiveContext())
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
          ).asJson.toString
        }
    }

  implicit val encodeJsonLDJson = new EncodeJsonLD[String] {
    val encode = (json: String) => json
  }
}
