package lspace.encode

import lspace.NS.types
import lspace.codec.jsonld.Encoder
import lspace.codec.{ActiveContext, ActiveProperty, ContextedT, NativeTypeEncoder}
import lspace.librarian.traversal.Collection
import lspace.structure.{ClassType, Node}

import scala.collection.immutable.ListMap

trait EncodeJsonLD[A] extends Encode[A] {
  def encode(implicit activeContext: ActiveContext): A => String
}

object EncodeJsonLD {

  import lspace.codec.JsonInProgress._

  implicit def contextedTToJsonLD[T](implicit en: EncodeJsonLD[T]) = new EncodeJsonLD[ContextedT[T]] {
    def encode(implicit activeContext: ActiveContext) =
      (ct: ContextedT[T]) => en.encode(activeContext ++ ct.activeContext)(ct.t)
  }

  implicit def nodeToJsonLD[T <: Node](implicit encoder: Encoder) = new EncodeJsonLD[T] {
    def encode(implicit activeContext: ActiveContext) = (node: T) => encoder(node)(activeContext)
  }

  implicit def nodesToJsonLD[T](implicit encoder: Encoder) = {
    implicit val bd: NativeTypeEncoder.Aux[encoder.Json] =
      encoder.baseEncoder.asInstanceOf[NativeTypeEncoder.Aux[encoder.Json]]
    import encoder._

    new EncodeJsonLD[List[T]] {
      def encode(implicit activeContext: ActiveContext): List[T] => String =
        (nodes: List[T]) => encoder.fromAny(nodes).withContext.noSpaces
    }
  }

  implicit def collectionToJsonLD[T, CT <: ClassType[_]](implicit encoder: Encoder) =
    new EncodeJsonLD[Collection[T, CT]] {
//      implicit val nte = encoder.baseEncoder
//      import nte._
      import encoder._
      def encode(implicit activeContext: ActiveContext): Collection[T, CT] => String =
        (collection: Collection[T, CT]) => {
          val jip            = encoder.fromAny(collection.item, collection.ct)
          val (startIri, ac) = jip.activeContext.compactIri(Collection.keys.start.property)
          val (endIri, ac2)  = ac.compactIri(Collection.keys.end.property)
          val context = ac2
            .copy(
              definitions = jip.activeContext.definitions() ++ Map(
                Collection.keys.start.property.iri -> ActiveProperty(`@type` = lspace.Label.D.`@datetime` :: Nil,
                                                                     property = Collection.keys.start.property)(),
                Collection.keys.end.property.iri -> ActiveProperty(`@type` = lspace.Label.D.`@datetime` :: Nil,
                                                                   property = Collection.keys.end.property)()
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
      def encode(implicit activeContext: ActiveContext): ActiveContext => String =
        (activeContext: ActiveContext) =>
          Map(
            types.`@context` -> encoder
              .fromActiveContext(activeContext)
              .getOrElse(Map[String, encoder.Json]().asJson)).asJson.noSpaces
    }
  }

  implicit val encodeJsonLDJson = new EncodeJsonLD[String] {
    def encode(implicit activeContext: ActiveContext) = (json: String) => json
  }
  implicit val encodeBooleanJsonLD = new EncodeJsonLD[Boolean] {
    def encode(implicit activeContext: ActiveContext) = (value: Boolean) => value.toString
  }
  implicit val encodeIntJsonLD = new EncodeJsonLD[Int] {
    def encode(implicit activeContext: ActiveContext) = (value: Int) => value.toString
  }
  implicit val encodeDoubleJsonLD = new EncodeJsonLD[Double] {
    def encode(implicit activeContext: ActiveContext) = (value: Double) => value.toString
  }
  implicit val encodeLongJsonLD = new EncodeJsonLD[Long] {
    def encode(implicit activeContext: ActiveContext) = (value: Long) => value.toString
  }
}
