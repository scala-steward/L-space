package lspace.encode

import lspace.NS.types
import lspace.codec.json.JsonEncoder
import lspace.codec.json.jsonld.{Encoder, JsonLDEncoder}
import lspace.codec.{ActiveContext, ActiveProperty, ContextedT}
import lspace.encode.EncodeJson._queryresultToJsonMap
import lspace.graphql.QueryResult
import lspace.librarian.traversal.Collection
import lspace.structure.{ClassType, Node}
import monix.eval.Coeval

import scala.collection.immutable.ListMap

trait EncodeJsonLD[A, F[_]] extends Encode[A, F] {
  type Out = String
}

object EncodeJsonLD {
  type Aux[In, F[_], Out0] = EncodeJsonLD[In, F] { type Out = Out0 }

  import lspace.codec.JsonInProgress._

  implicit def contextedTToJsonLD[T](implicit en: EncodeJsonLD[T, Coeval]) = new EncodeJsonLD[ContextedT[T], Coeval] {
    def encode(implicit activeContext: ActiveContext) =
      (ct: ContextedT[T]) => en.encode(activeContext ++ ct.activeContext)(ct.t)
  }

  implicit def nodeToJsonLD[T <: Node, Json](implicit encoder: JsonLDEncoder[Json]) = new EncodeJsonLD[T, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (node: T) => Coeval(encoder(node)(activeContext))
  }

  implicit def nodesToJsonLD[T, Json](implicit encoder: JsonLDEncoder[Json]) = {
    implicit val bd: JsonEncoder[Json] =
      encoder.baseEncoder.asInstanceOf[JsonEncoder[Json]]
    import encoder._
    import encoder.baseEncoder._

    new EncodeJsonLD[List[T], Coeval] {
      def encode(implicit activeContext: ActiveContext): List[T] => Coeval[String] =
        (nodes: List[T]) => Coeval(encoder.fromAny(nodes).withContext.noSpaces)
    }
  }

  implicit def collectionToJsonLD[T, CT <: ClassType[_], Json](implicit encoder: JsonLDEncoder[Json]) =
    new EncodeJsonLD[Collection[T, CT], Coeval] {
//      implicit val nte = encoder.baseEncoder
//      import nte._
      import encoder._
      import encoder.baseEncoder._

      def encode(implicit activeContext: ActiveContext): Collection[T, CT] => Coeval[String] =
        (collection: Collection[T, CT]) =>
          Coeval {
            val jip            = encoder.fromAny(collection.item, collection.ct)
            val (startIri, ac) = jip.activeContext.compactIri(Collection.keys.start.property)
            val (endIri, ac2)  = ac.compactIri(Collection.keys.end.property)
            val context = ac2
              .copy(
                definitions = jip.activeContext.definitions() ++ Map(
                  Collection.keys.start.property.iri -> ActiveProperty(
                    `@type` = lspace.Label.D.`@datetime` :: Nil,
                    property = Collection.keys.start.property
                  )(),
                  Collection.keys.end.property.iri -> ActiveProperty(
                    `@type` = lspace.Label.D.`@datetime` :: Nil,
                    property = Collection.keys.end.property
                  )()
                )
              )

            ListMap(
              types.`@context`                  -> context.asJson.get,
              types.`@type`                     -> Collection.ontology.iri.asJson,
              startIri                          -> collection.startDateTime.asJson,
              endIri                            -> collection.endDateTime.asJson,
              Collection.keys.item.property.iri -> jip.json
            ).asJson.noSpaces
          }
    }

  implicit def activeContextToJsonLD[Json](implicit encoder: JsonLDEncoder[Json]) = {
    import encoder.baseEncoder._

    new EncodeJsonLD[ActiveContext, Coeval] {
      def encode(implicit activeContext: ActiveContext): ActiveContext => Coeval[String] =
        (activeContext: ActiveContext) =>
          Coeval {
            Map(
              types.`@context` -> encoder
                .fromActiveContext(activeContext)
                .getOrElse(Map[String, Json]().asJson)
            ).asJson.noSpaces
          }
    }
  }

  implicit val encodeJsonLDJson = new EncodeJsonLD[String, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (json: String) => Coeval(json)
  }
  implicit val encodeBooleanJsonLD = new EncodeJsonLD[Boolean, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (value: Boolean) => Coeval(value.toString)
  }
  implicit val encodeIntJsonLD = new EncodeJsonLD[Int, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (value: Int) => Coeval(value.toString)
  }
  implicit val encodeDoubleJsonLD = new EncodeJsonLD[Double, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (value: Double) => Coeval(value.toString)
  }
  implicit val encodeLongJsonLD = new EncodeJsonLD[Long, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (value: Long) => Coeval(value.toString)
  }

  implicit def queryResultToJsonLD[T <: QueryResult, Json](implicit
    encoder: JsonLDEncoder[Json]
  ): EncodeJsonLD[T, Coeval] =
    new EncodeJsonLD[T, Coeval] {
      import encoder.baseEncoder._
      def encode(implicit activeContext: ActiveContext) =
        (node: T) => Coeval(_queryresultToJsonMap(node).asInstanceOf[Json].noSpaces)
    }
}
