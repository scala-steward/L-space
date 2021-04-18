package lspace.encode

import lspace.NS.types
import lspace.codec.{ActiveContext, ContextedT}
import lspace.librarian.traversal.Collection
import lspace._
import Label.P._
import lspace.codec.json.jsonld.JsonLDEncoder
import lspace.graphql.{Projection, QueryResult}
import monix.eval.{Coeval, Task}

trait EncodeJson[A, F[_]] extends Encode[A, F] {
  type Out = String
}

object EncodeJson {
  type Aux[In, F[_], Out0] = EncodeJson[In, F] { type Out = Out0 }

  implicit def contextedTToJson[T](implicit en: EncodeJson[T, Coeval]) = new EncodeJson[ContextedT[T], Coeval] {
    def encode(implicit activeContext: ActiveContext) =
      (ct: ContextedT[T]) => en.encode(activeContext ++ ct.activeContext)(ct.t)
  }

  private def _nodeToJsonMap[Json](resource: Resource[_])(implicit encoder: JsonLDEncoder[Json],
                                                          activeContext: ActiveContext): Json = {
    import encoder.baseEncoder._
    (List(
      Some(resource.iri).filter(_.nonEmpty).map(_.asJson).map(`@id`.iri -> _),
      Some(resource.labels)
        .filter(_.nonEmpty)
        .map {
          case List(label) => activeContext.compactIri(label.iri).asJson
          case labels =>
            val x = labels.map(_.iri).map(activeContext.compactIri(_).asJson)
            x.asJson
            labels.map(_.iri).map(activeContext.compactIri(_).asJson).asJson
        }
        .map(`@type`.iri -> _)
    ).filter(_.nonEmpty).map(_.get).toMap ++
      resource
        .outEMap()
        .map {
          case (property, edges) =>
            activeContext.compactIri(property.iri) -> (edges match {
              case List(edge) =>
                encoder.fromAny(edge.to, edge.to.labels.headOption).json
              case edges =>
                edges
                  .map(edge => encoder.fromAny(edge.to, edge.to.labels.headOption).json)
                  .asJson
            })
        }).asJson
  }

  implicit def nodeToJson[Json, T <: Node](implicit encoder: JsonLDEncoder[Json]): EncodeJson[T, Coeval] =
    new EncodeJson[T, Coeval] {
      import encoder.baseEncoder._
      def encode(implicit activeContext: ActiveContext): T => Coeval[String] =
        (node: T) => Coeval { _nodeToJsonMap(node).noSpaces }
    }

  implicit def nodesToJson[T, Json](implicit encoder: JsonLDEncoder[Json]): EncodeJson[List[T], Coeval] =
    new EncodeJson[List[T], Coeval] {
      import encoder.baseEncoder._
      def encode(implicit activeContext: ActiveContext): List[T] => Coeval[String] =
        (nodes: List[T]) =>
          Coeval {
            (nodes
              .map {
                case node: Node       => _nodeToJsonMap(node).asInstanceOf[Json]
                case edge: Edge[_, _] => _nodeToJsonMap(edge).asInstanceOf[Json]
                case value: Value[_]  => _nodeToJsonMap(value).asInstanceOf[Json]
                case value            => encoder.fromAny(value).json
              })
              .asJson
              .noSpaces
        }
    }

  implicit def collectionToJson[T, CT <: ClassType[_], Json](
      implicit encoder: JsonLDEncoder[Json]): EncodeJson[Collection[T, CT], Coeval] =
    new EncodeJson[Collection[T, CT], Coeval] {
      import encoder.baseEncoder._
      def encode(implicit activeContext: ActiveContext): Collection[T, CT] => Coeval[String] =
        (collection: Collection[T, CT]) => Coeval { encoder.fromAny(collection.item, collection.ct).json.noSpaces }
    }

  implicit def activeContextToJson[Json](implicit encoder: JsonLDEncoder[Json]): EncodeJson[ActiveContext, Coeval] = {
    import encoder.baseEncoder._

    new EncodeJson[ActiveContext, Coeval] {
      def encode(implicit activeContext: ActiveContext): ActiveContext => Coeval[String] =
        (activeContext: ActiveContext) =>
          Coeval {
            Map(
              types.`@context` -> encoder
                .fromActiveContext(activeContext)
                .getOrElse(Map[String, Json]().asJson)).asJson.noSpaces
        }
    }
  }

  implicit val encodeJsonJson: EncodeJson[String, Coeval] = new EncodeJson[String, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (string: String) => Coeval { string }
  }
  implicit val encodeBooleanJson: EncodeJson[Boolean, Coeval] = new EncodeJson[Boolean, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (value: Boolean) => Coeval { value.toString }
  }
  implicit val encodeIntJson: EncodeJson[Int, Coeval] = new EncodeJson[Int, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (value: Int) => Coeval { value.toString }
  }
  implicit val encodeDoubleJson: EncodeJson[Double, Coeval] = new EncodeJson[Double, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (value: Double) => Coeval { value.toString }
  }
  implicit val encodeLongJson: EncodeJson[Long, Coeval] = new EncodeJson[Long, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (value: Long) => Coeval { value.toString }
  }

  protected[encode] def namedProjection[Json](projection: Projection, result: List[Any])(
      implicit encoder: JsonLDEncoder[Json],
      activeContext: ActiveContext): Option[Json] = {
    import encoder._
    import encoder.baseEncoder._

    result match {
      case Nil => None
      case result =>
        projection.projections match {
          case Nil =>
            val expKey = activeContext.expandIri(projection.name).iri
            result match {
              case List(value) =>
                Some(encoder.fromAny(value, activeContext.expectedType(expKey)).json)
              case values =>
                Some(values.map(value => encoder.fromAny(value, activeContext.expectedType(expKey)).json).asJson)
            }
          case projections =>
            Some(
              (projections
                .zip(result))
                .flatMap {
                  case (projection, result: List[_]) =>
                    namedProjection(projection, result).map(v => projection.alias -> v)
                  case _ => throw new Exception("invalid")
                }
                .toMap).map(_.asJson)
        }
    }
  }

  protected[encode] def _queryresultToJsonMap[Json](queryResult: QueryResult)(implicit encoder: JsonLDEncoder[Json],
                                                                              activeContext: ActiveContext): Json = {
    import encoder.baseEncoder._

    queryResult.result match {
      case Nil => List[Json]().asJson
      case result =>
        (result.map {
          case result: List[_] =>
            ((queryResult.query.projections
              .zip(result))
              .flatMap {
                case (projection, result: List[_]) =>
                  namedProjection(projection, result).map(projection.alias -> _)
                case _ => throw new Exception("invalid")
              }
              .toMap)
              .asJson
          case result: Product =>
            ((queryResult.query.projections
              .zip(result.productIterator.toList))
              .flatMap {
                case (projection, result: List[_]) =>
                  namedProjection(projection, result).map(projection.alias -> _)
                case _ => throw new Exception("invalid")
              }
              .toMap)
              .asJson
        }).asJson
    }
  }

  implicit def queryResultToJson[T <: QueryResult, Json](implicit encoder: JsonLDEncoder[Json]): EncodeJson[T, Coeval] =
    new EncodeJson[T, Coeval] {
      import encoder.baseEncoder._
      def encode(implicit activeContext: ActiveContext) =
        (node: T) => Coeval { _queryresultToJsonMap(node).asInstanceOf[Json].noSpaces }
    }
}
