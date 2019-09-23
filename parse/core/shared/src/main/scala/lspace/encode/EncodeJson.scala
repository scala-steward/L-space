package lspace.encode

import lspace.NS.types
import lspace.codec.{ActiveContext, ContextedT}
import lspace.librarian.traversal.Collection
import lspace._
import Label.P._
import lspace.codec.json.jsonld.JsonLDEncoder
import lspace.graphql.{Projection, QueryResult}

trait EncodeJson[A] extends Encode[A] {
  def encode(implicit activeContext: ActiveContext): A => String
}

object EncodeJson {

  implicit def contextedTToJson[T](implicit en: EncodeJson[T]) = new EncodeJson[ContextedT[T]] {
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

  implicit def nodeToJson[Json, T <: Node](implicit encoder: JsonLDEncoder[Json]): EncodeJson[T] = new EncodeJson[T] {
    import encoder.baseEncoder._
    def encode(implicit activeContext: ActiveContext) =
      (node: T) => _nodeToJsonMap(node).noSpaces
  }

  implicit def nodesToJson[T, Json](implicit encoder: JsonLDEncoder[Json]): EncodeJson[List[T]] =
    new EncodeJson[List[T]] {
      import encoder.baseEncoder._
      def encode(implicit activeContext: ActiveContext): List[T] => String =
        (nodes: List[T]) =>
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

  implicit def collectionToJson[T, CT <: ClassType[_], Json](
      implicit encoder: JsonLDEncoder[Json]): EncodeJson[Collection[T, CT]] =
    new EncodeJson[Collection[T, CT]] {
      import encoder.baseEncoder._
      def encode(implicit activeContext: ActiveContext): Collection[T, CT] => String =
        (collection: Collection[T, CT]) => encoder.fromAny(collection.item, collection.ct).json.noSpaces
    }

  implicit def activeContextToJson[Json](implicit encoder: JsonLDEncoder[Json]): EncodeJson[ActiveContext] = {
    import encoder.baseEncoder._

    new EncodeJson[ActiveContext] {
      def encode(implicit activeContext: ActiveContext): ActiveContext => String =
        (activeContext: ActiveContext) =>
          Map(
            types.`@context` -> encoder
              .fromActiveContext(activeContext)
              .getOrElse(Map[String, Json]().asJson)).asJson.noSpaces
    }
  }

  implicit val encodeJsonJson: EncodeJson[String] = new EncodeJson[String] {
    def encode(implicit activeContext: ActiveContext) = (string: String) => string
  }
  implicit val encodeBooleanJson: EncodeJson[Boolean] = new EncodeJson[Boolean] {
    def encode(implicit activeContext: ActiveContext) = (value: Boolean) => value.toString
  }
  implicit val encodeIntJson: EncodeJson[Int] = new EncodeJson[Int] {
    def encode(implicit activeContext: ActiveContext) = (value: Int) => value.toString
  }
  implicit val encodeDoubleJson: EncodeJson[Double] = new EncodeJson[Double] {
    def encode(implicit activeContext: ActiveContext) = (value: Double) => value.toString
  }
  implicit val encodeLongJson: EncodeJson[Long] = new EncodeJson[Long] {
    def encode(implicit activeContext: ActiveContext) = (value: Long) => value.toString
  }

  private def namedProjection[Json](projection: Projection, result: List[Any])(
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
            Some((projection.projections zip result.productIterator.toList) flatMap {
              case (projection, result: List[Any]) =>
                namedProjection(projection, result).map(v => projection.alias -> v)
            } toMap).map(_.asJson)
        }
    }
  }

  private def _queryresultToJsonMap[Json](queryResult: QueryResult)(implicit encoder: JsonLDEncoder[Json],
                                                                    activeContext: ActiveContext): Json = {
    import encoder.baseEncoder._

    queryResult.result match {
      case Nil => List[Json]().asJson
      case result =>
        (result.map {
          case result: List[Any] =>
            ((queryResult.query.projections zip result.productIterator.toList) flatMap {
              case (projection, result: List[Any]) =>
                namedProjection(projection, result).map(projection.alias -> _)
              case (projection, result: List[Any]) =>
                namedProjection(projection, result).map(projection.alias -> _)
            } toMap).asJson
          case result: Product =>
            ((queryResult.query.projections zip result.productIterator.toList) flatMap {
              case (projection, result: List[Any]) =>
                namedProjection(projection, result).map(projection.alias -> _)
              case (projection, result: List[Any]) =>
                namedProjection(projection, result).map(projection.alias -> _)
            } toMap).asJson
        }).asJson

      case _ => throw new Exception("XXX")
    }
  }

  implicit def queryResultToJson[T <: QueryResult, Json](implicit encoder: JsonLDEncoder[Json]): EncodeJson[T] =
    new EncodeJson[T] {
      import encoder.baseEncoder._
      def encode(implicit activeContext: ActiveContext) =
        (node: T) => _queryresultToJsonMap(node).asInstanceOf[Json].noSpaces
    }
}
