package lspace.encode

import lspace.NS.types
import lspace.codec.{ActiveContext, ContextedT}
import lspace.codec.jsonld.Encoder
import lspace.librarian.traversal.Collection
import lspace._
import Label.P._
import lspace.graphql.{Projection, QueryResult}

trait EncodeJson[A] extends Encode[A] {
  def encode(implicit activeContext: ActiveContext): A => String
}

object EncodeJson {

  implicit def contextedTToJsonLD[T](implicit en: EncodeJson[T]) = new EncodeJson[ContextedT[T]] {
    def encode(implicit activeContext: ActiveContext) =
      (ct: ContextedT[T]) => en.encode(activeContext ++ ct.activeContext)(ct.t)
  }

  private def _nodeToJsonMap(resource: Resource[_])(implicit encoder: Encoder,
                                                    activeContext: ActiveContext): encoder.Json = {
    import encoder._
    encoder.mapToJson(
      List(
        Some(resource.iri).filter(_.nonEmpty).map(_.asJson).map(`@id`.iri -> _),
        Some(resource.labels)
          .filter(_.nonEmpty)
          .map {
            case List(label) => activeContext.compactIri(label.iri).asJson
            case labels      => labels.map(_.iri).map(activeContext.compactIri(_).asJson).asJson
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

  implicit def nodesToJson[T](implicit encoder: Encoder) =
    new EncodeJson[List[T]] {
      import encoder._
      def encode(implicit activeContext: ActiveContext): List[T] => String =
        (nodes: List[T]) =>
          encoder
            .listToJson(nodes.map {
              case node: Node       => _nodeToJsonMap(node).asInstanceOf[encoder.Json]
              case edge: Edge[_, _] => _nodeToJsonMap(edge).asInstanceOf[encoder.Json]
              case value: Value[_]  => _nodeToJsonMap(value).asInstanceOf[encoder.Json]
              case value            => encoder.fromAny(value).json
            })
            .noSpaces
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
          Map(
            types.`@context` -> encoder
              .fromActiveContext(activeContext)
              .getOrElse(Map[String, encoder.Json]().asJson)).asJson.noSpaces
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

  private def namedProjection(projection: Projection, result: List[Any])(
      implicit encoder: Encoder,
      activeContext: ActiveContext): Option[encoder.Json] = {
    import encoder._

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
                Some(encoder.listToJson(values.map(value =>
                  encoder.fromAny(value, activeContext.expectedType(expKey)).json)))
            }
          case projections =>
            Some((projection.projections zip result.productIterator.toList) flatMap {
              case (projection, result: List[Any]) => namedProjection(projection, result).map(projection.alias -> _)
            } toMap).map(encoder.mapToJson(_))
        }
    }
  }

  private def _queryresultToJsonMap(queryResult: QueryResult)(implicit encoder: Encoder,
                                                              activeContext: ActiveContext): encoder.Json = {
    queryResult.result match {
      case Nil => encoder.listToJson(List())
      case result =>
        encoder.mapToJson((queryResult.query.projections zip result.productIterator.toList) flatMap {
          case (projection, result: List[Any]) => namedProjection(projection, result).map(projection.alias -> _)
        } toMap)
    }
  }

  implicit def queryResultToJson[T <: QueryResult](implicit encoder: Encoder) = new EncodeJson[T] {
    import encoder._
    def encode(implicit activeContext: ActiveContext) =
      (node: T) => _queryresultToJsonMap(node).asInstanceOf[encoder.Json].noSpaces
  }
}
