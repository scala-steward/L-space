package lspace.decode

import java.util.UUID

import lspace._
import lspace.codec.ActiveContext
import lspace.codec.graphql.Decoder
import lspace.graphql.{Projection, Query}
import monix.eval.Task

trait DecodeGraphQL[A] extends Decode[A] {
  def decode: String => Task[A]
}

object DecodeGraphQL {
  def graphqlToQuery(allowedProperties: List[Property] = List(), forbiddenProperties: List[Property] = List())(
      implicit decoder: Decoder,
      activeContext: ActiveContext) = {

    lazy val validProperty: Property => Boolean = (property: Property) =>
      if (allowedProperties.nonEmpty) {
        allowedProperties.contains(property) && !forbiddenProperties.contains(property)
      } else if (forbiddenProperties.nonEmpty) {
        !forbiddenProperties.contains(property)
      } else {
        true
    }

    lazy val validProjection: Projection => Boolean = (projection: Projection) =>
      validProperty(projection.property) && (projection.query.isEmpty || projection.query.exists(
        _.projections.forall(projection => validProjection(projection))))

    lazy val validQuery: Query => Boolean = (query: Query) => query.projections.forall(validProjection)

    if (allowedProperties.nonEmpty || forbiddenProperties.nonEmpty) {
      new DecodeGraphQL[Query] {
        def decode = { (graphql: String) =>
          decoder
            .toGraphQL(graphql) match {
            case query: Query =>
              if (validQuery(query)) Task.now(query)
              else Task.raiseError(new Exception("query contains certain properties which are not allowed"))
            case _ => Task.raiseError(new Exception("not a graphql query"))
          }
        }
      }
    } else
      new DecodeJson[Query] {
        def decode = { (graphql: String) =>
          decoder
            .toGraphQL(graphql) match {
            case query: Query => Task.now(query)
            case _            => Task.raiseError(new Exception("not a graphql query"))
          }
        }
      }
  }
}
