package lspace.decode

import java.util.UUID

import lspace._
import lspace.codec.ActiveContext
import lspace.codec.graphql.Decoder
import lspace.graphql.{Projection, Query}
import monix.eval.Task

trait DecodeGraphQL[A, F[_]] extends Decode[A, F] {
  type In = String
}

object DecodeGraphQL {
  type Aux[Out, F[_], In0] = DecodeGraphQL[Out, F] { type In = In0 }

  def graphqlToQuery(allowedProperties: List[Property] = List(), forbiddenProperties: List[Property] = List())(
      implicit decoder: Decoder): DecodeGraphQL[Query, Task] = {

    lazy val validProperty: Property => Boolean =
      if (allowedProperties.nonEmpty) { (property: Property) =>
        allowedProperties.contains(property) && !forbiddenProperties.contains(property)
      } else if (forbiddenProperties.nonEmpty) { (property: Property) =>
        !forbiddenProperties.contains(property)
      } else { (property: Property) =>
        true
      }

    lazy val validProjection: Projection => Boolean = (projection: Projection) =>
      validProperty(projection.property) && (projection.projections.isEmpty || projection.projections.exists(
        _.projections.forall(projection => validProjection(projection))))

    lazy val validQuery: Query => Boolean = (query: Query) => query.projections.forall(validProjection)

    if (allowedProperties.nonEmpty || forbiddenProperties.nonEmpty) {
      new DecodeGraphQL[Query, Task] {
        def decode(implicit activeContext: ActiveContext) = { (graphql: String) =>
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
      new DecodeGraphQL[Query, Task] {
        def decode(implicit activeContext: ActiveContext) = { (graphql: String) =>
          decoder
            .toGraphQL(graphql) match {
            case query: Query => Task.now(query)
            case _            => Task.raiseError(new Exception("not a graphql query"))
          }
        }
      }
  }
}
