package lspace.services.rest.endpoints

import cats.effect.IO
import io.finch.{Endpoint, Ok}
import lspace.codec.{ActiveContext, ContextedT}
import lspace.codec.json.jsonld.JsonLDDecoder
import lspace.decode.DecodeGraphQL
import lspace.{codec, g}
import lspace.graphql.{Query, QueryResult}
import lspace.librarian.task.AsyncGuide
import lspace.services.rest.endpoints.util.MatchHeader
import lspace.structure.{Graph, Node, Ontology}
import monix.eval.Task
import monix.execution.Scheduler
import shapeless.{:+:, CNil}

object GraphqlApi {
  def apply[JSON](graph: Graph)(implicit activeContext: ActiveContext = ActiveContext(),
                                decoder: codec.graphql.Decoder,
                                guide: AsyncGuide,
                                scheduler: Scheduler): GraphqlApi =
    new GraphqlApi(graph)(activeContext, decoder, guide, scheduler)
}
class GraphqlApi(graph: Graph)(implicit val activeContext: ActiveContext,
                               decoder: codec.graphql.Decoder,
                               guide: AsyncGuide,
                               scheduler: Scheduler)
    extends Api {

  import lspace.services.codecs.Decode._
  implicit val graphqlToNode = DecodeGraphQL
    .graphqlToQuery()

  def context /*: Endpoint[IO, ActiveContext]*/ =
    get("@context") {
      Ok(activeContext)
    } :+: get("context") {
      Ok(activeContext)
    }

  /**
    *
    * BODY graphql
    */
  def list(ontology: Ontology): Endpoint[IO, ContextedT[QueryResult] :+: ContextedT[QueryResult] :+: CNil] = {
    get(param("query").map(decoder.toGraphQL(_)))
      .mapOutputAsync {
        case query: Query =>
          Task
            .now(query)
            .flatMap { query =>
              (g.N.hasLabel(ontology).untyped ++ query.toTraversal.untyped)
                .withGraph(graph)
                .toListF
                .map(result => QueryResult(query, result))
                .map(ContextedT(_))
                .map(Ok)
            }
            .to[IO]
      }
  } :+: {
    MatchHeader.beGraphQL :: post(body[Task[Query], lspace.services.codecs.Application.GraphQL])
      .mapOutputAsync {
        case queryTask: Task[Query] =>
          queryTask
            .flatMap { query =>
              (g.N.hasLabel(ontology).untyped ++ query.toTraversal.untyped)
                .withGraph(graph)
                .toListF
                .map(result => QueryResult(query, result))
                .map(ContextedT(_))
                .map(Ok)
            }
            .to[IO]
      }
  }
}
