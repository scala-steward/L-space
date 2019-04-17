package lspace.services.rest.endpoints

import cats.effect.IO
import io.finch.Endpoint
import lspace.librarian.traversal.Collection
import lspace.structure.{ClassType, Node}

trait SparqlApi extends ExecutionApi {
  def query: Endpoint[IO, Collection[Any, ClassType[Any]]] = ???
  //    {
  //      import io.finch.internal.HttpContent
  //      implicit val decoder = Decoder(DetachedGraph)
  //      implicit val d1 = io.finch.Decode
  //        .instance[lspace.sparql.Select, lspace.services.codecs.Application.SPARQL] { (b, cs) =>
  //          Right(
  //            DecodeJsonLD.jsonldToTraversal
  //              .decode(b.asString(cs)))
  //        }
  //      get(body[lspace.sparql.Select, lspace.services.codecs.Application.SPARQL]) {
  //        traversalTask: Task[lspace.sparql.Select] =>
  //          traversalTask.flatMap { traversal =>
  //            traversal.untyped
  //              .withGraph(graph)
  //              .toListF
  //              .map(_.collect { case node: Node if node.hasLabel(ontology).isDefined => node })
  //              .map(_.toList)
  //              .map(Ok)
  //          }.toIO
  //      }
  //    }
}
