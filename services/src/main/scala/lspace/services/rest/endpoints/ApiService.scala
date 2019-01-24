package lspace.services.rest.endpoints

import cats.effect.IO
import io.finch._
import lspace.librarian.process.traversal.Traversal
import lspace.librarian.structure._
import lspace.parse.JsonLD
import org.slf4j.LoggerFactory
import shapeless.HList

case class PagedResult(result: List[Node])

case class ApiService(ontology: Ontology)(implicit graph: Graph) extends JsonLDModule {
  val log             = LoggerFactory.getLogger(getClass)
  implicit val jsonld = JsonLD(graph) //todo JsonLD context per client-session

  implicit val ec = monix.execution.Scheduler.global
  val label       = ontology.label.getOrElse("en", throw new Exception("no label found")).toLowerCase()

  /**
    * GET /{id}
    */
  val getById: Endpoint[IO, Node] = get(path[Int]).mapOutputAsync { id =>
    graph.g.N
      .hasIri(graph.iri + "/" + label + "/" + id)
      .hasLabel(ontology)
      .toAsyncStream
      .map(_.headOption.map(Ok).getOrElse(NotFound(new Exception("Resource not found"))))
      .toIO
  }

  /**
    * GET /
    */
  val getByQuery: Endpoint[IO, PagedResult] = get(zero) {
    Ok(
      graph.g.N
        .hasLabel(ontology)
        .order(_.id)
        .range(0, 20)
        .toList).map(PagedResult)
  }

  /**
    * GET /
    * BODY ld+json: https://ns.l-space.eu/librarian/Traversal
    */
  val getByLibrarian: Endpoint[IO, PagedResult] = get(bodyJsonLDTraversal).mapOutputAsync {
    traversal: Traversal[ClassType[Any], ClassType[Any], HList] =>
      traversal.toUntypedStreamTask
        .map(_.collect { case node: Node if node.hasLabel(ontology).isDefined => node })
        .map(_.toList)
        .map(PagedResult(_))
        .map(Ok)
        .toIO
  }

//  val getBySPARQL: Endpoint[IO, PagedResult]

//  val postNode: Endpoint[IO, Node] = post(zero :: jsonldToNode).mapOutputAsync { node: Node =>
//    Task {
//      graph.nodes.post(node)
//    }.map(Created(_)).toIO
//  }

  def api = getById :+: getByQuery :+: getByLibrarian

}
