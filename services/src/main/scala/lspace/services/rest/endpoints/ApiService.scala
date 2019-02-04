package lspace.services.rest.endpoints

import cats.effect.IO
import io.finch._
import lspace.librarian.process.traversal.Traversal
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default._
import lspace.parse.JsonLD
import monix.eval.Task
import shapeless.HList
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string._
import eu.timepit.refined.numeric._
import io.finch.refined._

case class PagedResult(result: List[Node])

case class ApiService(ontology: Ontology)(implicit graph: Graph) extends JsonLDModule {
  implicit val jsonld = JsonLD.detached //todo JsonLD context per client-session

  implicit val ec = monix.execution.Scheduler.global
  val label       = ontology.label.getOrElse("en", throw new Exception("no label found")).toLowerCase()

  val idToNodeTask: Long => Task[Option[Node]] = (id: Long) =>
    graph.g.N
      .hasIri(graph.iri + "/" + label + "/" + id)
      .hasLabel(ontology)
      .toAsyncStream
      .map(_.headOption)
  val iriToNodeTask: String => Task[Option[Node]] = (iri: String) =>
    graph.g.N
      .hasIri(iri)
      .hasLabel(ontology)
      .toAsyncStream
      .map(_.headOption)

  val byIri: Endpoint[IO, Node] = get(path[String]).mapOutputAsync { (iri: String) =>
    iriToNodeTask(iri).map(_.map(Ok).getOrElse(NotFound(new Exception("Resource not found")))).toIO
  }

  /**
    * GET /{id}
    */
  val byId: Endpoint[IO, Node] = get(path[Long]).mapOutputAsync { id =>
    idToNodeTask(id).map(_.map(Ok).getOrElse(NotFound(new Exception("Resource not found")))).toIO
  }

//  def list: Endpoint[IO, PagedResult] = get(param[Int Refined Positive]("low") :: param[Int Refined Positive]("high")) {
//    (low: Int Refined Positive, high: Int Refined Positive) =>
//      graph.g.N.hasLabel(ontology).range(low.value, high.value).toObservable.toListL.map(PagedResult)
//  }
  /**
    * GET /
    */
  val list: Endpoint[IO, PagedResult] = get(zero).mapOutputAsync { hn =>
    graph.g.N.hasLabel(ontology).toObservable.toListL.map(PagedResult).map(Ok).toIO
  }

  val create = post(bodyJsonLD) { node: Node =>
    Task {
      val t       = graph.transaction
      val newNode = t.nodes.create(ontology)
      newNode --- `@id` --> (graph.iri + "/" + label + "/" + newNode.id)
      node --- `@id` --> (graph.iri + "/" + label + "/" + newNode.id)
      newNode.addLabel(ontology)
      t ++ node.graph
      t.commit()
      Created(newNode)
    }.toIO
  }

  val replaceById
    : Endpoint[IO, Node] = put(path[Long] :: bodyJsonLD) { (id: Long, node: Node) => //TODO: validate before mutating
    val t = graph //.transaction
    t.nodes
      .hasIri(graph.iri + "/" + label + "/" + id)
      .headOption //TODO: handle 'unexpected' multiple results
      .map { existingNode => //TODO: validate if node is without @id or @id is equal to ```graph.iri + "/" + label + "/" + id```
        Task {
          existingNode.outE().filterNot(e => e.key == `@id` || e.key == `@ids`).foreach(_.remove())
          node.outE(`@id`).foreach(_.remove())
          node --- `@id` --> (graph.iri + "/" + label + "/" + id)
          println("replace with " + node.out())
          try {
            t ++ node.graph
            //t.commit()
          } catch {
            case e =>
              println(e.getMessage)
              throw e
          }
          println("replaced with " + existingNode.out())
          Ok(existingNode)
        }.toIO
      }
      .getOrElse(Task(NotFound(new Exception("cannot PUT a resource which does not exist"))).toIO)
  }

  val updateById: Endpoint[IO, Node] = patch(path[Long] :: bodyJsonLD) { (id: Long, node: Node) =>
    val t = graph //.transaction
    t.nodes
      .hasIri(graph.iri + "/" + label + "/" + id)
      .headOption //TODO: handle 'unexpected' multiple results
      .map { existingNode => //TODO: validate if node is without @id or @id is equal to ```graph.iri + "/" + label + "/" + id```
        Task {
          node.outE(`@id`).foreach(_.remove())
//          existingNode.outE(node.outEMap().keys.toList: _*).foreach(_.remove())
          node.outEMap().keys.foreach(existingNode.removeOut)
          node --- `@id` --> (graph.iri + "/" + label + "/" + id)
          t ++ node.graph
          //  t.commit()
          Ok(existingNode)
        }.toIO
      }
      .getOrElse(Task(NotFound(new Exception("cannot PATCH a resource which does not exist"))).toIO)
  }
  val removeById: Endpoint[IO, Node] = delete(path[Long]) { id: Long =>
    val t = graph.transaction
    t.nodes
      .hasIri(graph.iri + "/" + label + "/" + id)
      .headOption //TODO: handle 'unexpected' multiple results
      .map { node =>
        Task {
          node.remove() //TODO: decide if values or blank nodes need to be explicitly removed
          NoContent[Node]
        }.toIO
      }
      .getOrElse(Task(NotFound(new Exception("cannot DELETE a resource which does not exist"))).toIO)
  }

  /**
    * GET /
    * BODY ld+json: https://ns.l-space.eu/librarian/Traversal
    */
  val getByLibrarian: Endpoint[IO, PagedResult] =
    get(bodyJsonLDTyped(Traversal.ontology, node => Traversal.toTraversal(node)(graph))).mapOutputAsync {
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

  def api = byId :+: /*byIri :+:*/ list :+: create :+: replaceById :+: updateById :+: removeById :+: getByLibrarian

}
