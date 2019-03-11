package lspace.services.rest.endpoints

import cats.effect.IO
import com.twitter.finagle.http.{Request, Response}
import io.finch._
import lspace.structure._
import lspace.structure.Property.default._
import monix.eval.Task
import shapeless.{:+:, CNil, HList, Poly1}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string._
import eu.timepit.refined.numeric._
import io.finch.refined._
import lspace.codec.{Decoder, NativeTypeDecoder}
import lspace.decode.{DecodeJson, DecodeJsonLD}
import lspace.provider.detached.DetachedGraph

object LabeledNodeApi {
  def apply(ontology: Ontology)(implicit graph: Lspace, baseDecoder: NativeTypeDecoder): LabeledNodeApi =
    new LabeledNodeApi(ontology)
}

class LabeledNodeApi(
    val ontology: Ontology,
    additionalProperties: List[Property] = List(),
    forbiddenProperties: List[Property] = List())(implicit val graph: Graph, val baseDecoder: NativeTypeDecoder)
    extends Api {
//  implicit val encoder = Encoder //todo Encode context per client-session
//  implicit val decoder
//    : lspace.codec.Decoder[Any] = Decoder(graph).asInstanceOf[lspace.codec.Decoder[Any]] //todo Decode context per client-session

  type Json = baseDecoder.Json
  implicit val bd: NativeTypeDecoder.Aux[Json] = baseDecoder.asInstanceOf[NativeTypeDecoder.Aux[Json]]
  //  import monix.eval.Task.catsEffect
  //  import monix.execution.Scheduler.global

  implicit val ec = monix.execution.Scheduler.global
  val label       = ontology.label("en").getOrElse(throw new Exception("no label found")).toLowerCase()

  import lspace._
  import Implicits.AsyncGuide._

  val idToNodeTask: Long => Task[Option[Node]] = (id: Long) =>
    g.N
      .hasIri(graph.iri + "/" + label + "/" + id)
      .hasLabel(ontology)
      .withGraph(graph)
      .headOptionF
  val iriToNodeTask: String => Task[Option[Node]] = (iri: String) =>
    g.N
      .hasIri(iri)
      .hasLabel(ontology)
      .withGraph(graph)
      .headOptionF

  def byIri: Endpoint[IO, Node] = get(path[String]).mapOutputAsync { (iri: String) =>
    iriToNodeTask(iri).map(_.map(Ok).getOrElse(NotFound(new Exception("Resource not found")))).toIO
  }

  /**
    * GET /{id}
    */
  def byId: Endpoint[IO, Node] = get(path[Long]).mapOutputAsync { id =>
    idToNodeTask(id).map(_.map(Ok).getOrElse(NotFound(new Exception("Resource not found")))).toIO
  }

//  def list: Endpoint[IO, PagedResult] = get(param[Int Refined Positive]("low") :: param[Int Refined Positive]("high")) {
//    (low: Int Refined Positive, high: Int Refined Positive) =>
//      graph.g.N.hasLabel(ontology).range(low.value, high.value).toObservable.toListL.map(PagedResult)
//  }
  /**
    * GET /
    */
  def list: Endpoint[IO, List[Node]] = get(zero).mapOutputAsync { hn =>
    g.N.hasLabel(ontology).withGraph(graph).toListF.map(Ok).toIO
  }

//  val create2: Endpoint[IO, Node] =
//    post(bodyJson(ontology.properties.toList)).mapOutputAsync { node: Node =>
//      Task {
//        val t       = graph.transaction
//        val newNode = t.nodes.create(ontology)
//        newNode --- `@id` --> (graph.iri + "/" + label + "/" + newNode.id)
//        node --- `@id` --> (graph.iri + "/" + label + "/" + newNode.id)
//        newNode.addLabel(ontology)
//        t ++ node.graph
//        t.commit()
//        Created(graph.nodes.hasId(newNode.id).get)
//      }.onErrorHandle { f =>
//        f.printStackTrace(); throw f
//      }.toIO
//    }

  def create: Endpoint[IO, Node] = {
    import io.finch.internal.HttpContent
    implicit val decoder = Decoder(DetachedGraph)
    implicit val d1 = io.finch.Decode.instance[Task[Node], lspace.services.codecs.Application.JsonLD] { (b, cs) =>
      Right(
        DecodeJsonLD
          .jsonldToLabeledNode(ontology, ontology.properties().toList, additionalProperties, forbiddenProperties)
          .decode(b.asString(cs)))
    }
    implicit val d2 = io.finch.Decode.instance[Task[Node], Application.Json] { (b, cs) =>
      Right(
        DecodeJson
          .jsonToLabeledNode(ontology, ontology.properties().toList, additionalProperties, forbiddenProperties)
          .decode(b.asString(cs)))
    }
    post(body[Task[Node], lspace.services.codecs.Application.JsonLD :+: Application.Json :+: CNil]) {
      nodeTask: Task[Node] =>
        nodeTask.map { node =>
          val t       = graph.transaction
          val newNode = t.nodes.create(ontology)
          newNode --- `@id` --> (graph.iri + "/" + label + "/" + newNode.id)
          node --- `@id` --> (graph.iri + "/" + label + "/" + newNode.id)
          newNode.addLabel(ontology)
          t ++ node.graph
          t.commit()
          Created(graph.nodes.hasId(newNode.id).get)
        }.toIO //(catsEffect(global))
    }
  }

  def replaceById: Endpoint[IO, Node] = {
    import io.finch.internal.HttpContent
    implicit val decoder = Decoder(DetachedGraph)
    implicit val d1 = io.finch.Decode.instance[Task[Node], lspace.services.codecs.Application.JsonLD] { (b, cs) =>
      Right(
        DecodeJsonLD
          .jsonldToLabeledNode(ontology, ontology.properties().toList, additionalProperties, forbiddenProperties)
          .decode(b.asString(cs)))
    }
    implicit val d2 = io.finch.Decode.instance[Task[Node], Application.Json] { (b, cs) =>
      Right(
        DecodeJson
          .jsonToLabeledNode(ontology, ontology.properties().toList, additionalProperties, forbiddenProperties)
          .decode(b.asString(cs)))
    }
    put(path[Long] :: body[Task[Node], lspace.services.codecs.Application.JsonLD :+: Application.Json :+: CNil]) {
      (id: Long, nodeTask: Task[Node]) => //TODO: validate before mutating
        nodeTask.flatMap { node =>
          val t = graph.transaction
          t.nodes
            .hasIri(graph.iri + "/" + label + "/" + id)
            .headOption //TODO: handle 'unexpected' multiple results
            .map { existingNode => //TODO: validate if node is without @id or @id is equal to ```graph.iri + "/" + label + "/" + id```
              Task {
                node.outE(`@id`).foreach(_.remove())
                node --- `@id` --> (graph.iri + "/" + label + "/" + id)
                existingNode.outE().filterNot(e => e.key == `@id` || e.key == `@ids`).foreach(_.remove())
                t ++ node.graph
                t.commit()
                Ok(graph.nodes.hasId(existingNode.id).get)
              }
            }
            .getOrElse(Task(NotFound(new Exception("cannot PUT a resource which does not exist"))))
        }.toIO
    }
  }

  def updateById: Endpoint[IO, Node] = {
    import io.finch.internal.HttpContent
    implicit val decoder = Decoder(DetachedGraph)
    implicit val d1 = io.finch.Decode.instance[Task[Node], lspace.services.codecs.Application.JsonLD] { (b, cs) =>
      Right(
        DecodeJsonLD
          .jsonldToLabeledNode(ontology, ontology.properties().toList, additionalProperties, forbiddenProperties)
          .decode(b.asString(cs)))
    }
    implicit val d2 = io.finch.Decode.instance[Task[Node], Application.Json] { (b, cs) =>
      Right(
        DecodeJson
          .jsonToLabeledNode(ontology, ontology.properties().toList, additionalProperties, forbiddenProperties)
          .decode(b.asString(cs)))
    }
    patch(path[Long] :: body[Task[Node], lspace.services.codecs.Application.JsonLD :+: Application.Json :+: CNil]) {
      (id: Long, nodeTask: Task[Node]) =>
        nodeTask.flatMap { node =>
          val t = graph.transaction
          t.nodes
            .hasIri(graph.iri + "/" + label + "/" + id)
            .headOption //TODO: handle 'unexpected' multiple results
            .map { existingNode => //TODO: validate if node is without @id or @id is equal to ```graph.iri + "/" + label + "/" + id```
              Task {
                node.outE(`@id`).foreach(_.remove())
                node --- `@id` --> (graph.iri + "/" + label + "/" + id)
                //          existingNode.outE(node.outEMap().keys.toList: _*).foreach(_.remove())
                node.outEMap().keys.filterNot(_ == `@id`).foreach(existingNode.removeOut)
                t ++ node.graph
                t.commit()
                Ok(graph.nodes.hasId(existingNode.id).get)
              }
            }
            .getOrElse(Task(NotFound(new Exception("cannot PATCH a resource which does not exist"))))
        }.toIO
    }
  }
  def removeById: Endpoint[IO, Node] = delete(path[Long]) { id: Long =>
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
  def getByLibrarian: Endpoint[IO, List[Node]] = {
    import io.finch.internal.HttpContent
    implicit val decoder = Decoder(DetachedGraph)
    implicit val d1 = io.finch.Decode
      .instance[Task[Traversal[ClassType[Any], ClassType[Any], HList]], lspace.services.codecs.Application.JsonLD] {
        (b, cs) =>
          Right(
            DecodeJsonLD.jsonldToTraversal
              .decode(b.asString(cs)))
      }
    get(body[Task[Traversal[ClassType[Any], ClassType[Any], HList]], lspace.services.codecs.Application.JsonLD]) {
      traversalTask: Task[Traversal[ClassType[Any], ClassType[Any], HList]] =>
        traversalTask.flatMap { traversal =>
          traversal.untyped
            .withGraph(graph)
            .toListF
            .map(_.collect { case node: Node if node.hasLabel(ontology).isDefined => node })
            .map(_.toList)
            .map(Ok)
        }.toIO
    }
  }

//  val getBySPARQL: Endpoint[IO, PagedResult]

//  val postNode: Endpoint[IO, Node] = post(zero :: jsonldToNode).mapOutputAsync { node: Node =>
//    Task {
//      graph.nodes.post(node)
//    }.map(Created(_)).toIO
//  }

  def api =
    byId :+: /*byIri :+:*/ list :+:
      create :+: replaceById :+: updateById :+: removeById :+: getByLibrarian
  def labeledApi = label :: api
}
