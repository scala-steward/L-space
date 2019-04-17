package lspace.services.rest.endpoints

import cats.effect.IO
import io.finch._
import lspace._
import lspace.Label.D._
import lspace.Label.P._
import lspace.codec.{jsonld, ActiveContext, NativeTypeDecoder, NativeTypeEncoder}
import lspace.decode.{DecodeJson, DecodeJsonLD}
import lspace.encode.{EncodeJson, EncodeJsonLD}
import lspace.provider.detached.DetachedGraph
import lspace.services.LApplication
import monix.eval.Task
import shapeless.{:+:, CNil, HList}

object LabeledNodeApi {
  def apply(graph: Graph, ontology: Ontology, defaultContext: ActiveContext = ActiveContext())(
      implicit
      baseDecoder: NativeTypeDecoder,
      baseEncoder: NativeTypeEncoder): LabeledNodeApi =
    new LabeledNodeApi(ontology)(graph, baseDecoder, baseEncoder, defaultContext)
}

class LabeledNodeApi(val ontology: Ontology,
                     allowedProperties: List[Property] = List(),
                     forbiddenProperties: List[Property] = List())(implicit val graph: Graph,
                                                                   val baseDecoder: NativeTypeDecoder,
                                                                   val baseEncoder: NativeTypeEncoder,
                                                                   activeContext: ActiveContext)
    extends Api {
//  implicit val encoder = Encoder //todo Encode context per client-session
//  implicit val decoder
//    : lspace.codec.jsonld.Decoder[Any] = Decoder(graph).asInstanceOf[lspace.codec.jsonld.Decoder[Any]] //todo Decode context per client-session

  type Json = baseDecoder.Json
  implicit val bd: NativeTypeDecoder.Aux[Json] = baseDecoder.asInstanceOf[NativeTypeDecoder.Aux[Json]]

  import lspace.services.codecs.Decode._
  implicit val decoder = jsonld.Decoder(DetachedGraph)

  implicit val jsonldToNode = DecodeJsonLD
    .jsonldToLabeledNode(ontology, allowedProperties, forbiddenProperties)
  implicit val jsonToNode = DecodeJson
    .jsonToLabeledNode(ontology, allowedProperties, forbiddenProperties)
  import DecodeJson._
  import DecodeJsonLD._
  import lspace.Implicits.Scheduler.global
  val label = ontology.iri.reverse.takeWhile(_ != '/').reverse.toLowerCase() //label("en").getOrElse(throw new Exception("no label found")).toLowerCase()

  import lspace._
  import Implicits.AsyncGuide._

  def context: Endpoint[IO, ActiveContext] = get("context") {
    Ok(activeContext)
  }

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

  /**
    * GET /{id}
    */
  def byId: Endpoint[IO, Node] = get(path[Long]).mapOutputAsync { id =>
    idToNodeTask(id).map(_.map(Ok).getOrElse(NotFound(new Exception("Resource not found")))).toIO
  }

  def byIri: Endpoint[IO, Node] = get(path[String]).mapOutputAsync { (iri: String) =>
    iriToNodeTask(iri).map(_.map(Ok).getOrElse(NotFound(new Exception("Resource not found")))).toIO
  }

  /**
    * GET /
    */
  def list: Endpoint[IO, List[Node]] = get(zero).mapOutputAsync { hn =>
    g.N.hasLabel(ontology).withGraph(graph).toListF.map(Ok).toIO
  }

  def create: Endpoint[IO, Node] = {
    post(body[Task[Node], lspace.services.codecs.Application.JsonLD :+: Application.Json :+: CNil]) {
      nodeTask: Task[Node] =>
        nodeTask.flatMap { node =>
          val t = graph.transaction
          for {
            newNode <- t.nodes.create(node.labels: _*)
            _       <- newNode --- `@id` --> (graph.iri + "/" + label + "/" + newNode.id)
            _       <- newNode.addLabel(ontology)
            _       <- Task.sequence(node.outE().map(e => newNode --- e.key --> e.to))
            _       <- t.commit()
            r       <- graph.nodes.hasId(newNode.id)
          } yield Created(r.get)
        }.toIO //(catsEffect(global))
    }
  }

  def replaceById: Endpoint[IO, Node] = {
    implicit val jsonldToNode = DecodeJsonLD
      .jsonldToLabeledNode(ontology, allowedProperties, forbiddenProperties)
    implicit val jsonToNode = DecodeJson
      .jsonToLabeledNode(ontology, allowedProperties, forbiddenProperties)

    put(path[Long] :: body[Task[Node], lspace.services.codecs.Application.JsonLD :+: Application.Json :+: CNil]) {
      (id: Long, nodeTask: Task[Node]) => //TODO: validate before mutating
        nodeTask
          .flatMap { node =>
            val t = graph.transaction
            for {
              existingNode <- t.nodes //TODO: validate if node is without @id or @id is equal to ```graph.iri + "/" + label + "/" + id```
                .hasIri(graph.iri + "/" + label + "/" + id)
                .headL
              _ <- Task.sequence(node.outE(`@id`).map(_.remove()))
              _ <- Task.sequence(existingNode.outE().filterNot(e => e.key == `@id` || e.key == `@ids`).map(_.remove()))
              _ <- Task.sequence(node.outE().map(e => existingNode --- e.key --> e.to))
              _ <- t.commit()
              r <- graph.nodes.hasId(existingNode.id)
            } yield Ok(r.get)
          }
          .onErrorHandle(f => NotFound(new Exception("cannot PUT a resource which does not exist"))) //TODO: handle 'unexpected' multiple results
          .toIO
    }
  }

  def updateById: Endpoint[IO, Node] = {
    implicit val jsonldToNode = DecodeJsonLD
      .jsonldToNode(allowedProperties, forbiddenProperties)
    implicit val jsonToNode = DecodeJson
      .jsonToNode(allowedProperties, forbiddenProperties)

    patch(path[Long] :: body[Task[Node], lspace.services.codecs.Application.JsonLD :+: Application.Json :+: CNil]) {
      (id: Long, nodeTask: Task[Node]) =>
        nodeTask
          .flatMap { node =>
            val t = graph.transaction
            for {
              existingNode <- t.nodes //TODO: validate if node is without @id or @id is equal to ```graph.iri + "/" + label + "/" + id```
                .hasIri(graph.iri + "/" + label + "/" + id)
                .headL
              _ <- Task.sequence(node.outE(`@id`).map(_.remove()))
              _ <- Task.sequence(
                existingNode
                  .outE()
                  .filterNot(e => e.key == `@id` || e.key == `@ids`)
                  .filter(e => node.outMap().keySet.contains(e.key))
                  .map(_.remove()))
              _ <- Task.sequence(node.outE().map(e => existingNode --- e.key --> e.to))
              _ <- t.commit()
              r <- graph.nodes.hasId(existingNode.id)
            } yield Ok(r.get)
          }
          .onErrorHandle(f => NotFound(new Exception("cannot PATCH a resource which does not exist")))
          .toIO
    }
  }
  def removeById: Endpoint[IO, Node] = delete(path[Long]) { id: Long =>
    val t = graph.transaction
    (for {
      node <- t.nodes
        .hasIri(graph.iri + "/" + label + "/" + id)
        .headL //TODO: handle 'unexpected' multiple results
      _ <- node.remove() //TODO: decide if values or blank nodes need to be explicitly removed
    } yield
      NoContent[Node]).onErrorHandle(f => NotFound(new Exception("cannot DELETE a resource which does not exist"))).toIO
  }

  /**
    * GET /
    * BODY ld+json: https://ns.l-space.eu/librarian/Traversal
    */
  def getByLibrarian: Endpoint[IO, List[Node]] = {
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
    context :+: byId :+: byIri :+: list :+:
      create :+: replaceById :+: updateById :+: removeById :+: getByLibrarian
  def labeledApi = label :: api

  def compiled = {
    type Json = baseEncoder.Json
    implicit val be: NativeTypeEncoder.Aux[Json] = baseEncoder.asInstanceOf[NativeTypeEncoder.Aux[Json]]

    import lspace.services.codecs.Encode._
    implicit val encoder = jsonld.Encoder.apply(be)

    import EncodeJson._
    import EncodeJsonLD._

    Bootstrap
      .configure(enableMethodNotAllowed = true, enableUnsupportedMediaType = true)
      .serve[LApplication.JsonLD :+: Application.Json :+: CNil](api)
      .compile
  }
}
