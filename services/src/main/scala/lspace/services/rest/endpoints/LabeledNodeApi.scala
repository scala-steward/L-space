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
    allowedProperties: List[Property] = List(),
    forbiddenProperties: List[Property] = List())(implicit val graph: Graph, val baseDecoder: NativeTypeDecoder)
    extends Api {
//  implicit val encoder = Encoder //todo Encode context per client-session
//  implicit val decoder
//    : lspace.codec.Decoder[Any] = Decoder(graph).asInstanceOf[lspace.codec.Decoder[Any]] //todo Decode context per client-session

  type Json = baseDecoder.Json
  implicit val bd: NativeTypeDecoder.Aux[Json] = baseDecoder.asInstanceOf[NativeTypeDecoder.Aux[Json]]
  //  import monix.eval.Task.catsEffect
  //  import monix.execution.Scheduler.global

  import lspace.Implicits.Scheduler.global
  val label = ontology.iri.reverse.takeWhile(_ != '/').reverse.toLowerCase() //label("en").getOrElse(throw new Exception("no label found")).toLowerCase()

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

  /**
    * GET /
    */
  def list: Endpoint[IO, List[Node]] = get(zero).mapOutputAsync { hn =>
    g.N.hasLabel(ontology).withGraph(graph).toListF.map(Ok).toIO
  }

  def create: Endpoint[IO, Node] = {
    import io.finch.internal.HttpContent
    import lspace.services.codecs.Decode._
    implicit val decoder = Decoder(DetachedGraph)
//    implicit val d1 = io.finch.Decode.instance[Task[Node], lspace.services.codecs.Application.JsonLD] { (b, cs) =>
//      Right(
    implicit val jsonldToLabeledNode = DecodeJsonLD
      .jsonldToLabeledNode(ontology, allowedProperties, forbiddenProperties)
//          .decode(b.asString(cs)))
//    }
//    implicit val d2 = io.finch.Decode.instance[Task[Node], Application.Json] { (b, cs) =>
//      Right(
    implicit val jsonToLabeledNode = DecodeJson
      .jsonToLabeledNode(ontology, allowedProperties, forbiddenProperties)
//          .decode(b.asString(cs)))
//    }
    post(body[Task[Node], lspace.services.codecs.Application.JsonLD :+: Application.Json :+: CNil]) {
      nodeTask: Task[Node] =>
        nodeTask.flatMap { node =>
          val t = graph.transaction
          for {
            newNode <- t.nodes.create(ontology)
            _       <- newNode --- `@id` --> (graph.iri + "/" + label + "/" + newNode.id)
//            _       <- node --- `@id` --> (graph.iri + "/" + label + "/" + newNode.id)
            _ <- newNode.addLabel(ontology)
//            _       <- t ++ node.graph //Detached graph does not work here, it does not persist resources
            _ <- Task.sequence(node.outE().map(e => newNode --- e.key --> e.to))
            _ <- t.commit()
            r <- graph.nodes.hasId(newNode.id)
          } yield Created(r.get)
        }.toIO //(catsEffect(global))
    }
  }

  def replaceById: Endpoint[IO, Node] = {
    import io.finch.internal.HttpContent
    implicit val decoder = Decoder(DetachedGraph)
    implicit val d1 = io.finch.Decode.instance[Task[Node], lspace.services.codecs.Application.JsonLD] { (b, cs) =>
      Right(
        DecodeJsonLD
          .jsonldToLabeledNode(ontology, allowedProperties, forbiddenProperties)
          .decode(b.asString(cs)))
    }
    implicit val d2 = io.finch.Decode.instance[Task[Node], Application.Json] { (b, cs) =>
      Right(
        DecodeJson
          .jsonToLabeledNode(ontology, allowedProperties, forbiddenProperties)
          .decode(b.asString(cs)))
    }
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
//              _ <- node --- `@id` --> (graph.iri + "/" + label + "/" + id)
              _ <- Task.sequence(existingNode.outE().filterNot(e => e.key == `@id` || e.key == `@ids`).map(_.remove()))
//              _ <- t ++ node.graph
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
    import io.finch.internal.HttpContent
    implicit val decoder = Decoder(DetachedGraph)
    implicit val d1 = io.finch.Decode.instance[Task[Node], lspace.services.codecs.Application.JsonLD] { (b, cs) =>
      Right(
        DecodeJsonLD
          .jsonldToLabeledNode(ontology, allowedProperties, forbiddenProperties)
          .decode(b.asString(cs)))
    }
    implicit val d2 = io.finch.Decode.instance[Task[Node], Application.Json] { (b, cs) =>
      Right(
        DecodeJson
          .jsonToLabeledNode(ontology, allowedProperties, forbiddenProperties)
          .decode(b.asString(cs)))
    }
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
//              _ <- node --- `@id` --> (graph.iri + "/" + label + "/" + id)
              _ <- Task.sequence(
                existingNode
                  .outE()
                  .filterNot(e => e.key == `@id` || e.key == `@ids`)
                  .filter(e => node.outMap().keySet.contains(e.key))
                  .map(_.remove()))
//              _ <- t ++ node.graph
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

  trait SparqlApi extends ExecutionApi {
    def query: Endpoint[IO, List[Node]] = ???
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
    def mutate: Endpoint[IO, Unit]          = ???
    def ask: Endpoint[IO, Boolean]          = ???
    def subscribe: Endpoint[IO, List[Node]] = ???
  }
  object sparql extends SparqlApi

  trait GraphqlApi extends ExecutionApi {
    def query: Endpoint[IO, List[Node]]     = ???
    def mutate: Endpoint[IO, Unit]          = ???
    def ask: Endpoint[IO, Boolean]          = ???
    def subscribe: Endpoint[IO, List[Node]] = ???
  }
  object graphql extends GraphqlApi

  trait LibrarianApi extends ExecutionApi {
    def query: Endpoint[IO, List[Node]]     = ???
    def mutate: Endpoint[IO, Unit]          = ???
    def ask: Endpoint[IO, Boolean]          = ???
    def subscribe: Endpoint[IO, List[Node]] = ???
  }
  object librarian extends LibrarianApi

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
