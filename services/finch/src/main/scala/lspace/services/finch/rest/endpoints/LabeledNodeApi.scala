package lspace.services.rest.endpoints

import cats.Applicative
import cats.effect.IO
import io.finch._
import lspace._
import lspace.Label.P._
import lspace.codec.json.jsonld.JsonLDDecoder
import lspace.codec.{ActiveContext, ActiveProperty, ContextedT}
import lspace.decode.{DecodeGraphQL, DecodeJson, DecodeJsonLD}
import lspace.librarian.task.AsyncGuide
import monix.eval.Task
import monix.execution.Scheduler
import shapeless.{:+:, ::, CNil, HList, HNil}

object LabeledNodeApi {
  def apply[JSON](graph: Graph, ontology: Ontology, newNodeBaseIri: String = "")(implicit
    activeContext: ActiveContext = ActiveContext(),
    decoder: JsonLDDecoder[JSON],
    guide: AsyncGuide,
    scheduler: Scheduler
  ): LabeledNodeApi[JSON] =
    new LabeledNodeApi(
      graph,
      if (newNodeBaseIri.nonEmpty) newNodeBaseIri
      else graph.iri + "/" + ontology.iri.reverse.takeWhile(_ != '/').reverse.toLowerCase() + "/",
      ontology
    )(activeContext, decoder, guide, scheduler)

}

class LabeledNodeApi[JSON](
  graph: Graph,
  val newNodeBaseIri: String,
  val ontology: Ontology,
  val allowedProperties: List[Property] = List(),
  val forbiddenProperties: List[Property] = List()
)(implicit val activeContext: ActiveContext, decoder: JsonLDDecoder[JSON], guide: AsyncGuide, scheduler: Scheduler)
    extends Api {
  //  implicit val encoder = Encoder //todo Encode context per client-session
  //  implicit val decoder
  //    : lspace.codec.json.jsonld.Decoder[Any] = Decoder(graph).asInstanceOf[lspace.codec.json.jsonld.Decoder[Any]] //todo Decode context per client-session

  import lspace.services.codecs.Decode._

  implicit val jsonldToNode = DecodeJsonLD
    .jsonldToLabeledNode(ontology, allowedProperties, forbiddenProperties)
  implicit val jsonToNode = DecodeJson
    .jsonToLabeledNode(ontology, allowedProperties, forbiddenProperties)

  def context =
    get(path("@context")) {
      Ok(activeContext)
    } :+: get(path("context")) {
      Ok(activeContext)
    }

  val idToNodeTask: Long => Task[Option[Node]] = (id: Long) =>
    g.N
      .hasIri(newNodeBaseIri + id)
      .hasLabel(ontology)
      .withGraph(graph)
      .headOptionF
  val iriToNodeTask: String => Task[Option[Node]] = (iri: String) =>
    g.N
      .hasIri(iri)
      .hasLabel(ontology)
      .withGraph(graph)
      .headOptionF

  /** GET /{id}
    */
  def byId: Endpoint[IO, ContextedT[Node]] = get(path[Long]).mapOutputAsync { id =>
    idToNodeTask(id).map(_.map(ContextedT(_)).map(Ok).getOrElse(NotFound(new Exception("Resource not found")))).to[IO]
  }

  //  def byIri: Endpoint[IO, ContextedT[Node]] = get(path[String]).mapOutputAsync { (iri: String) =>
  //    iriToNodeTask(iri).map(_.map(ContextedT(_)).map(Ok).getOrElse(NotFound(new Exception("Resource not found")))).to[IO]
  //  }

  def list: Endpoint[IO, ContextedT[List[Any]]] = get(paths[String]).mapOutputAsync {
    case Nil => g.N.hasLabel(ontology).withGraph(graph).toListF.map(ContextedT(_)).map(Ok).to[IO]
    case List(id) =>
      g.N
        .hasIri(newNodeBaseIri + id)
        .hasLabel(ontology)
        .withGraph(graph)
        .toListF
        .map(ContextedT(_))
        .map(Ok)
        .to[IO]
    case List(id, key) =>
      val expKey = activeContext.expandIri(key).iri
      activeContext.definitions
        .get(expKey)
        .orElse(Property.properties.get(expKey).map(p => ActiveProperty(property = p)()))
        .map { activeProperty =>
          activeProperty.`@type`.headOption
            .map {
              case ontology: Ontology =>
                g.N
                  .hasIri(newNodeBaseIri + id)
                  .hasLabel(this.ontology)
                  .out(activeProperty.property)
                  .hasLabel(ontology)
                  .withGraph(graph)
                  .toListF
                  .map(ContextedT(_))
                  .map(Ok)
                  .to[IO]
              case property: Property =>
                g.N
                  .hasIri(newNodeBaseIri + id)
                  .hasLabel(ontology)
                  .out(activeProperty.property)
                  .hasLabel(property)
                  .withGraph(graph)
                  .toListF
                  .map(ContextedT(_))
                  .map(Ok)
                  .to[IO]
              case datatype: DataType[_] =>
                g.N
                  .hasIri(newNodeBaseIri + id)
                  .hasLabel(ontology)
                  .out(activeProperty.property)
                  .hasLabel(datatype)
                  .withGraph(graph)
                  .toListF
                  .map(ContextedT(_))
                  .map(Ok)
                  .to[IO]
            }
            .getOrElse {
              g.N
                .hasIri(newNodeBaseIri + id)
                .hasLabel(ontology)
                .out(activeProperty.property)
                .withGraph(graph)
                .toListF
                .map(ContextedT(_))
                .map(Ok)
                .to[IO]
            }
        }
        .getOrElse(Task.now(Forbidden(new Exception(s"path $key not supported"))).to[IO])
    case List(id, key1, key2) =>
      val expKey1 = activeContext.expandIri(key1).iri
      val expKey2 = activeContext.expandIri(key2).iri
      (for {
        activeProperty1 <- activeContext.definitions
          .get(expKey1)
          .orElse(Property.properties.get(expKey1).map(p => ActiveProperty(property = p)()))
        activeProperty2 <- activeContext.definitions
          .get(expKey2)
          .orElse(Property.properties.get(expKey2).map(p => ActiveProperty(property = p)()))
      } yield g.N
        .hasIri(newNodeBaseIri + id)
        .hasLabel(ontology)
        .out(activeProperty1.property)
        .out(activeProperty2.property)
        .withGraph(graph)
        .toListF
        .map(ContextedT(_))
        .map(Ok)
        .to[IO]).getOrElse(Task.now(Forbidden(new Exception(s"path $key1/$key2 not supported"))).to[IO])
    case paths => Task.now(Forbidden(new Exception(s"path ${paths.mkString("/")} not supported"))).to[IO]
  }

  def create: Endpoint[IO, ContextedT[Node]] =
    post(body[Task[Node], lspace.services.codecs.Application.JsonLD :+: Application.Json :+: CNil]) {
      nodeTask: Task[Node] =>
        nodeTask
          .flatMap { node =>
            val t = graph.transaction
            for {
              newNode <- t.nodes.create(node.labels: _*)
              _       <- newNode --- `@id` --> (newNodeBaseIri + newNode.id)
              _       <- newNode.addLabel(ontology)
              _       <- Task.sequence(node.outE().map(e => newNode --- e.key --> e.to))
              _       <- t.commit()
              r       <- graph.nodes.hasId(newNode.id)
              out = ContextedT(r.get)
            } yield Created(out).withHeader("Location" -> newNode.iri)
          }
          .to[IO] // (catsEffect(global))
    }

  def replaceById: Endpoint[IO, ContextedT[Node]] = {
    implicit val jsonldToNode = DecodeJsonLD
      .jsonldToLabeledNode(ontology, allowedProperties, forbiddenProperties)
    implicit val jsonToNode = DecodeJson
      .jsonToLabeledNode(ontology, allowedProperties, forbiddenProperties)

    put(path[String] :: body[Task[Node], lspace.services.codecs.Application.JsonLD :+: Application.Json :+: CNil]) {
      (id: String, nodeTask: Task[Node]) => // TODO: validate before mutating
        nodeTask
          .flatMap { node =>
            val t = graph.transaction
            for {
              existingNode <-
                t.nodes // TODO: validate if node is without @id or @id is equal to ```graph.iri + "/" + label + "/" + id```
                  .hasIri(newNodeBaseIri + id)
                  .headL
              _ <- Task.sequence(node.outE(`@id`).map(_.remove()))
              _ <- Task.sequence(existingNode.outE().filterNot(e => e.key == `@id` || e.key == `@ids`).map(_.remove()))
              _ <- Task.sequence(node.outE().map(e => existingNode --- e.key --> e.to))
              _ <- t.commit()
              r <- graph.nodes.hasId(existingNode.id)
              out = ContextedT(r.get)
            } yield Ok(out)
          }
          .onErrorHandle(f =>
            NotFound(new Exception("cannot PUT a resource which does not exist"))
          ) // TODO: handle 'unexpected' multiple results
          .to[IO]
    }
  }

  def updateById: Endpoint[IO, ContextedT[Node]] = {
    implicit val jsonldToNode = DecodeJsonLD
      .jsonldToNode(allowedProperties, forbiddenProperties)
    implicit val jsonToNode = DecodeJson
      .jsonToNode(allowedProperties, forbiddenProperties)

    patch(path[String] :: body[Task[Node], lspace.services.codecs.Application.JsonLD :+: Application.Json :+: CNil]) {
      (id: String, nodeTask: Task[Node]) =>
        nodeTask
          .flatMap { node =>
            val t = graph.transaction
            for {
              existingNode <-
                t.nodes // TODO: validate if node is without @id or @id is equal to ```graph.iri + "/" + label + "/" + id```
                  .hasIri(newNodeBaseIri + id)
                  .headL
              _ <- Task.sequence(node.outE(`@id`).map(_.remove()))
              _ <- Task.sequence(
                existingNode
                  .outE()
                  .filterNot(e => e.key == `@id` || e.key == `@ids`)
                  .filter(e => node.outMap().keySet.contains(e.key))
                  .map(_.remove())
              )
              _ <- Task.sequence(node.outE().map(e => existingNode --- e.key --> e.to))
              _ <- t.commit()
              r <- graph.nodes.hasId(existingNode.id)
              out = ContextedT(r.get)
            } yield Ok(out)
          }
          .onErrorHandle(f => NotFound(new Exception("cannot PATCH a resource which does not exist")))
          .to[IO]
    }
  }
  def removeById: Endpoint[IO, Node] = delete(path[String]) { id: String =>
    val t = graph.transaction
    (for {
      node <- t.nodes
        .hasIri(newNodeBaseIri + id)
        .headL // TODO: handle 'unexpected' multiple results
      _ <- node.remove() // TODO: decide if values or blank nodes need to be explicitly removed
    } yield NoContent[Node])
      .onErrorHandle(f => NotFound(new Exception("cannot DELETE a resource which does not exist")))
      .to[IO]
  }

  def api =
    /*context :+: */ byId :+: /*byIri :+: */ list :+:
      create :+: replaceById :+: updateById :+: removeById
  //  def labeledApi = label :: api
}
