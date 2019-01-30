package lspace.lgraph.provider.file

import argonaut._
import Argonaut._
import lspace.NS.types
import lspace.librarian.structure._
import lspace.parse.{ActiveContext, JsonInProgress, JsonLD}
import lspace.parse.util.FromJsonException
import monix.eval.Task

case class JsonLDFS(graph: Graph, idMaps: IdMaps = IdMaps()) extends JsonLD(graph) {

  override lazy val encode = new super.encode {

    override def fromAny(value: Any, expectedType: Option[ClassType[_]] = None)(
        implicit activeContext: ActiveContext): JsonInProgress = {
      value match {
        case resource: IriResource =>
          resource match {
            case value: Value[_] =>
              if (expectedType.contains(value.label)) {
                fromData(value.value, value.label)
              } else {
                val jip = fromData(value.value, value.label)
                JsonInProgress(
                  Json.jObject(
                    JsonObject
                      .fromTraversableOnce(
                        Map(types.`@value` -> jip.json, types.`@type` -> value.label.iri.compact.asJson))),
                  jip.activeContext
                )
              }
            case node: Node               => JsonInProgress(node.id.asJson)
            case edge: Edge[_, _]         => JsonInProgress(edge.id.asJson)
            case iriResource: IriResource => JsonInProgress(iriResource.iri.asJson, activeContext)
          }
        case _ =>
          val label = ClassType.valueToOntologyResource(value)
          if (expectedType.contains(label)) {
            fromData(value, label)
          } else {
            val jip = fromData(value, label)
            JsonInProgress(
              Json.jObject(
                JsonObject
                  .fromTraversableOnce(Map(types.`@value` -> jip.json, types.`@type` -> label.iri.compact.asJson))),
              jip.activeContext)
          }
      }
    }
  }

  override lazy val decode = new super.decode {
    override def tryNodeRef(json: Json)(implicit activeContext: ActiveContext): Option[Task[Node]] = //need IdMaps here
      json.string
        .map(activeContext.expandIri)
        .map(s => Task(graph.nodes.upsert(s))) //TODO: add label if missing?
        .orElse(
          json.number
            .flatMap(n => n.toLong.orElse(n.toInt.map(_.toLong)))
            .map(idMaps.nodeIds
              .getOrElse(_, throw FromJsonException("unknown node id ref")))
            .map(id => Task(graph.getOrCreateNode(id))))

    override def toNode(expandedJson: Map[String, Json], label: Option[Ontology])(
        implicit activeContext: ActiveContext): Task[Node] = {
      expandedJson
        .get(types.`@id`)
        .flatMap(_.number.flatMap(n => n.toLong.orElse(n.toInt.map(_.toLong))))
        .map(idMaps.nodeIds
          .getOrElse(_, throw FromJsonException("unknown node id ref")))
        .map { id =>
          extractOntologies(expandedJson).flatMap { ontologies =>
            Task(graph.getOrCreateNode(id)).map { node =>
              if (ontologies.isEmpty) label.foreach(node.addLabel)
              else ontologies.foreach(node.addLabel)
              if ((expandedJson - types.`@id` - types.`@type`).nonEmpty) {
                println("node object has more properties that expected for ld+json+fs")
              } //TODO: log/warn if more unexpected data is found
              node
            }
          }
        }
        .getOrElse(Task.raiseError(FromJsonException(s"@id is expected to be a long in ld+json+fs ${expandedJson
          .get(types.`@id`)}")))
    }

    override def tryEdgeRef(json: Json, label: Property)(
        implicit activeContext: ActiveContext): Option[Task[Edge[_, _]]] =
      json.string
        .flatMap(graph.edges.hasIri(_).headOption) //TODO: check if label == edge.key and throw exception if !=
        .map(Task.now)
        .orElse(
          json.number
            .flatMap(n => n.toLong.orElse(n.toInt.map(_.toLong)))
            .map(idMaps.nodeIds
              .getOrElse(_, throw FromJsonException("unknown edge id ref")))
            .flatMap(graph.edges.hasId(_).map(Task(_))))
    //    override def toEdge(expandedJson: Map[String, Json], expectedTypes: List[Property])(
    //      implicit activeContext: ActiveContext): Option[Task[Edge[Any, Any]]] = {
    //      (activeContext.extractFrom(expandedJson), activeContext.extractTo(expandedJson)) match {
    //        case (Some(source), Some(destination)) =>
    //          Some((for {
    //            from <- toResource(source, None)
    //            to   <- toResource(destination, None)
    //          } yield {
    //            extractProperties(expandedJson)
    //              .map(_.orElse(expectedTypes.headOption))
    //              .flatMap {
    //                _.filter(expectedTypes.contains)
    //                  .map { label =>
    //                    val edge: Edge[Any, Any] = from.addOut(label, to)
    //                    withEdges(
    //                      edge,
    //                      expandedJson - types.`@context` - types.`@id` - types.`@ids` - types.`@from` - types.`@to` - types.`@type`)
    //                      .map(e => edge)
    //                  }
    //                  .getOrElse(Task.raiseError(FromJsonException("unexpected @type for edge object")))
    //              }
    //          }).flatten)
    //        case (Some(from), None) =>
    //          Some(Task.raiseError(FromJsonException("incomplete edge, missing @from")))
    //        case (None, Some(to)) =>
    //          Some(Task.raiseError(FromJsonException("incomplete edge, missing @from")))
    //        case _ => None
    //      }
    //    }
  }
}
