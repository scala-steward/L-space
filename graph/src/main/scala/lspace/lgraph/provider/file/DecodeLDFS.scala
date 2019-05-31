package lspace.lgraph.provider.file

import lspace.NS.types
import lspace.codec.{ActiveContext, ExpandedMap, NativeTypeDecoder}
import lspace.codec.exception.FromJsonException
import lspace.codec.jsonld.Decoder
import lspace.structure._
import monix.eval.Task

case class DecodeLDFS[Json0](override val graph: Graph, idMaps: IdMaps = IdMaps())(
    implicit val baseDecoder: NativeTypeDecoder.Aux[Json0])
    extends Decoder {
  type Json = Json0
  override def apply(graph0: Lspace): lspace.codec.jsonld.Decoder.Aux[Json] =
    DecodeLDFS.apply(graph0, idMaps)(baseDecoder)

  lazy val nsDecoder = {
    def graph0       = graph
    def baseDecoder0 = baseDecoder
    new Decoder {
      type Json = Json0
      val graph: Graph                                      = graph0.ns
      implicit def baseDecoder: NativeTypeDecoder.Aux[Json] = baseDecoder0
      lazy val nsDecoder                                    = this
    }
  }

  override def tryNodeRef(json: Json)(implicit activeContext: ActiveContext): Option[Task[Node]] = //need IdMaps here
    json.long
      .orElse(json.int.map(_.toLong))
      .flatMap(idMaps.nodeIds.get)
      //              .getOrElse(id, throw FromJsonException(s"unknown node id ref $id in ${graph.iri} ${idMaps.nodeIds.toList
      //                .sortBy(_._1)} ${graph.nodes().toList.map(_.id).sorted}")))
      .map(id => graph.getOrCreateNode(id))
      .orElse(
        json.string
          .map(activeContext.expandIri)
          .map(s => graph.nodes.upsert(s.iri)) //TODO: add label if missing?
      )

  override def toNode(expandedJson: ExpandedMap[Json], label: Option[Ontology])(
      implicit activeContext: ActiveContext): Task[Node] = {
    expandedJson
      .get(types.`@id`)
      .flatMap(json =>
        json.long
          .orElse(json.int.map(_.toLong)))
      .flatMap(idMaps.nodeIds.get)
//        .getOrElse(_, throw FromJsonException("unknown node id ref")))
      .map { id =>
        expandedJson.extractOntologies.flatMap { ontologies =>
          for {
            node <- graph.getOrCreateNode(id)
            _ <- Task.sequence(if (ontologies.isEmpty) label.toList.map(node.addLabel)
            else ontologies.map(node.addLabel))
          } yield {
            if ((expandedJson - types.`@id` - types.`@type`).nonEmpty) {
              scribe.warn("node object has more properties that expected for ld+json+fs")
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
    json.long
      .orElse(json.int.map(_.toLong))
      .flatMap(idMaps.edgeIds.get)
      //            .getOrElse(_, throw FromJsonException("unknown edge id ref")))
      .map(graph.edges.hasId(_).map(_.getOrElse(throw FromJsonException("wrong edgeref"))))
      .orElse(
        json.string
          .map(graph.edges.hasIri(_).headL) //TODO: check if label == edge.key and throw exception if !=
      )
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
