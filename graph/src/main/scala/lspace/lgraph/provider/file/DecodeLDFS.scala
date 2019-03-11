package lspace.lgraph.provider.file

import lspace.NS.types
import lspace.codec.{ExpandedMap, NativeTypeDecoder}
import lspace.codec.exception.FromJsonException
import lspace.structure._
import monix.eval.Task

case class DecodeLDFS[Json0](override val graph: Graph, idMaps: IdMaps = IdMaps())(
    implicit val baseDecoder: NativeTypeDecoder.Aux[Json0])
    extends lspace.codec.Decoder {
  type Json = Json0
  override def apply(graph0: Lspace): lspace.codec.Decoder.Aux[Json] = DecodeLDFS.apply(graph0, idMaps)(baseDecoder)

  lazy val nsDecoder = {
    def graph0       = graph
    def baseDecoder0 = baseDecoder
    new lspace.codec.Decoder {
      type Json = Json0
      val graph: Graph                                      = graph0.ns
      implicit def baseDecoder: NativeTypeDecoder.Aux[Json] = baseDecoder0
      lazy val nsDecoder                                    = this
    }
  }

  override def tryNodeRef(json: Json)(implicit activeContext: AC): Option[Task[Node]] = //need IdMaps here
    json.string
      .map(activeContext.expandIri)
      .map(s => Task(graph.nodes.upsert(s.iri))) //TODO: add label if missing?
      .orElse(
        json.long
          .orElse(json.int.map(_.toLong))
          .flatMap(idMaps.nodeIds.get)
//              .getOrElse(id, throw FromJsonException(s"unknown node id ref $id in ${graph.iri} ${idMaps.nodeIds.toList
//                .sortBy(_._1)} ${graph.nodes().toList.map(_.id).sorted}")))
          .map(id => Task(graph.getOrCreateNode(id))))

  override def toNode(expandedJson: ExpandedMap[Json], label: Option[Ontology])(
      implicit activeContext: AC): Task[Node] = {
    expandedJson
      .get(types.`@id`)
      .flatMap(json =>
        json.long
          .orElse(json.int.map(_.toLong)))
      .flatMap(idMaps.nodeIds.get)
//        .getOrElse(_, throw FromJsonException("unknown node id ref")))
      .map { id =>
        expandedJson.extractOntologies.flatMap { ontologies =>
          Task(graph.getOrCreateNode(id)).map { node =>
            if (ontologies.isEmpty) label.foreach(node.addLabel)
            else ontologies.foreach(node.addLabel)
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

  override def tryEdgeRef(json: Json, label: Property)(implicit activeContext: AC): Option[Task[Edge[_, _]]] =
    json.string
      .flatMap(graph.edges.hasIri(_).headOption) //TODO: check if label == edge.key and throw exception if !=
      .map(Task.now)
      .orElse(
        json.long
          .orElse(json.int.map(_.toLong))
          .flatMap(idMaps.edgeIds.get)
//            .getOrElse(_, throw FromJsonException("unknown edge id ref")))
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
