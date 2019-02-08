package lspace.services.rest.endpoints

import java.time.Instant

import lspace.librarian.process.traversal.{Collection, Traversal}
import lspace.librarian.structure._
import cats.effect.IO
import io.finch._
import lspace.decode.DecodeJsonLD
import lspace.librarian.datatype.DataType
import monix.eval.Task
import scribe._
import shapeless.{CNil, HList}

case class TraversalService(graph: Graph) extends Api {
  implicit val _graph = graph
  implicit val decoder: lspace.codec.Decoder[Any] = lspace.codec.argonaut
    .Decoder(graph)
    .asInstanceOf[lspace.codec.Decoder[Any]] //todo JsonLD context per client-session

  implicit val ec = monix.execution.Scheduler.global

  /**
    * Traversal on this (multi-)graph
    *
    * @return
    */
  val traverse: Endpoint[IO, Collection[Any]] = {
    import io.finch.internal.HttpContent
    implicit val d1 = io.finch.Decode
      .instance[Task[Traversal[ClassType[Any], ClassType[Any], HList]], lspace.services.codecs.Application.JsonLD] {
        (b, cs) =>
          Right(
            DecodeJsonLD.jsonldToTraversal
              .decode(b.asString(cs)))
      }
    post(
      "traverse" :: body[Task[Traversal[ClassType[Any], ClassType[Any], HList]],
                         lspace.services.codecs.Application.JsonLD]) {
      traversalTask: Task[Traversal[ClassType[Any], ClassType[Any], HList]] =>
        traversalTask.map { traversal =>
          val start                     = Instant.now()
          val result                    = traversal.toUntypedStream.toList
          val collection: Collection[_] = Collection(start, Instant.now(), result)

          collection.logger.debug("result count: " + result.size.toString)
          Ok(collection)
        }.toIO
    }
  }

  import eu.timepit.refined.api.Refined
  import eu.timepit.refined.string._
  import io.finch.refined._
//  val byIri = get(param[String Refined Url]("url")) { (s: String Refined Url) =>
//    ???
//  }

  val getLabels: Endpoint[IO, Collection[Any]] = get("label") {
    val start = Instant.now()
    val result = graph.ns.g.N
      .union(_.hasLabel(Ontology.ontology), _.hasLabel(Property.ontology), _.hasLabel(DataType.ontology))

    val collection: Collection[Any] = Collection(start, Instant.now(), result.toList, result.ct)
    Ok(collection)
  }

  val api = traverse :+: getLabels

}
