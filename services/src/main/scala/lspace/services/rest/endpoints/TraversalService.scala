package lspace.services.rest.endpoints

import java.time.Instant

import lspace.librarian.process.traversal.{Collection, Traversal}
import lspace.librarian.structure._
import cats.effect.IO
import io.finch._
import lspace.codec.{NativeTypeDecoder, NativeTypeEncoder}
import lspace.decode.DecodeJsonLD
import lspace.librarian.datatype.DataType
import lspace.librarian.provider.detached.DetachedGraph
import monix.eval.Task
import scribe._
import shapeless.{CNil, HList}

//object TraversalService {
//  type Aux[Json0] = TraversalService { type Json = Json0 }
//  def apply[Json0](graph: Graph)(implicit baseDecoder: NativeTypeDecoder.Aux[Json0]): TraversalService.Aux[Json0] =
//    new TraversalService(graph) { type Json = Json0 }
//}
object TraversalService {
  def apply[Json0](graph0: Graph)(implicit baseDecoder0: NativeTypeDecoder.Aux[Json0],
                                  baseEncoder0: NativeTypeEncoder.Aux[Json0]): TraversalService =
    new TraversalService {
      val graph: Graph = graph0
      type Json = Json0
      implicit override def baseDecoder: NativeTypeDecoder.Aux[Json] = baseDecoder0
      implicit override def baseEncoder: NativeTypeEncoder.Aux[Json] = baseEncoder0
    }
}

trait TraversalService extends Api {
  def graph: Graph
  type Json
  implicit def baseDecoder: NativeTypeDecoder.Aux[Json]
  implicit def baseEncoder: NativeTypeEncoder.Aux[Json]
//case class TraversalService(graph: Graph)(implicit baseDecoder: NativeTypeDecoder) extends Api {
//  implicit val _graph  = graph

  implicit val decoder = lspace.codec.Decoder(DetachedGraph)
//    .asInstanceOf[lspace.codec.Decoder[Any]] //todo JsonLD context per client-session

  implicit val ec = monix.execution.Scheduler.global

  /**
    * Traversal on this (multi-)graph
    *
    * @return
    */
  lazy val traverse: Endpoint[IO, Collection[Any]] = {
    import io.finch.internal.HttpContent
    implicit val _graph = graph
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

  lazy val getLabels: Endpoint[IO, Collection[Any]] = get("label") {
    val start = Instant.now()
    val result = graph.ns.g.N
      .union(_.hasLabel(Ontology.ontology), _.hasLabel(Property.ontology), _.hasLabel(DataType.ontology))

    val collection: Collection[Any] = Collection(start, Instant.now(), result.toList, result.ct)
    Ok(collection)
  }

  lazy val api = traverse :+: getLabels

}
