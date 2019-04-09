package lspace.services.rest.endpoints

import java.time.Instant

import lspace.librarian.traversal.Collection
import lspace.structure._
import cats.effect.IO
import io.finch._
import lspace.codec.{NativeTypeDecoder, NativeTypeEncoder}
import lspace.decode.DecodeJsonLD
import lspace.datatype.DataType
import lspace.provider.detached.DetachedGraph
import monix.eval.Task
import monix.reactive.Observable
import scribe._
import shapeless.{CNil, HList}

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

  import lspace.Implicits.Scheduler.global

  import lspace._
  import Implicits.AsyncGuide._

  /**
    * Traversal on this (multi-)graph
    *
    * @return
    */
  lazy val traverse: Endpoint[IO, _root_.fs2.Stream[IO, Collection[Any, ClassType[Any]]]] = {
    import io.finch.internal.HttpContent
    import cats.effect._, _root_.fs2._
    import io.finch.fs2._
    import _root_.fs2.interop.reactivestreams._
    import scala.concurrent.ExecutionContext
    implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    implicit val _graph                         = graph
    implicit val d1 = io.finch.Decode
      .instance[Task[Traversal[ClassType[Any], ClassType[Any], HList]], lspace.services.codecs.Application.JsonLD] {
        (b, cs) =>
          Right(
            DecodeJsonLD.jsonldToTraversal
              .decode(b.asString(cs)))
      }
    post(
      "traverse" :: body[Task[Traversal[ClassType[Any], ClassType[Any], HList]],
                         lspace.services.codecs.Application.JsonLD]).mapOutputAsync {
      traversalTask: Task[Traversal[ClassType[Any], ClassType[Any], HList]] =>
//        Ok(
//        Observable
//          .fromTask(
        traversalTask
          .map { traversal =>
            println(s"executing ${traversal.prettyPrint}")
            val start = Instant.now()
            //          traversal.untyped.withGraph(graph).toListF.map { values =>
            //            val collection: Collection[Any, ClassType[Any]] = Collection(start, Instant.now(), values)
            //            collection.logger.debug("result count: " + values.size.toString)
            //            Ok(collection)
            //          }
            traversal.untyped
              .withGraph(graph)
              .apply()
              .bufferTumbling(100)
              .map { values =>
                val collection: Collection[Any, ClassType[Any]] = Collection(start, Instant.now(), values.toList)
                collection.logger.debug("result count: " + values.size.toString)
                collection
//                    Ok(collection)
              }
              .toReactivePublisher
              .toStream[IO]()
          }
          .map(Ok(_))
          .toIO
//          .flatten
    }
  }

  import eu.timepit.refined.api.Refined
  import eu.timepit.refined.string._
  import io.finch.refined._
//  val byIri = get(param[String Refined Url]("url")) { (s: String Refined Url) =>
//    ???
//  }

  lazy val getLabels: Endpoint[IO, Collection[Any, ClassType[Any]]] = get("label") {
    val start = Instant.now()
    val traversal = g.N
      .union(_.hasLabel(Ontology.ontology), _.hasLabel(Property.ontology), _.hasLabel(DataType.ontology))

    traversal.untyped
      .withGraph(graph)
      .toListF
      .map { values =>
        val collection: Collection[Any, ClassType[Any]] = Collection(start, Instant.now(), values, traversal.ct)
        Ok(collection)
      }
      .toIO
  }

  lazy val api = traverse :+: getLabels

}
