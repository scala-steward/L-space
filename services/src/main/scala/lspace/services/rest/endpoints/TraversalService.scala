package lspace.services.rest.endpoints

import java.time.Instant

import lspace.librarian.process.traversal.{Collection, Traversal}
import lspace.librarian.structure._
import org.slf4j.LoggerFactory
import cats.effect.IO
import io.finch._
import lspace.librarian.datatype.DataType
import lspace.parse.JsonLD
import shapeless.{CNil, HList}

case class TraversalService(graph: Graph) extends JsonLDModule {
  val log             = LoggerFactory.getLogger(getClass)
  implicit val _graph = graph
  implicit val jsonld = JsonLD(graph) //todo JsonLD context per client-session

  /**
    * Traversal on this (multi-)graph
    * @return
    */
  val traverse: Endpoint[IO, Collection[Any]] =
    post("traverse" :: bodyJsonLDTraversal) { traversal: Traversal[ClassType[Any], ClassType[Any], HList] =>
      val start                     = Instant.now()
      val result                    = traversal.toUntypedStream.toList
      val collection: Collection[_] = Collection(start, Instant.now(), result)
      log.debug("result count: " + result.size.toString)
      Ok(collection)
    }

  val getLabels: Endpoint[IO, Collection[Any]] = get("label") {
    val start = Instant.now()
    val result = graph.ns.g.N
      .union(_.hasLabel(Ontology.ontology), _.hasLabel(Property.ontology), _.hasLabel(DataType.ontology))

    val collection: Collection[Any] = Collection(start, Instant.now(), result.toList, result.ct)
    Ok(collection)
  }

  val api = traverse :+: getLabels

}
