package lspace.services.rest.endpoints

import java.time.Instant

import lspace.librarian.process.traversal.{Collection, Traversal}
import lspace.librarian.structure._
import org.slf4j.LoggerFactory

import cats.effect.IO
import io.finch._
import shapeless.{CNil, HList}

object GraphService {}

case class GraphService(context: String, graph: Graph) extends Service with JsonLDModule {
  val log    = LoggerFactory.getLogger(getClass)
  val jsonld = lspace.parse.json.JsonLD(graph)

  /**
    * Traversal on this (multi-)graph
    * @return
    */
  val traverse: Endpoint[IO, Collection[Any]] =
    post("traverse" :: jsonldTraversal(graph, jsonld)) { traversal: Traversal[ClassType[Any], ClassType[Any], HList] =>
      //    println(s"Got foo: $json")
      val start  = Instant.now()
      val result = traversal.toUntypedStream.toList
      val collection = Collection(start, Instant.now(), result)(new DataType[Any] {
        override def iri: String = ""
      })
      log.debug("result count: " + result.size.toString)
      Ok(collection)
    }

  val getLabels: Endpoint[IO, Collection[Any]] = get("label") {
    val start = Instant.now()
    val result = graph.ns.g.N
      .union(_.hasLabel(Ontology.ontology), _.hasLabel(Property.ontology), _.hasLabel(DataType.ontology))
    val collection: Collection[Any] = Collection(start, Instant.now(), result.toList)(result.et)
    Ok(collection)
  }

  val api = context :: "v1" :: (traverse :+: getLabels)

  def printRoutes = (context :: "v1" :: (traverse :+: getLabels)).toString()
}
