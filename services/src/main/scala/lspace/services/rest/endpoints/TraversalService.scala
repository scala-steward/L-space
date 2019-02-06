package lspace.services.rest.endpoints

import java.time.Instant

import lspace.librarian.process.traversal.{Collection, Traversal}
import lspace.librarian.structure._
import cats.effect.IO
import io.finch._
import lspace.librarian.datatype.DataType
import lspace.parse.JsonLD
import lspace.services.codecs.JsonLDModule
import scribe._
import shapeless.{CNil, HList}

case class TraversalService(graph: Graph) extends JsonLDModule {
  implicit val _graph = graph
  implicit val jsonld = JsonLD(graph) //todo JsonLD context per client-session

  /**
    * Traversal on this (multi-)graph
    * @return
    */
  val traverse: Endpoint[IO, Collection[Any]] =
    post("traverse" :: bodyJsonLDTyped(Traversal.ontology, node => Traversal.toTraversal(node)(graph))) {
      traversal: Traversal[ClassType[Any], ClassType[Any], HList] =>
        val start                     = Instant.now()
        val result                    = traversal.toUntypedStream.toList
        val collection: Collection[_] = Collection(start, Instant.now(), result)

        collection.logger.debug("result count: " + result.size.toString)
        Ok(collection)
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
