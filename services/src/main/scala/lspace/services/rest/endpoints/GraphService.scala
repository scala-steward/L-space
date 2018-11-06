package lspace.services.rest.endpoints

import java.time.Instant

import com.twitter.finagle.http.filter.Cors
import lspace.librarian.process.traversal.{Collection, Traversal}
import lspace.librarian.structure._
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success}
import argonaut._
import argonaut.Argonaut._
import cats.effect.IO
import io.finch._
import io.finch.argonaut.preserveOrder._
import lspace.parse.json.JsonLD

object GraphService {}

case class GraphService(context: String, graph: Graph) extends Service {
  val log = LoggerFactory.getLogger(getClass)

  val headersAll = root.map(_.headerMap.toMap)

  val policy: Cors.Policy = Cors.Policy(allowsOrigin = _ => Some("*"),
                                        allowsMethods = _ => Some(Seq("GET", "POST")),
                                        allowsHeaders = _ => Some(Seq("Accept")))

  //  private def resultToJson(result: List[Any]): JsValue = {
  //    JsArray(result.map {
  //      case node: Node => graph.ldParser.toJSON.nodeToJson(node)._1.asInstanceOf[JsValue]
  //      case property: Edge[_, _] => graph.ldParser.toJSON.propertyToJson(property)._1.asInstanceOf[JsValue]
  //      case value => graph.ldParser.toJSON.valueToJson(value).asInstanceOf[JsValue]
  //    })
  //  }
  //  implicit def renderResult(result: List[Any]): Endpoint[String] = {
  //    Ok(resultToJson(result))
  //  }
  //  /**
  //   * retrieve a single resource
  //   * @param uri
  //   * @return
  //   */
  //  def get(uri: String): Endpoint[String]
  //
  //  /**
  //   * Traversal from NodeResource (Vertex) or LinkResource (Edge)
  //   * @param uri
  //   * @return
  //   */
  //  def traversalFrom(uri: String): Endpoint[Json]

  /**
    * Traversal on this (multi-)graph
    * @return
    */
  //  def traversal(): Endpoint[Json]
  val traverse: Endpoint[IO, Json] = post("traverse" :: jsonBody[Json]) { json: Json =>
    //    println(s"Got foo: $json")
    json.obj
      .map { obj =>
        JsonLD.resource(obj).map {
          case traversalNode: Node =>
            val start  = Instant.now()
            val result = Traversal.wrap(traversalNode)(graph)(ClassType.default[Any]).toUntypedStream.toList
            val collection = Collection(start, Instant.now(), result)(new DataType[Any] {
              override def iri: String = ""
            })
            log.debug("result count: " + result.size.toString)
            val (collectionJson, builder1) = JsonLD.nodeToJsonWithContext(collection)

            Ok(collectionJson).withHeader("Content-Type", "application/ld+json")
          case _ =>
            log.debug("not a valid traversal provided")
            NotAcceptable(new Exception("not a valid traversal provided"))
        } match {
          case Success(result) => result
          case Failure(error) =>
            error.printStackTrace()
            log.error(error.getMessage)
            NotAcceptable(new Exception("not a valid body"))
        }
      }
      .getOrElse(NotAcceptable(new Exception("traversal object expected")))

  }

  val getLabels: Endpoint[IO, Json] = get("label") {
    val start = Instant.now()
    val result = graph.g.N
      .union(_.hasLabel(Ontology.ontology), _.hasLabel(Property.ontology), _.hasLabel(DataType.ontology))
    val collection: Collection[Node] = Collection(start, Instant.now(), result.toList)(result.et)
    val (collectionJson, builder1)   = JsonLD.nodeToJsonWithContext(collection)

    Ok(collectionJson).withHeader("Content-Type", "application/ld+json")
  }

  val api = context :: "v1" :: (traverse :+: getLabels)

  //  protected def traverse[Start, End, LastStep, Labels <: HList](traversal: Traversal[Start, End, LastStep, Labels]): Result
}
