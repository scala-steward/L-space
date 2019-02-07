package lspace.codec.argonaut

import argonaut.{Json, JsonObject, Parse}
import lspace.codec
import lspace.codec.exception.FromJsonException
import lspace.librarian.structure.Graph
import lspace.parse.util._
import lspace.parse.{ActiveContext, ActiveProperty}
import lspace.types.vector.{Point, Polygon}
import monix.eval.Task

object Decode {
  def apply(graph: Graph): Decode = new Decode(graph)
}
class Decode(val graph: Graph) extends lspace.codec.Decode[Json, JsonObject] {

  override def encoder: codec.Encode[Json, JsonObject] = Encode

  override def getNewActiveContext: AC = ActiveContext()

  override def getNewActiveProperty: AP = ActiveProperty()

  override def parse(string: String): Task[Json] = Task.defer {
    Parse
      .parse(string) match {
      case Right(json) => Task(json)
      case Left(error) => Task.raiseError(FromJsonException(error))
    }
  }

  implicit override def jsonToJsonObject(json: Json): Option[JsonObject] = json.obj

  implicit override def jsonToList(json: Json): Option[List[Json]] = json.array.map(_.toList)

  implicit override def jsonObjectToMap(json: JsonObject): Map[String, Json] = json.toMap

  implicit override def jsonToString(json: Json): Option[String] = json.string

  implicit override def jsonToBoolean(json: Json): Option[Boolean] = json.bool

  implicit override def jsonToInt(json: Json): Option[Int] = json.number.flatMap(_.toInt)

  implicit override def jsonToDouble(json: Json): Option[Double] = json.number.flatMap(_.toDouble)

  implicit override def jsonToLong(json: Json): Option[Long] = json.number.flatMap(_.toLong)

  implicit override def jsonToGeopoint(json: Json): Option[Point] =
    lspace.decode.fromGeoJson(json).collect { case point: Point => point }

  implicit override def jsonToGeopolygon(json: Json): Option[Polygon] =
    lspace.decode.fromGeoJson(json).collect { case polygon: Polygon => polygon }

  val httpClient: HttpClient = HttpClientImpl
  override def fetch[T](iri: String)(cb: Map[String, Json] => Task[T]): Task[T] = {
    scribe.trace(s"fetch ${iri}")
    Task
      .fromTry(httpClient.getResource(iri) { content =>
        Parse
          .parseOption(content)
          .flatMap(_.obj)
          .map(_.toMap)
          .map(cb)
          .getOrElse(throw FromJsonException("could not parse"))
      })
      .flatten
  }
}
