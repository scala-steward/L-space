package lspace.codec.argonaut

import argonaut.{Json, JsonObject, Parse}
import lspace.codec
import lspace.codec.{ActiveContext, ActiveProperty}
import lspace.codec.exception.FromJsonException
import lspace.librarian.structure.Graph
import lspace.types.vector.{Point, Polygon}
import monix.eval.Task

object Decoder {
  def apply(graph: Graph): Decoder = new Decoder(graph)
}
class Decoder(val graph: Graph) extends lspace.codec.Decoder[Json] {

  override def encoder: codec.Encoder[Json] = Encoder

  override def getNewActiveContext: AC = ActiveContext()

  override def getNewActiveProperty: AP = ActiveProperty()

  override def parse(string: String): Task[Json] = Task.defer {
    Parse
      .parse(string) match {
      case Right(json) => Task(json)
      case Left(error) => Task.raiseError(FromJsonException(error))
    }
  }

  implicit override def jsonToMap(json: Json): Option[Map[String, Json]] = json.obj.map(_.toMap)

  implicit override def jsonToList(json: Json): Option[List[Json]] = json.array.map(_.toList)

  implicit override def jsonToString(json: Json): Option[String] = json.string

  implicit override def jsonToBoolean(json: Json): Option[Boolean] = json.bool

  implicit override def jsonToInt(json: Json): Option[Int] = json.number.flatMap(_.toInt)

  implicit override def jsonToDouble(json: Json): Option[Double] = json.number.flatMap(_.toDouble)

  implicit override def jsonToLong(json: Json): Option[Long] = json.number.flatMap(_.toLong)

}
