package lspace.codec.circe

import io.circe.parser
import lspace.codec.exception.FromJsonException
import monix.eval.Task

import scala.util.Try

object NativeTypeDecoder extends NativeTypeDecoder {
  type Aux[Json0] = NativeTypeDecoder { type Json = Json0 }
}
class NativeTypeDecoder extends lspace.codec.NativeTypeDecoder {
  type Json = io.circe.Json

  def parse(string: String): Task[Json] = Task.defer {
    parser.parse(string) match {
      case Right(json) => Task(json)
      case Left(error) => Task.raiseError(FromJsonException(error.getMessage()))
    }
  }

  def jsonToMap(json: Json): Option[Map[String, Json]] = json.asObject.map(_.toMap)

  def jsonToList(json: Json): Option[List[Json]] = json.asArray.map(_.toList)

  def jsonToString(json: Json): Option[String] = json.asString

  def jsonToBoolean(json: Json): Option[Boolean] = json.asBoolean

  def jsonToInt(json: Json): Option[Int] = json.asNumber.flatMap(_.toInt)

  def jsonToDouble(json: Json): Option[Double] = json.asNumber.map(_.toDouble)

  def jsonToLong(json: Json): Option[Long] =
    json.asNumber
      .flatMap(n => n.toLong.orElse(n.toInt.map(_.toLong)))
      .orElse(json.asString.flatMap(s => Try(s.toLong).toOption))

}
