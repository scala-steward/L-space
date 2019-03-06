package lspace.codec.argonaut

import argonaut.Parse
import lspace.codec.exception.FromJsonException
import monix.eval.Task

import scala.util.Try

object NativeTypeDecoder extends NativeTypeDecoder {
  type Aux[Json0] = NativeTypeDecoder { type Json = Json0 }
}
class NativeTypeDecoder extends lspace.codec.NativeTypeDecoder {
  type Json = argonaut.Json

  def parse(string: String): Task[Json] = Task.defer {
    Parse
      .parse(string) match {
      case Right(json) => Task(json)
      case Left(error) =>
        Task.raiseError(FromJsonException(error))
    }
  }

  def jsonIsNull(json: Json): Boolean                  = json.isNull
  def jsonToMap(json: Json): Option[Map[String, Json]] = json.obj.map(_.toMap)

  def jsonToList(json: Json): Option[List[Json]] = json.array.map(_.toList)

  def jsonToString(json: Json): Option[String] = json.string

  def jsonToBoolean(json: Json): Option[Boolean] = json.bool

  def jsonToInt(json: Json): Option[Int] = json.number.flatMap(_.toInt)

  def jsonToDouble(json: Json): Option[Double] = json.number.flatMap(_.toDouble)

  def jsonToLong(json: Json): Option[Long] =
    json.number
      .flatMap(n => n.toLong.orElse(n.toInt.map(_.toLong)))
      .orElse(json.string.flatMap(s => Try(s.toLong).toOption))

}
