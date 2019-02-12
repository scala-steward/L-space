package lspace.codec

import monix.eval.Task

object NativeTypeDecoder {
  type Aux[Json0] = NativeTypeDecoder { type Json = Json0 }
}
trait NativeTypeDecoder {
  type Json
  def parse(string: String): Task[Json]

  implicit def jsonToList(json: Json): Option[List[Json]]
  implicit def jsonToMap(json: Json): Option[Map[String, Json]]
  implicit def jsonToString(json: Json): Option[String]
  implicit def jsonToBoolean(json: Json): Option[Boolean]
  implicit def jsonToInt(json: Json): Option[Int]
  implicit def jsonToDouble(json: Json): Option[Double]
  implicit def jsonToLong(json: Json): Option[Long]
}
