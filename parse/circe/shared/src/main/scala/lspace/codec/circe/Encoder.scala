package lspace.codec.circe

import io.circe.Json
import lspace.codec.json

import scala.collection.immutable.ListMap
import shapeless.syntax.std.tuple._

object Encoder extends Encoder {}
class Encoder extends json.Encoder[Json] {

  implicit override def jNull: Json                  = Json.Null
  implicit override def stringToJson: String => Json = text => Json.fromString(text)
  implicit override def boolToJson: Boolean => Json  = boolean => Json.fromBoolean(boolean)
  implicit override def intToJson: Int => Json       = int => Json.fromInt(int)
  implicit override def doubleToJson: Double => Json =
    double => Json.fromDouble(double).get // Not all doubles can be represented as Json? Number-length limitation?
  implicit override def longToJson: Long => Json                     = long => Json.fromLong(long)
  implicit override def mapToJson: Map[String, Json] => Json         = map => Json.obj(map.toList: _*)
  implicit override def listToJson: List[Json] => Json               = list => Json.arr(list: _*)
  implicit override def listMapToJson: ListMap[String, Json] => Json = list => Json.obj(list.toList: _*)
  implicit override def t2ToJson: ((Json, Json)) => Json =
    tuple => Json.arr(tuple.toList: _*)
  implicit override def t3ToJson: ((Json, Json, Json)) => Json =
    (tuple: (Json, Json, Json)) => Json.arr(tuple.toList: _*)
  implicit override def t4ToJson: ((Json, Json, Json, Json)) => Json =
    (tuple: (Json, Json, Json, Json)) => Json.arr(tuple.toList: _*)
  implicit override def tupleListToJson: List[(Json, Json)] => Json =
    (tuples: List[(Json, Json)]) => tuples.map(_.asJson).asJson

  def jsonToNoSpacesString: Json => String = json => json.noSpaces
}
