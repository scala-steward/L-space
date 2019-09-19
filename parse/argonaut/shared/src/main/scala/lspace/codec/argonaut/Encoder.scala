package lspace.codec.argonaut

import argonaut.{Json, PrettyParams}
import lspace.codec.json
import lspace.types.geo.Geometry

import scala.collection.immutable.ListMap
import shapeless.syntax.std.tuple._

object Encoder extends Encoder {}
class Encoder extends json.JsonEncoder[Json] {

  implicit override def jNull: Json                                  = Json.jNull
  implicit override def stringToJson: String => Json                 = text => Json.jString(text)
  implicit override def boolToJson: Boolean => Json                  = boolean => Json.jBool(boolean)
  implicit override def intToJson: Int => Json                       = int => Json.jNumber(int)
  implicit override def doubleToJson: Double => Json                 = double => Json.jNumber(double)
  implicit override def longToJson: Long => Json                     = long => Json.jNumber(long)
  implicit override def geoToJson: Geometry => Json                  = geo => lspace.encode.GeometryCodecJson[Json](geo)(this)
  implicit override def mapToJson: Map[String, Json] => Json         = map => Json.jObjectFields(map.toList: _*)
  implicit override def listToJson: List[Json] => Json               = list => Json.jArrayElements(list: _*)
  implicit override def listMapToJson: ListMap[String, Json] => Json = list => Json.jObjectFields(list.toList: _*)
  implicit override def t2ToJson: ((Json, Json)) => Json =
    tuple => Json.jArrayElements(tuple.toList: _*)
  implicit override def t3ToJson: ((Json, Json, Json)) => Json =
    (tuple: (Json, Json, Json)) => Json.jArrayElements(tuple.toList: _*)
  implicit override def t4ToJson: ((Json, Json, Json, Json)) => Json =
    (tuple: (Json, Json, Json, Json)) => Json.jArrayElements(tuple.toList: _*)
  implicit override def tupleListToJson: List[(Json, Json)] => Json =
    (tuples: List[(Json, Json)]) => tuples.map(_.asJson).asJson

  private val printer                      = PrettyParams.nospace.copy(preserveOrder = true)
  def jsonToNoSpacesString: Json => String = json => printer.pretty(json)
}
