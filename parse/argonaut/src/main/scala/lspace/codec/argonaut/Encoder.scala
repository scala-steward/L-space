package lspace.codec.argonaut

import argonaut._
import Argonaut._
import lspace.NS.types
import lspace.codec.{ActiveContext, ActiveProperty, JsonInProgress, JsonObjectInProgress}
import lspace.librarian.datatype.DataType
import lspace.librarian.structure._
import lspace.types.vector.Geometry

import scala.collection.immutable.ListMap

object Encoder extends Encoder {}
class Encoder extends lspace.codec.Encoder[Json] {

  override def getNewActiveContext: AC  = ActiveContext()
  override def getNewActiveProperty: AP = ActiveProperty()

  implicit override def nullToJson: Json                                     = Json.jNull
  implicit override def textToJson(text: String): Json                       = Json.jString(text)
  implicit override def boolToJson(boolean: Boolean): Json                   = Json.jBool(boolean)
  implicit override def intToJson(int: Int): Json                            = Json.jNumber(int)
  implicit override def doubleToJson(double: Double): Json                   = Json.jNumber(double)
  implicit override def longToJson(long: Long): Json                         = Json.jNumber(long)
  implicit override def geoToJson(geo: Geometry): Json                       = lspace.encode.GeometryCodecJson[Json](geo)
  implicit override def mapToJson(map: Map[String, Json]): Json              = map.asJson
  implicit override def listToJson(list: List[Json]): Json                   = list.asJson
  implicit override def listmapToJson(list: ListMap[String, Json]): Json     = Json.jObjectFields(list.toList: _*)
  implicit override def tuple2ToJson(tuples: (Json, Json)): Json             = tuples.asJson
  implicit override def tuple3ToJson(tuples: (Json, Json, Json)): Json       = tuples.asJson
  implicit override def tuple4ToJson(tuples: (Json, Json, Json, Json)): Json = tuples.asJson
  implicit override def tuple2ListToJson(tuples: List[(Json, Json)]): Json   = tuples.map(_.asJson).asJson

  private val printer = PrettyParams.nospace.copy(preserveOrder = true)
  override def apply[T <: Node](node: Node): String =
    printer.pretty(fromNode(node)(getNewActiveContext).withContext)
}
