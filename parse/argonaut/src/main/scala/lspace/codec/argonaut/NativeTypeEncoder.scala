package lspace.codec.argonaut

import argonaut.{Json, PrettyParams}
import lspace.types.vector.Geometry

import scala.collection.immutable.ListMap
import shapeless.syntax.std.tuple._

object NativeTypeEncoder extends NativeTypeEncoder {
  type Aux[Json0] = NativeTypeDecoder { type Json = Json0 }
}
class NativeTypeEncoder extends lspace.codec.NativeTypeEncoder {
  type Json = argonaut.Json

  implicit override def jNull: Json                                       = Json.jNull
  implicit override def encode(text: String): Json                        = Json.jString(text)
  implicit override def encode(boolean: Boolean): Json                    = Json.jBool(boolean)
  implicit override def encode(int: Int): Json                            = Json.jNumber(int)
  implicit override def encode(double: Double): Json                      = Json.jNumber(double)
  implicit override def encode(long: Long): Json                          = Json.jNumber(long)
  implicit override def encode(geo: Geometry): Json                       = lspace.encode.GeometryCodecJson[Json](geo)(this)
  implicit override def encode(map: Map[String, Json]): Json              = Json.jObjectFields(map.toList: _*)
  implicit override def encode(list: List[Json]): Json                    = Json.jArrayElements(list: _*)
  implicit override def encode(list: ListMap[String, Json]): Json         = Json.jObjectFields(list.toList: _*)
  implicit override def encode(tuples: (Json, Json)): Json                = Json.jArrayElements(tuples.toList: _*)
  implicit override def encode(tuples: (Json, Json, Json)): Json          = Json.jArrayElements(tuples.toList: _*)
  implicit override def encode(tuples: (Json, Json, Json, Json)): Json    = Json.jArrayElements(tuples.toList: _*)
  implicit override def encodeTupleList(tuples: List[(Json, Json)]): Json = encode(tuples.map(encode))

  private val printer                          = PrettyParams.nospace.copy(preserveOrder = true)
  def jsonToNoSpacesString(json: Json): String = printer.pretty(json)
}
