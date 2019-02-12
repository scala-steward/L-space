package lspace.codec.circe

import io.circe.Json
import lspace.types.vector.Geometry

import scala.collection.immutable.ListMap
import shapeless.syntax.std.tuple._

object NativeTypeEncoder extends NativeTypeEncoder {
  type Aux[Json0] = NativeTypeDecoder { type Json = Json0 }
}
class NativeTypeEncoder extends lspace.codec.NativeTypeEncoder {
  type Json = io.circe.Json

  implicit override def jNull: Json                    = Json.Null
  implicit override def encode(text: String): Json     = Json.fromString(text)
  implicit override def encode(boolean: Boolean): Json = Json.fromBoolean(boolean)
  implicit override def encode(int: Int): Json         = Json.fromInt(int)
  implicit override def encode(double: Double): Json =
    Json.fromDouble(double).get //Not all doubles can be represented as Json? Number-length limitation?
  implicit override def encode(long: Long): Json                          = Json.fromLong(long)
  implicit override def encode(geo: Geometry): Json                       = lspace.encode.GeometryCodecJson[Json](geo)(this)
  implicit override def encode(map: Map[String, Json]): Json              = Json.obj(map.toList: _*)
  implicit override def encode(list: List[Json]): Json                    = Json.arr(list: _*)
  implicit override def encode(list: ListMap[String, Json]): Json         = Json.obj(list.toList: _*)
  implicit override def encode(tuples: (Json, Json)): Json                = Json.arr(tuples.toList: _*)
  implicit override def encode(tuples: (Json, Json, Json)): Json          = Json.arr(tuples.toList: _*)
  implicit override def encode(tuples: (Json, Json, Json, Json)): Json    = Json.arr(tuples.toList: _*)
  implicit override def encodeTupleList(tuples: List[(Json, Json)]): Json = tuples.map(_.asJson).asJson

  def jsonToNoSpacesString(json: Json): String = json.noSpaces
}
