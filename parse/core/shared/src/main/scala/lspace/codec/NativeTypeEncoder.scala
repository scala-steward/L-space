package lspace.codec

import lspace.types.vector.Geometry

import scala.collection.immutable.ListMap

object NativeTypeEncoder {
  type Aux[Json0] = NativeTypeEncoder { type Json = Json0 }
}
trait NativeTypeEncoder {
  type Json

  implicit def jNull: Json
  implicit def encode(text: String): Json
  implicit def encode(boolean: Boolean): Json
  implicit def encode(int: Int): Json
  implicit def encode(double: Double): Json
  implicit def encode(long: Long): Json
  implicit def encode(geo: Geometry): Json

  implicit def encode(map: Map[String, Json]): Json
  implicit def encode(map: ListMap[String, Json]): Json
  implicit def encode(list: List[Json]): Json
  implicit def encode(tuples: (Json, Json)): Json
  implicit def encode(tuples: (Json, Json, Json)): Json
  implicit def encode(tuples: (Json, Json, Json, Json)): Json
  implicit def encodeTupleList(tuples: List[(Json, Json)]): Json

  implicit def jsonToNoSpacesString(json: Json): String

  implicit class WithT[T](v: T)(implicit f: T => Json) {
    def asJson: Json = f(v)
  }
}
