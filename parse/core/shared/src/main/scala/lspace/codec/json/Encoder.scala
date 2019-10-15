package lspace.codec.json

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import lspace.types.geo.Geometry

import scala.collection.immutable.ListMap

object Encoder {
//  type Aux[Json0] = Encoder { type Json = Json0 }
}
trait Encoder[Json] extends lspace.codec.Encoder {
//  type Json
  lazy val geojsonEncoder = lspace.codec.json.geojson.GeoJsonEncoder(this)

  implicit def jNull: Json
  implicit def stringToJson: (String) => Json
  implicit def boolToJson: Boolean => Json
  implicit def intToJson: Int => Json
  implicit def doubleToJson: Double => Json
  implicit def longToJson: Long => Json
  implicit val geoToJson: Geometry => Json                = geo => geojsonEncoder.encodeGeometryObject(geo)
  implicit val dateToJson: Instant => Json                = (date: Instant) => date.toString.asJson
  implicit val localdatetimeToJson: LocalDateTime => Json = (date: LocalDateTime) => date.toString.asJson
  implicit val localdateToJson: LocalDate => Json         = (date: LocalDate) => date.toString.asJson
  implicit val localtimeToJson: LocalTime => Json         = (date: LocalTime) => date.toString.asJson

  implicit def mapToJson: Map[String, Json] => Json
  implicit def listMapToJson: ListMap[String, Json] => Json
  implicit def listToJson: List[Json] => Json
  implicit def listToJson2[T](list: List[T])(implicit f: T => Json): Json = list.map(f)
  implicit def t2ToJson: ((Json, Json)) => Json
  implicit def t3ToJson: ((Json, Json, Json)) => Json
  implicit def t4ToJson: ((Json, Json, Json, Json)) => Json
  implicit def tupleListToJson: List[(Json, Json)] => Json

  implicit def jsonToNoSpacesString: Json => String

  implicit class WithT[T](v: T)(implicit f: T => Json) {
    def asJson: Json = f(v)
  }

  implicit class WithEJson(json: Json) {
    def noSpaces: String = jsonToNoSpacesString(json)
  }
}
