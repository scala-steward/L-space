package lspace.codec

import lspace.NS.types

import scala.collection.immutable.ListMap

case class JsonInProgress[Json](json: Json)(implicit val activeContext: ActiveContext[Json]) {}

object JsonInProgress {

  implicit class WithJsonInProgress[Json](jip: JsonInProgress[Json])(implicit encoder: lspace.codec.Encoder[Json]) {

    /**
      * returns
      */
    lazy val withContext: Json = {
      encoder.listmapToJson(
        ListMap(jip.activeContext.asJson.map(types.`@context` -> _).toList: _*) ++ ListMap(types.`@graph` -> jip.json))
    }
  }
}
