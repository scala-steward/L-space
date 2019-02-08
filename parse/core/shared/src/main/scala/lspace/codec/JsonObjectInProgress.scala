package lspace.codec

import lspace.NS.types

import scala.collection.immutable.ListMap

case class JsonObjectInProgress[Json](json: Map[String, Json])(implicit val activeContext: ActiveContext[Json]) {}

object JsonObjectInProgress {

  implicit class WithJsonObjectInProgress[Json](joip: JsonObjectInProgress[Json])(
      implicit encoder: lspace.codec.Encoder[Json]) {

    /**
      * returns
      */
    lazy val withContext: Json = {
      val context = joip.activeContext.asJson

      if (context.isDefined) encoder.listmapToJson(ListMap(types.`@context` -> context.get) ++ joip.json)
      else encoder.mapToJson(joip.json)
    }
  }
}
