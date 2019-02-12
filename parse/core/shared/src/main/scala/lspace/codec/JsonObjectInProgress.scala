package lspace.codec

import lspace.NS.types

import scala.collection.immutable.ListMap

case class JsonObjectInProgress[Json](json: List[(String, Json)])(implicit val activeContext: ActiveContext) {}

object JsonObjectInProgress {

  implicit class WithJsonObjectInProgress[Json](joip: JsonObjectInProgress[Json])(
      implicit encoder: lspace.codec.NativeTypeEncoder.Aux[Json]) {

    /**
      * returns
      */
    lazy val withContext: Json = {
      val context = joip.activeContext.asJson

      if (context.isDefined)
        encoder.encode(ListMap(types.`@context` -> context.get) ++ joip.json)
      else encoder.encode(ListMap[String, Json]() ++ joip.json)
    }
  }
}
