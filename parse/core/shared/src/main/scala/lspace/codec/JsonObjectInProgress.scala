package lspace.codec

case class JsonObjectInProgress[Json](json: List[(String, Json)])(implicit val activeContext: ActiveContext) {}

object JsonObjectInProgress {}
