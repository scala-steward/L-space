package lspace.codec

case class JsonInProgress[Json](json: Json)(implicit val activeContext: ActiveContext) {}

object JsonInProgress {}
