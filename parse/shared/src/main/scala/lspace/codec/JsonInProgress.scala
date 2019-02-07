package lspace.codec

case class JsonInProgress[Json, JsonObject](json: Json)(implicit val activeContext: ActiveContext[Json, JsonObject]) {
  def map(cb: (Json, ActiveContext[Json, JsonObject]) => JsonObjectInProgress[Json, JsonObject])
    : JsonObjectInProgress[Json, JsonObject] =
    cb(json, activeContext)
  def map(cb: (Json, ActiveContext[Json, JsonObject]) => JsonInProgress[Json, JsonObject])
    : JsonInProgress[Json, JsonObject] =
    cb(json, activeContext)
}
