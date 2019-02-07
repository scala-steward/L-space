package lspace.codec

case class JsonObjectInProgress[Json, JsonObject](json: JsonObject)(
    val activeContext: ActiveContext[Json, JsonObject]) {
//  def map(cb: (JsonObject, ActiveContext[Json, JsonObject]) => JsonObjectInProgress[Json, JsonObject])
//    : JsonObjectInProgress[Json, JsonObject] =
//    cb(json, activeContext)
//  def map(cb: (JsonObject, ActiveContext[Json, JsonObject]) => JsonInProgress[Json, JsonObject])
//    : JsonInProgress[Json, JsonObject] =
//    cb(json, activeContext)
}
