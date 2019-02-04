package lspace.parse

import argonaut.{Json, JsonObject}
import lspace.NS.types

object JsonInProgress {
  def apply(json: Json, doubledefinitionWA: Int = 1)(implicit activeContext: ActiveContext): JsonInProgress =
    JsonInProgress(json, activeContext)

  implicit class WithJsonInProgress(jip: JsonInProgress) {
    lazy val withContext: JsonObject =
      JsonObjectInProgress(JsonObject.single(types.`@graph`, jip.json), jip.activeContext).withContext
  }
}

case class JsonInProgress(json: Json, activeContext: ActiveContext) {
  def map(cb: (Json, ActiveContext) => JsonObjectInProgress): JsonObjectInProgress =
    cb(json, activeContext)
  def map(cb: (Json, ActiveContext) => JsonInProgress): JsonInProgress =
    cb(json, activeContext)
}
