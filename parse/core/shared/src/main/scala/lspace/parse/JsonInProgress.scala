package lspace.parse

//import argonaut.{Json, JsonObject}
//import lspace.NS.types
//
//object JsonInProgress {
////  def apply(json: Json, doubledefinitionWA: Int = 1)(
////      implicit activeContext: ActiveContext): lspace.codec.JsonInProgress[Json, JsonObject] =
////    JsonInProgress(json, activeContext)
//
//  import JsonObjectInProgress._
//  implicit class WithJsonInProgress(jip: lspace.codec.JsonInProgress[Json, JsonObject]) {
//    lazy val withContext: JsonObject =
//      lspace.codec
//        .JsonObjectInProgress[Json, JsonObject](JsonObject.single(types.`@graph`, jip.json))(jip.activeContext)
//        .withContext
//  }
//}

//case class JsonInProgress(json: Json, activeContext: ActiveContext) {
//  def map(cb: (Json, ActiveContext) => JsonObjectInProgress): JsonObjectInProgress =
//    cb(json, activeContext)
//  def map(cb: (Json, ActiveContext) => JsonInProgress): JsonInProgress =
//    cb(json, activeContext)
//}
