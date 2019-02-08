//package lspace.parse
//
//import argonaut._
//import Argonaut._
//import lspace.NS.types
//
//import scala.collection.immutable.ListMap
//
////case class JsonObjectInProgress(json: JsonObject, activeContext: ActiveContext) {
////  def map(cb: (JsonObject, ActiveContext) => JsonObjectInProgress): JsonObjectInProgress =
////    cb(json, activeContext)
////  def map(cb: (JsonObject, ActiveContext) => JsonInProgress): JsonInProgress =
////    cb(json, activeContext)
////}
//
//object JsonObjectInProgress {
////  def apply(json: JsonObject, doubledefinitionWA: Int = 1)(
////      implicit activeContext: ActiveContext): lspace.codec.JsonObjectInProgress[Json, JsonObject] =
////    lspace.codec.JsonObjectInProgress[Json, JsonObject](json)(activeContext)
//
//  implicit class WithJsonObject(jip: lspace.codec.JsonObjectInProgress[Json]) {
//    lazy val withContext: JsonObject = _context(jip.activeContext) match {
//      case Some(context) => JsonObject.fromTraversableOnce(ListMap(types.`@context` -> context) ++ jip.json.toMap)
//      case None          => jip.json
//    }
//    lazy val withContextAsJson: Json = Json.jObject(withContext)
//
//    implicit class WithIriString(iri: String)(implicit activeContext: ActiveContext) {
//      lazy val compact: String = activeContext.compactIri(iri)
//    }
//
//    private def _context(activeContext: lspace.codec.ActiveContext[Json]): Option[Json] = {
//      val (newActiveContext, result) = activeContext.properties.foldLeft((activeContext, ListMap[String, Json]())) {
//        case ((activeContext, result), (key, activeProperty)) =>
//          val (keyIri, newActiveContext) = activeContext.compactIri(key)
//          List(
//            _context(activeProperty.`@context`).map(types.`@context` -> _),
//            _containers(activeProperty).map(types.`@container`       -> _),
//            _types(activeProperty).map(types.`@type`                 -> _)
//          ).flatten match {
//            case kv if kv.nonEmpty =>
//              newActiveContext -> (result ++ ListMap(
//                keyIri -> Json.jObject(JsonObject.fromTraversableOnce(ListMap(kv: _*)))))
//            case kv =>
//              newActiveContext -> result
//          }
//      }
//      JsonObject.fromTraversableOnce(newActiveContext.`@prefix`.map {
//        case (prefix, iri) => prefix -> iri.asJson
//      }.toList ++ result.toList) match {
//        case kv if kv.isNotEmpty => Some(Json.jObject(kv))
//        case kv                  => None
//      }
//    }
//
//    private def _containers(activeProperty: lspace.codec.ActiveProperty[Json]): Option[Json] = {
//      implicit val activeContext = activeProperty.`@context`
//      activeProperty.`@container` match {
//        case Nil             => None
//        case List(container) => Some(activeContext.compactIri(container.iri).asJson)
//        case containers =>
//          Some(activeProperty.`@container`.foldLeft(List[Json]()) {
//            case (result, container) => result :+ activeContext.compactIri(container.iri).asJson
//          }.asJson)
//      }
//    }
//
//    private def _types(activeProperty: lspace.codec.ActiveProperty[Json]): Option[Json] = {
//      implicit val activeContext = activeProperty.`@context`
//      activeProperty.`@type` match {
//        case Nil       => None
//        case List(tpe) => Some(activeContext.compactIri(tpe.iri).asJson)
//        case tpes =>
//          Some(activeProperty.`@type`.foldLeft(List[Json]()) {
//            case (result, tpe) => result :+ activeContext.compactIri(tpe.iri).asJson
//          }.asJson)
//      }
//    }
//  }
//}
