package lspace.parse

//import argonaut.{Json, JsonObject}
//import lspace.codec
//import lspace.librarian.structure.{ClassType, Property}
//
//import scala.collection.immutable.ListMap

//case class ActiveContext(`@prefix`: ListMap[String, String] = ListMap(),
//                         `@vocab`: Option[String] = None,
//                         `@language`: Option[String] = None,
//                         `@base`: Option[String] = None,
//                         properties: Map[Property, codec.ActiveProperty[Json, JsonObject]] = Map())
//    extends lspace.codec.ActiveContext[Json, JsonObject] {
//
//  //TODO: map of expanded prefix map values (values can be compacted with leading prefixes)
//  override def copy(
//      `@prefix`: ListMap[String, String],
//      `@vocab`: Option[String],
//      `@language`: Option[String],
//      `@base`: Option[String],
//      properties: Map[Property, codec.ActiveProperty[Json, JsonObject]]): codec.ActiveContext[Json, JsonObject] =
//    ActiveContext(`@prefix`, `@vocab`, `@language`, `@base`, properties)
//
//  override def jsonToString(json: Json): Option[String] = json.string
//
//  override def jsonToArray(json: Json): Option[List[Json]] = json.array.map(_.toList)
//
//  override def jsonToObject(json: Json): Option[JsonObject] = json.obj
//
//  override def jsonObjectToMap(obj: JsonObject): Map[String, Json] = obj.toMap
//}
