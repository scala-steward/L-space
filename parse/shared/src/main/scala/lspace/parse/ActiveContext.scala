package lspace.parse

import argonaut.{Json, JsonObject}
import lspace.NS.types
import lspace.librarian.structure.{ClassType, Property}
import lspace.parse.util.FromJsonException

import scala.collection.immutable.ListMap

case class ActiveContext(`@prefix`: ListMap[String, String] = ListMap(),
                         `@vocab`: Option[String] = None,
                         `@language`: Option[String] = None,
                         `@base`: Option[String] = None,
                         properties: Map[Property, ActiveProperty] = Map()) {

  //TODO: map of expanded prefix map values (values can be compacted with leading prefixes)
  /**
    *
    * @param key
    * @return the compacted iri and a new context with possible additional prefixes
    */
  def compactIri(key: ClassType[_]): (String, ActiveContext) = {
    key.label.get(`@language`.getOrElse("en")) match {
      case Some(label) if label.nonEmpty =>
        val uriBase = key.iri.stripSuffix(label)
        if (uriBase.nonEmpty && uriBase != key.iri) {
          `@prefix`.find(_._2 == uriBase).map(_._1 + ":" + label).map(iri => iri -> this).getOrElse {
            val prefix = `@prefix`.size.toString
            s"$prefix:$label" -> this.copy(`@prefix` = `@prefix` + (prefix -> uriBase))
          }
        } else key.iri -> this
      case _ =>
        key.iri -> this
    }
  }

  def compactIri(iri: String): String = {
    val validPrefixes = `@prefix`.filter(pv => iri.startsWith(pv._2))
    if (validPrefixes.nonEmpty) {
      val (term, prefix) = validPrefixes.maxBy(_._2.length)
      val suffix         = iri.stripPrefix(prefix)
      term + ":" + suffix
    } else iri
  }

  /**
    *
    * @param term
    * @return
    */
  def expandIri(term: String): String = {
    val (prefix, suffix) =
      if (term.startsWith("http") || term.take(10).contains("://")) "" -> term
      else
        term.split(":") match {
          case Array(prefix, term) => prefix -> term
          case Array(term)         => ""     -> term
          case _                   => ""     -> term
        }
    val iri =
      if (prefix != "") `@prefix`.get(prefix).map(_ + suffix).getOrElse(suffix)
      else
        term //TODO: search vocabularies for matching terms, requires pre-fetching vocabularies or try assembled iri's (@vocab-iri + term)
    if (iri.startsWith("https")) iri
    else if (iri.startsWith("http")) iri.replaceFirst("http", "https")
    else iri
    //    else context.get(term).flatMap(_.get(types.id)).getOrElse(term)
  }

  def expandKeys(obj: JsonObject): Map[String, Json] = obj.toMap.map { case (key, value) => expandIri(key) -> value }

  def expectedType(property: Property) =
    properties
      .get(property)
      .flatMap(_.`@type`.headOption)
      .orElse(property.range.headOption)

  def extractId(obj: Map[String, Json]) =
    obj.get(types.`@id`).flatMap(_.string).map(expandIri)

  def extractIds(obj: Map[String, Json]) =
    obj
      .get(types.`@ids`)
      .flatMap(
        json =>
          json.array
            .map(_.flatMap(_.string.orElse(throw FromJsonException("unknown key/iri format"))))
            .orElse(json.string.map(List(_))))
      .getOrElse(List())
      .map(expandIri)

  def extractLabels(obj: Map[String, Json]): Map[String, String] =
    obj
      .get(types.`@label`)
      .flatMap(
        json =>
          json.obj
            .map(_.toMap.map {
              case (key, json) =>
                key -> json.string.getOrElse(throw FromJsonException("@label value is not a string"))
            })
            .orElse(json.string.map(l => Map("en" -> l))))
      .getOrElse(Map())

  def extractComments(obj: Map[String, Json]): Map[String, String] =
    obj
      .get(types.`@comment`)
      .flatMap(
        json =>
          json.obj
            .map(_.toMap.map {
              case (key, json) =>
                key -> json.string.getOrElse(throw FromJsonException("@comment value is not a string"))
            })
            .orElse(json.string.map(l => Map("en" -> l))))
      .getOrElse(Map())

  def extractContainer(obj: Map[String, Json]): Option[Json] =
    obj.get(types.`@container`)

  def extractValue[T](obj: Map[String, Json])(cb: Json => T): Option[T] =
    obj.get(types.`@value`).map(cb)

  def extractFrom(obj: Map[String, Json]): Option[Json] =
    obj.get(types.`@from`)

  def extractTo(obj: Map[String, Json]): Option[Json] =
    obj.get(types.`@to`)
}
