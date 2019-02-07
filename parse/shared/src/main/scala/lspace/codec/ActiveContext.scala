package lspace.codec

import lspace.NS.types
import lspace.codec.exception.FromJsonException
import lspace.librarian.structure.{ClassType, Property}

import scala.collection.immutable.ListMap

trait ActiveContext[Json, JsonObject] {
  def `@prefix`: ListMap[String, String]
  def `@vocab`: Option[String]
  def `@language`: Option[String]
  def `@base`: Option[String]
  def properties: Map[Property, ActiveProperty[Json, JsonObject]]

  def copy(`@prefix`: ListMap[String, String] = `@prefix`,
           `@vocab`: Option[String] = `@vocab`,
           `@language`: Option[String] = `@language`,
           `@base`: Option[String] = `@base`,
           properties: Map[Property, ActiveProperty[Json, JsonObject]] = properties): ActiveContext[Json, JsonObject]
  //TODO: map of expanded prefix map values (values can be compacted with leading prefixes)
  /**
    *
    * @param key
    * @return the compacted iri and a new context with possible additional prefixes
    */
  def compactIri(key: ClassType[_]): (String, ActiveContext[Json, JsonObject]) = {
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

  def expandKeys(obj: Map[String, Json]): Map[String, Json] = obj.map { case (key, value) => expandIri(key) -> value }

  def expectedType(property: Property) =
    properties
      .get(property)
      .flatMap(_.`@type`.headOption)
      .orElse(property.range.headOption)

  def jsonToString(json: Json): Option[String]
  def jsonToArray(json: Json): Option[List[Json]]
  def jsonToObject(json: Json): Option[JsonObject]
  def jsonObjectToMap(obj: JsonObject): Map[String, Json]

  def extractId(obj: Map[String, Json]) =
    obj.get(types.`@id`).flatMap(jsonToString).map(expandIri)

  def extractIds(obj: Map[String, Json]) =
    obj
      .get(types.`@ids`)
      .flatMap(
        json =>
          jsonToArray(json)
            .map(_.flatMap(jsonToString(_).orElse(throw FromJsonException("unknown key/iri format"))))
            .orElse(jsonToString(json).map(List(_))))
      .getOrElse(List())
      .map(expandIri)

  def extractLabels(obj: Map[String, Json]): Map[String, String] =
    obj
      .get(types.`@label`)
      .flatMap(
        json =>
          jsonToObject(json)
            .map(jsonObjectToMap(_).map {
              case (key, json) =>
                key -> jsonToString(json).getOrElse(throw FromJsonException("@label value is not a string"))
            })
            .orElse(jsonToString(json).map(l => Map("en" -> l))))
      .getOrElse(Map())

  def extractComments(obj: Map[String, Json]): Map[String, String] =
    obj
      .get(types.`@comment`)
      .flatMap(
        json =>
          jsonToObject(json)
            .map(jsonObjectToMap(_).map {
              case (key, json) =>
                key -> jsonToString(json).getOrElse(throw FromJsonException("@comment value is not a string"))
            })
            .orElse(jsonToString(json).map(l => Map("en" -> l))))
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
