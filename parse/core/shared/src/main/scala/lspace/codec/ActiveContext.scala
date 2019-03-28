package lspace.codec

import lspace.NS.types
import lspace.codec.exception.FromJsonException
import lspace.structure.{ClassType, Ontology, Property}
import lspace.types.string.{Blank, Identifier, Iri}
import shapeless.tag.@@

import scala.collection.immutable.ListMap

//trait ActiveContext[Json] {
//  def `@prefix`: ListMap[String, String]
//  def `@vocab`: Option[String]
//  def `@language`: Option[String]
//  def `@base`: Option[String]
//  def properties: Map[Property, ActiveProperty[Json]]
case class ActiveContext(`@prefix`: ListMap[String, String] = ListMap[String, String](),
                         `@vocab`: List[String] = List(),
                         `@language`: List[String] = List(),
                         `@base`: Option[Option[String]] = None,
                         definitions: Map[String, ActiveProperty] = Map[String, ActiveProperty]()) {

  //TODO: map of expanded prefix map values (values can be compacted with leading prefixes)

  def asJson[Json](implicit encoder: NativeTypeEncoder.Aux[Json]): Option[Json] = ActiveContext.toJson(this)

  /**
    *
    * @param key
    * @return the compacted iri and a new context with possible additional prefixes
    */
  def compactIri(key: ClassType[_], language: String = "en"): (String, ActiveContext) = {
    key.label(`@language`.headOption.getOrElse(language)) match {
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
  def expandIri(term: String): Identifier = {
    val (prefix, suffix) = term.split(":") match {
      case Array(prefix, term) => prefix -> term
      case Array(term)         => ""     -> term
      case _                   => ""     -> term
    }
    if (prefix.startsWith("_")) Blank(term)
    else if (suffix.startsWith("//")) Iri(term)
    else {
      val iri =
        if (prefix != "") {
          `@prefix`.get(prefix).map(_ + suffix).getOrElse(suffix)
        } else
          `@prefix`.get(term)
            .orElse(
              `@vocab`.toStream
                .map(_ + term)
                .flatMap(ClassType.classtypes.get) //search vocabularies for matching terms, requires pre-fetching vocabularies or try assembled iri's (@vocab-iri + term)
                .headOption
                .map(_.iri)
            )
            .orElse(
              `@base`.headOption.map(_ + term)
            )
            .getOrElse(term)
      if (iri.startsWith("https")) Iri(iri)
      else if (iri.startsWith("http")) Iri(iri.replaceFirst("http", "https"))
      else Iri(iri)
    }
  }

//  def expandKeys[Json](obj: Map[String, Json]): Map[String, Json] = obj.map {
//    case (key, value) => expandIri(key) -> value
//  }

  def expectedType(property: Property) =
    definitions
      .get(property.iri)
      .flatMap(_.`@type`.headOption)
//      .orElse(property.range().headOption) // @range is only meant for guidance and not used for de-/serialization

//  def jsonToString(json: Json): Option[String]
//  def jsonToArray(json: Json): Option[List[Json]]
//  def jsonToMap(json: Json): Option[Map[String, Json]]

//  def extractId[Json](obj: ExpandedMap[Json])(implicit decoder: NativeTypeDecoder.Aux[Json]) =
//    obj.get(types.`@id`).flatMap(decoder.jsonToString(_)).map(expandIri)
//
//  def extractIds[Json](obj: ExpandedMap[Json])(implicit decoder: NativeTypeDecoder.Aux[Json]) =
//    obj
//      .get(types.`@ids`)
//      .flatMap(
//        json =>
//          decoder
//            .jsonToList(json)
//            .map(_.flatMap(decoder.jsonToString(_).orElse(throw FromJsonException("unknown key/iri format"))))
//            .orElse(decoder.jsonToString(json).map(List(_))))
//      .getOrElse(List())
//      .map(expandIri)
//
//  def extractLabels[Json](obj: ExpandedMap[Json])(implicit decoder: NativeTypeDecoder.Aux[Json]): Map[String, String] =
//    obj
//      .get(types.`@label`)
//      .flatMap(
//        json =>
//          decoder
//            .jsonToMap(json)
//            .map(_.map {
//              case (key, json) =>
//                key -> decoder.jsonToString(json).getOrElse(throw FromJsonException("@label value is not a string"))
//            })
//            .orElse(decoder.jsonToString(json).map(l => Map("en" -> l))))
//      .getOrElse(Map())
//
//  def extractComments[Json](obj: ExpandedMap[Json])(
//      implicit decoder: NativeTypeDecoder.Aux[Json]): Map[String, String] = {
//    obj
//      .get(types.`@comment`)
//      .flatMap(
//        json =>
//          decoder
//            .jsonToMap(json)
//            .map(_.map {
//              case (key, json) =>
//                key -> decoder.jsonToString(json).getOrElse(throw FromJsonException("@comment value is not a string"))
//            })
//            .orElse(decoder.jsonToString(json).map(l => Map("en" -> l))))
//      .getOrElse(Map())
//  }
//
//  def extractContainer[Json](obj: ExpandedMap[Json]): Option[Json] =
//    obj.get(types.`@container`)
//
//  def extractValue[Json, T](obj: ExpandedMap[Json])(cb: Json => T): Option[T] =
//    obj.get(types.`@value`).map(cb)
//
//  def extractFrom[Json](obj: ExpandedMap[Json]): Option[Json] =
//    obj.get(types.`@from`)
//
//  def extractTo[Json](obj: ExpandedMap[Json]): Option[Json] =
//    obj.get(types.`@to`)

  def ++(activeContext: ActiveContext): ActiveContext =
    this.copy(
      `@prefix` = `@prefix` ++ activeContext.`@prefix`,
      `@vocab` = `@vocab` ++ activeContext.`@vocab`,
      `@language` = `@language` ++ activeContext.`@language`,
      `@base` = activeContext.`@base`.orElse(`@base`)
    )
}

object ActiveContext {

  def toJson[Json](context: ActiveContext)(implicit encoder: NativeTypeEncoder.Aux[Json]): Option[Json] = {
    val (newActiveContext, result) = context.definitions.foldLeft((context, ListMap[String, Json]())) {
      case ((activeContext, result), (key, activeProperty)) =>
        val (keyIri, newActiveContext) = activeContext.compactIri(activeProperty.property)
        List(
          toJson(activeProperty.`@context`).map(types.`@context` -> _),
          _containers(activeProperty).map(types.`@container`     -> _),
          _types(activeProperty).map(types.`@type`               -> _)
        ).flatten match {
          case kv if kv.nonEmpty =>
            newActiveContext -> (result ++ ListMap(keyIri -> encoder.encode(ListMap(kv: _*))))
          case kv =>
            newActiveContext -> result
        }
    }
    ListMap(newActiveContext.`@prefix`.map {
      case (prefix, iri) => prefix -> encoder.encode(iri)
    }.toList ++ result.toList: _*) match {
      case kv if kv.nonEmpty => Some(encoder.encode(kv))
      case kv                => None
    }
  }

  implicit class WithIriString(iri: String)(implicit activeContext: ActiveContext) {
    lazy val compact: String = activeContext.compactIri(iri)
  }

  private def _containers[Json](activeProperty: ActiveProperty)(
      implicit encoder: NativeTypeEncoder.Aux[Json]): Option[Json] = {
    implicit val activeContext = activeProperty.`@context`
    activeProperty.`@container` match {
      case Nil             => None
      case List(container) => Some(encoder.encode(activeContext.compactIri(container.iri)))
      case containers =>
        Some(encoder.encode(activeProperty.`@container`.foldLeft(List[encoder.Json]()) {
          case (result, container) => result :+ encoder.encode(activeContext.compactIri(container.iri))
        }))
    }
  }

  private def _types[Json](activeProperty: ActiveProperty)(
      implicit encoder: NativeTypeEncoder.Aux[Json]): Option[Json] = {
    implicit val activeContext = activeProperty.`@context`
    activeProperty.`@type` match {
      case Nil       => None
      case List(tpe) => Some(encoder.encode(activeContext.compactIri(tpe.iri)))
      case tpes =>
        Some(encoder.encode(activeProperty.`@type`.foldLeft(List[encoder.Json]()) {
          case (result, tpe) => result :+ encoder.encode(activeContext.compactIri(tpe.iri))
        }))
    }
  }
}
