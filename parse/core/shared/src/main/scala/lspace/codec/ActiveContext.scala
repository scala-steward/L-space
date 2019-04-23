package lspace.codec

import lspace.NS.types
import lspace.codec.exception.FromJsonException
import lspace.codec.jsonld.Encoder
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

  /**
    *
    * @param key
    * @return the compacted iri and a new context with possible additional prefixes
    */
  def compactIri(key: ClassType[_], language: String = "en"): (String, ActiveContext) = {
    val validPrefixes = `@prefix`.filter(pv => key.iri.startsWith(pv._2))
    if (validPrefixes.nonEmpty) {
      val (term, prefix) = validPrefixes.maxBy(_._2.length)
      val suffix         = key.iri.stripPrefix(prefix)
      (if (suffix.isEmpty) term else term + ":" + suffix) -> this
    } else {
      val (rLabel, rPrefix) = key.iri.reverse.span(_ != '/')
      if (rPrefix.nonEmpty) {
        val prefix = `@prefix`.size.toString
        s"$prefix:${rLabel.reverse}" -> this.copy(`@prefix` = `@prefix` + (prefix -> rPrefix.reverse))
      } else {
        key.iri -> this
      }
    }
//    key.label(`@language`.headOption.getOrElse(language)) match {
//      case Some(label) if label.nonEmpty =>
//        val uriBase = key.iri.stripSuffix(label)
//        if (uriBase.nonEmpty && uriBase != key.iri) {
//          `@prefix`.find(_._2 == uriBase).map(_._1 + ":" + label).map(iri => iri -> this).getOrElse {
//            val prefix = `@prefix`.size.toString
//            s"$prefix:$label" -> this.copy(`@prefix` = `@prefix` + (prefix -> uriBase))
//          }
//        } else key.iri -> this
//      case _ =>
//        key.iri -> this
//    }
  }

  def compactIri(iri: String): String = {
    val validPrefixes = `@prefix`.filter(pv => iri.startsWith(pv._2))
    if (validPrefixes.nonEmpty) {
      val (term, prefix) = validPrefixes.maxBy(_._2.length)
      val suffix         = iri.stripPrefix(prefix)
      if (suffix.isEmpty) term else term + ":" + suffix
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
          `@prefix`.get(prefix)
            .map(_ + suffix)
            .orElse(`@prefix`.get(term))
            .getOrElse(term) //throw FromJsonException(s"prefix not found ${prefix}"))
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
      else if (iri.startsWith("http:")) Iri(iri.replaceFirst("http:", "https:"))
      else Iri(iri)
    }
  }

  def expectedType(property: Property) =
    definitions
      .get(property.iri)
      .flatMap(_.`@type`.headOption)
//      .orElse(property.range().headOption) // @range is only meant for guidance and not used for de-/serialization

  def ++(activeContext: ActiveContext): ActiveContext =
    this.copy(
      `@prefix` = `@prefix` ++ activeContext.`@prefix`,
      `@vocab` = `@vocab` ++ activeContext.`@vocab`,
      `@language` = `@language` ++ activeContext.`@language`,
      `@base` = activeContext.`@base`.orElse(`@base`),
      definitions = definitions ++ activeContext.definitions
    )
}

object ActiveContext {

  implicit class WithIriString(iri: String)(implicit activeContext: ActiveContext) {
    lazy val compact: String = activeContext.compactIri(iri)
  }
}
