package lspace.codec

import lspace.structure.{ClassType, Property}
import lspace.types.string.{Blank, Identifier, Iri}

import scala.collection.immutable.ListMap

object ActiveContext {
  def apply(`@prefix`: ListMap[String, String] = ListMap[String, String](),
            `@vocab`: List[String] = List(),
            `@language`: List[String] = List(),
            `@base`: Option[Option[String]] = None,
            definitions: Map[String, ActiveProperty] = Map[String, ActiveProperty](),
            remotes: List[NamedActiveContext] = List()): ActiveContext =
    new ActiveContext(`@prefix`, `@vocab`, `@language`, `@base`, definitions, remotes)

  implicit class WithIriString(iri: String)(implicit activeContext: ActiveContext) {
    lazy val compact: String = activeContext.compactIri(iri)
  }
}

class ActiveContext(`@prefix0`: ListMap[String, String] = ListMap[String, String](),
                    `@vocab0`: List[String] = List(),
                    `@language0`: List[String] = List(),
                    val `@base`: Option[Option[String]] = None,
                    definitions0: Map[String, ActiveProperty] = Map[String, ActiveProperty](),
                    val remotes: List[NamedActiveContext]) {

  override def equals(o: Any): Boolean = o match {
    case activeContext: NamedActiveContext => false
    case activeContext: ActiveContext =>
      `@prefix`() == activeContext
        .`@prefix`() && `@vocab`() == activeContext.`@vocab`() && `@language`() == activeContext
        .`@language`() && `@base` == activeContext.`@base` && definitions() == activeContext
        .definitions() && remotes == activeContext.remotes
    case _ => false
  }

  object `@prefix` {
    lazy val all: ListMap[String, String] = `@prefix0` ++ remotes.flatMap(_.`@prefix`.all)
    def apply(): ListMap[String, String]  = `@prefix0`
    def get(prefix: String): Option[String] =
      `@prefix0`.get(prefix).orElse(remotes.reverse.toStream.flatMap(_.`@prefix`.get(prefix)).headOption)
    def prefixOptions(value: String): ListMap[String, String] =
      `@prefix0`.filter(pv => value.startsWith(pv._2)) ++ remotes.flatMap(_.`@prefix`.prefixOptions(value))
  }

  object `@vocab` {
    lazy val all: List[String] = `@vocab0` ++ remotes.flatMap(_.`@vocab`.all)
    def apply(): List[String]  = `@vocab0`
  }

  object `@language` {
    lazy val all: List[String] = `@language0` ++ remotes.flatMap(_.`@language`.all)
    def apply(): List[String]  = `@language0`
  }

  object definitions {
    lazy val all: Map[String, ActiveProperty] = definitions0 ++ remotes.flatMap(_.definitions.all)
    def apply(): Map[String, ActiveProperty]  = definitions0
    def get(iri: String): Option[ActiveProperty] =
      definitions0.get(iri).orElse(remotes.reverse.flatMap(_.definitions.get(iri)).headOption)
  }

  //TODO: map of expanded prefix map values (values can be compacted with leading prefixes)

  def copy(`@prefix`: ListMap[String, String] = this.`@prefix`(),
           `@vocab`: List[String] = this.`@vocab`(),
           `@language`: List[String] = this.`@language`(),
           `@base`: Option[Option[String]] = `@base`,
           definitions: Map[String, ActiveProperty] = this.definitions(),
           remotes: List[NamedActiveContext] = remotes): ActiveContext =
    ActiveContext(`@prefix`, `@vocab`, `@language`, `@base`, definitions, remotes)

  /**
    *
    * @param key
    * @return the compacted iri and a new context with possible additional prefixes
    */
  def compactIri(key: ClassType[_], language: String = "en"): (String, ActiveContext) = {
    val validPrefixes = `@prefix`.prefixOptions(key.iri)
    if (validPrefixes.nonEmpty) {
      val (term, prefix) = validPrefixes.maxBy(_._2.length)
      val suffix         = key.iri.stripPrefix(prefix)
      (if (suffix.isEmpty) term else term + ":" + suffix) -> this
    } else {
      val (rLabel, rPrefix) = key.iri.reverse.span(_ != '/')
      if (rPrefix.nonEmpty) {
//        val prefix = `@prefix`().size.toString
        val prefix = `@prefix`.all.size.toString
        s"$prefix:${rLabel.reverse}" -> this.copy(`@prefix` = `@prefix`() + (prefix -> rPrefix.reverse))
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
    val validPrefixes = `@prefix`.prefixOptions(iri)
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
              `@vocab`.all.toStream
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

  def expectedType(iri: String) =
    definitions
      .get(iri)
      .flatMap(_.`@type`.headOption)
//      .orElse(property.range().headOption) // @range is only meant for guidance and not used for de-/serialization

  def ++(activeContext: ActiveContext): ActiveContext =
    this.copy(
      `@prefix` = `@prefix`() ++ activeContext.`@prefix`(),
      `@vocab` = `@vocab`() ++ activeContext.`@vocab`(),
      `@language` = `@language`() ++ activeContext.`@language`(),
      `@base` = activeContext.`@base`.orElse(`@base`),
      definitions = definitions() ++ activeContext.definitions(),
      remotes = remotes ++ activeContext.remotes
    )
}
