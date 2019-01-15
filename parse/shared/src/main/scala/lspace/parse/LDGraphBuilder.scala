package lspace.parse

import lspace.librarian.structure.{ClassType, Ontology, Property}
import lspace.NS.types

import scala.collection.immutable.HashMap

object LDGraphBuilder {
  case class PropertyMod(context: Context = Context(),
                         tpe: Option[ClassType[_]] = None,
                         container: Option[String] = None) {
    def prefix: Map[String, String]          = context.prefix
    def vocab: Option[String]                = context.vocab
    def property: Map[Property, PropertyMod] = context.property
  }
  case class Context(prefix: Map[String, String] = Map(),
                     vocab: Option[String] = None,
                     language: Option[String] = None,
                     base: Option[String] = None,
                     property: Map[Property, PropertyMod] = Map()) {}
}
import LDGraphBuilder._

/**
  *
  * @param context prefix -> [@id -> baseUri]
  * @param properties
  * @param typeMods
  */
case class LDGraphBuilder(context: LDGraphBuilder.Context = LDGraphBuilder.Context(),
                          //  containerModes: HashMap[Property, String] = HashMap[Property, String](),
                          iris: Set[String] = Set[String]()) {

  /**
    *
    * @param term
    * @return
    */
  def expandIri(term: String, path: Vector[Property] = Vector()): String = {

    val (prefix, suffix) =
      if (term.startsWith("http") || term.take(10).contains("://")) "" -> term
      else
        term.split(":") match {
          case Array(prefix, term) => prefix -> term
          case Array(term)         => ""     -> term
          case _                   => ""     -> term
        }
    val iri =
      if (prefix != "") context.prefix.get(prefix).map(_ + suffix).getOrElse(suffix)
      //      {
      //        contexts.reverse.collectFirst {
      //          case c if path.nonEmpty && c.property.get(path.last).map(_.prefix.contains(prefix)).getOrElse(false) => c.property(path.last).prefix(prefix)
      //          case c if c.prefix.contains(prefix) => c.prefix(prefix)
      //        }.map(_ + suffix).getOrElse(term)
      //      }
      else
        term //TODO: search vocabularies for matching terms, requires pre-fetching vocabularies or try assembled iri's (@vocab-iri + term)
    if (iri.startsWith("https")) iri
    else if (iri.startsWith("http")) iri.replaceFirst("http", "https")
    else iri
    //    else context.get(term).flatMap(_.get(types.id)).getOrElse(term)
  }
}
