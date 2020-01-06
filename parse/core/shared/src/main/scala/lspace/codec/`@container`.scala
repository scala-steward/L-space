package lspace.codec

import lspace.NS.types
import lspace.structure.IriResource

sealed abstract class Container(val iri: String) extends IriResource {
  override def toString: String = "@id"
}

object Container {
  case object `@id`       extends `@container`("@id")
  case object `@type`     extends `@container`("@type")
  case object `@list`     extends `@container`("@list")
  case object `@set`      extends `@container`("@set")
  case object `@language` extends `@container`("@language")
  case object `@graph`    extends `@container`("@graph")
  case object `@index`    extends `@container`("@index")

  def apply(iri: String): Option[`@container`] =
    Option(iri match {
      case types.`@list`     => `@list`
      case types.`@id`       => `@id`
      case types.`@type`     => `@type`
      case types.`@set`      => `@set`
      case types.`@language` => `@language`
      case types.`@graph`    => `@graph`
      case types.`@index`    => `@index`
      case _                 => null
    })
}
