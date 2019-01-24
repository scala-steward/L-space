package lspace.parse

import lspace.NS.types
import lspace.librarian.structure.IriResource

sealed abstract class `@container`(val iri: String) extends IriResource {
  override def toString: String = "@id"
}

object `@container` {
  case object `@id`       extends `@container`("@id")
  case object `@type`     extends `@container`("@type")
  case object `@list`     extends `@container`("@list")
  case object `@set`      extends `@container`("@set")
  case object `@language` extends `@container`("@language")
  case object `@index`    extends `@container`("@index")

  def apply(iri: String): Option[`@container`] =
    Option(iri match {
      case types.`@list`     => `@list`
      case types.`@id`       => `@id`
      case types.`@type`     => `@type`
      case types.`@set`      => `@set`
      case types.`@language` => `@language`
      case types.`@index`    => `@index`
      case _                 => null
    })
}
