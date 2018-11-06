package lspace.types.string

import shapeless.tag
import shapeless.tag.@@

object Iri {
  def apply(v: String): String @@ Iri = tag[Iri](v)
}

trait Iri
