package lspace.types.string

object Blank {
  implicit def blankToString(blank: Blank): String = blank.iri
}
case class Blank(iri: String) extends Identifier
