package lspace.types.string

object Iri {
  implicit def iriToString(iri: Iri): String = iri.iri
}
case class Iri(iri: String) extends Identifier
