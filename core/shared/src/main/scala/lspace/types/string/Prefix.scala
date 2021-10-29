package lspace.types.string

object Prefix {
  implicit def prefixToString(prefix: Prefix): String = prefix.iri
  implicit def stringToPrefix(iri: String): String    = Prefix(iri)
}

case class Prefix(iri: String) extends AnyVal {
  def +(path: String): String = Iri(iri + path).iri // TODO: return Iri
}
