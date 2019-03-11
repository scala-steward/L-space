package lspace.types.string

trait Identifier extends Product with Serializable {
  def iri: String
}
