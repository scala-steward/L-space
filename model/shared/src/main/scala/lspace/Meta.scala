package lspace

// opaque type Iri[iri <: String] = iri
// object Iri:
//   def apply[iri <: String](iri: iri): Iri[iri.type] = iri

//   extension [iri <: String](iri: Iri[iri]) def show: iri = iri
// end Iri
opaque type Iri = String
object Iri:
  def apply(iri: String): Iri = iri

  extension (iri: Iri)
    def unapply: String = iri
    def ++(string: String): Iri = iri + string

end Iri

opaque type Name = String

object Name:
  def apply(value: String): Name = value

  extension (name: Name) def unapply: String = name
end Name

opaque type Comment = String

object Comment:
  def apply(value: String): Comment = value

  extension (comment: Comment) def unapply: String = comment
end Comment
