package lspace.librarian.structure

trait IriResource {

  /**
    * An empty uri means that there is no URI assigned.
    * @return
    */
  def iri: String

  override def equals(o: scala.Any): Boolean = o match {
    case resource: IriResource =>
      resource.iri == iri
    case _ => false
  }
}

object IriResource {
  def apply(_iri: String): IriResource = new IriResource {
    val iri: String = _iri
  }

}
