package lspace.librarian.structure

import lspace.librarian.datatype.IriType
import lspace.librarian.process.traversal.helper.ClassTypeable

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

  implicit def default: ClassTypeable.Aux[IriResource, IriResource, IriType[IriResource]] =
    new ClassTypeable[IriResource] {
      type C  = IriResource
      type CT = IriType[IriResource]
      def ct: CT = IriType[IriResource]
    }
}
