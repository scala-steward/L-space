package lspace.structure

import lspace.datatype.IriType
import lspace.structure.util.ClassTypeable

trait IriResource {

  /**
    * An empty uri means that there is no URI assigned.
    * @return
    */
  def iri: String

  /**
    * @id is a IRI/URI identifier
    * @return a String which is empty if no @id is assigned, TODO: should this be an Option[String]?
    */
  def `@id` = iri

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
