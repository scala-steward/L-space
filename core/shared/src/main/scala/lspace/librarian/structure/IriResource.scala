package lspace.librarian.structure

import lspace.NS

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

object IriResource extends LowPriorityIriTypeImplicits {
  def apply(_iri: String): IriResource = new IriResource {
    val iri: String = _iri
  }

}

trait LowPriorityIriTypeImplicits {
//  implicit def dt[T <: IriResource](implicit ev: T <:< IriResource) = new IriType[T] {
//    val iri: String = ldcontext.types.DATATYPE
//  }
}

//object Iri {
//  implicit def asString(iri: Iri): String       = iri.iri
//  implicit def stringAsIri(iri: String): String = Iri(iri)
//}
//case class Iri(iri: String) extends IriResource
