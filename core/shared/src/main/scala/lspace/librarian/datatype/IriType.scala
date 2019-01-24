package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.{IriResource, Property}

trait IriType[+T] extends DataType[T]

object IriType extends DataTypeDef[IriType[IriResource]] {

  lazy val datatype = new IriType[IriResource] {
    val iri: String                                             = NS.types.`@url`
    override val iris: Set[String]                              = Set(NS.types.schemaURL, NS.types.xsdAnyURI)
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@url`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(DataType.datatype)
  }

  object keys extends DataType.Properties
  override lazy val properties: List[Property] = DataType.properties
  trait Properties extends DataType.Properties

  def apply[T]: IriType[T] = new IriType[T] { val iri: String = "" }

  implicit def clsIri[T]: ClassTypeable.Aux[IriType[T], T, IriType[T]] = new ClassTypeable[IriType[T]] {
    type C  = T
    type CT = IriType[T]
    def ct: CT = DataType.urlType[T]
  }
}
