package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure.{IriResource, Ontology, Property}

trait IriType[+T] extends DataType[T]

object IriType extends DataTypeDef[IriType[IriResource]] {

  lazy val datatype = new IriType[IriResource] {
    val iri: String                = NS.types.`@url`
    override val iris: Set[String] = Set(NS.types.`@url`, NS.types.schemaURL, NS.types.xsdAnyURI)
    labelMap ++= Map("en" -> NS.types.`@url`)
    override lazy val _extendedClasses: List[_ <: DataType[_]] = List(DataType.datatype)
  }

  object keys
  override lazy val properties: List[Property] = List()
  trait Properties

  def apply[T]: IriType[T] = new IriType[T] { val iri: String = "" }

  import shapeless.=:!=
  implicit def clsIri[T](implicit classtpbl: ClassTypeable.Aux[T, T, IriType[T]],
                         ev1: T =:!= Ontology,
                         ev2: T =:!= Property): ClassTypeable.Aux[IriType[T], T, IriType[T]] =
    new ClassTypeable[IriType[T]] {
      type C  = T
      type CT = IriType[T]
      def ct: CT = classtpbl.ct
    }

  implicit val defaultOntology: ClassTypeable.Aux[IriType[Ontology], Ontology, IriType[Ontology]] =
    new ClassTypeable[IriType[Ontology]] {
      type C  = Ontology
      type CT = IriType[Ontology]
      def ct: CT = Ontology.urlType
    }

  implicit val defaultProperty: ClassTypeable.Aux[IriType[Property], Property, IriType[Property]] =
    new ClassTypeable[IriType[Property]] {
      type C  = Property
      type CT = IriType[Property]
      def ct: CT = Property.urlType
    }

}
