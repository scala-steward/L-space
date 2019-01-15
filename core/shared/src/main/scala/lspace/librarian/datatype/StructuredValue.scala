package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.Ontology

object StructuredValue extends DataTypeDef[StructuredValue[Any]] {

  lazy val datatype: StructuredValue[Any] = new StructuredValue[Any] {
    val iri: String                                             = NS.types.`@structured`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@structured`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(DataType.datatype)
  }

  object keys
//  lazy val ontology =
//    Ontology(NS.types.`@structured`, extendedClasses = List(DataType.ontology))

  def apply[T]: StructuredValue[T] = new StructuredValue[T] {
    val iri: String                                             = NS.types.`@structured`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@structured`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(DataType.datatype)
  }

  implicit def clsStructured[T]: ClassTypeable.Aux[StructuredValue[T], T, StructuredValue[T]] =
    new ClassTypeable[StructuredValue[T]] {
      type C  = T
      type CT = StructuredValue[T]
      def ct: CT = apply[T]
    }
//  implicit def default[T, CT[+Z] <: ClassType[Z]](implicit ev: CT[T] =:= StructuredValue[Any]): StructuredValue[T] =
//    structuredType[T]
}

trait StructuredValue[+T] extends DataType[T]
