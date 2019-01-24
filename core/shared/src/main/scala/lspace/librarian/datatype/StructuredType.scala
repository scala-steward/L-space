package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.Ontology

object StructuredType extends DataTypeDef[StructuredType[Any]] {

  lazy val datatype: StructuredType[Any] = new StructuredType[Any] {
    val iri: String                                             = NS.types.`@structured`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@structured`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(DataType.datatype)
  }

  object keys
//  lazy val ontology =
//    Ontology(NS.types.`@structured`, extendedClasses = List(DataType.ontology))

  def apply[T]: StructuredType[T] = new StructuredType[T] {
    val iri: String                                             = NS.types.`@structured`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@structured`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(DataType.datatype)
  }

  implicit def clsStructured[T]: ClassTypeable.Aux[StructuredType[T], T, StructuredType[T]] =
    new ClassTypeable[StructuredType[T]] {
      type C  = T
      type CT = StructuredType[T]
      def ct: CT = apply[T]
    }
//  implicit def default[T, CT[+Z] <: ClassType[Z]](implicit ev: CT[T] =:= StructuredValue[Any]): StructuredValue[T] =
//    structuredType[T]
}

trait StructuredType[+T] extends DataType[T]
