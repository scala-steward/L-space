package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.Property

trait QuantityType[+T] extends StructuredType[T]

object QuantityType extends DataTypeDef[QuantityType[Any]] {

  lazy val datatype: QuantityType[Any] = new QuantityType[Any] {
    val iri: String                                             = NS.types.`@quantity`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@quantity`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(StructuredType.datatype)
  }

  object keys extends StructuredType.Properties
  override lazy val properties: List[Property] = StructuredType.properties
  trait Properties extends StructuredType.Properties

  implicit val clsQuantity: ClassTypeable.Aux[QuantityType[Any], Any, QuantityType[Any]] =
    new ClassTypeable[QuantityType[Any]] {
      type C  = Any
      type CT = QuantityType[Any]
      def ct: CT = datatype
    }
}