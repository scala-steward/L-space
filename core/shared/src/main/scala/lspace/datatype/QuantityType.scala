package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure.{ClassType, Property}

trait QuantityType[+T] extends StructuredType[T]

object QuantityType extends DataTypeDef[QuantityType[Any]] {

  lazy val datatype: QuantityType[Any] = new QuantityType[Any] {
    val iri: String = NS.types.`@quantity`
    labelMap ++= Map("en" -> NS.types.`@quantity`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(StructuredType.datatype)
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
