package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure.{ClassType, Property}

object NumericType extends DataTypeDef[NumericType[AnyVal]] {

  lazy val datatype: NumericType[AnyVal] = new NumericType[AnyVal] {
    val iri: String                = NS.types.`@number`
    override val iris: Set[String] = Set(NS.types.`@number`, NS.types.schemaNumber, "https://schema.org/Number")
    labelMap ++= Map("en" -> NS.types.schemaNumber)
    override protected def _extendedClasses: List[ClassType[Any]] = List(LiteralType.datatype)
  }

  object keys extends LiteralType.Properties
  override lazy val properties: List[Property] = LiteralType.properties
  trait Properties extends LiteralType.Properties

  def numType[T]: NumericType[T] = new NumericType[T] {
    val iri: String                                            = NS.types.`@number`
    override val iris: Set[String]                             = Set(NS.types.schemaNumber)
    override protected def _extendedClasses: List[ClassType[Any]] = List(LiteralType.datatype)
  }

  implicit def clsNumeric[T, CT[+Z] <: NumericType[Z]]: ClassTypeable.Aux[CT[T], T, NumericType[T]] =
    new ClassTypeable[CT[T]] {
      type C  = T
      type CT = NumericType[T]
      def ct: CT = NumericType.numType[T]
    }
}

trait NumericType[+T] extends LiteralType[T]
