package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._

object DoubleType extends DataTypeDef[DoubleType[Double]] {

  lazy val datatype: DoubleType[Double] = new DoubleType[Double] {
    val iri: String = NS.types.`@double`
    override val iris: Set[String] =
      Set(NS.types.`@double`, NS.types.schemaFloat, "http://schema.org/Float", NS.types.xsdDouble)
    labelMap = Map("en" -> NS.types.`@double`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(NumericType.datatype)
  }

  object keys extends NumericType.Properties
  override lazy val properties: List[Property] = NumericType.properties
  trait Properties extends NumericType.Properties

  implicit val defaultDoubleTypeDouble: ClassTypeable.Aux[DoubleType[Double], Double, DoubleType[Double]] =
    new ClassTypeable[DoubleType[Double]] {
      type C  = Double
      type CT = DoubleType[Double]
      def ct: CT = datatype
    }
}
trait DoubleType[+T] extends NumericType[T]
