package lspace.datatype

import lspace.NS
import lspace.structure.Property

object DurationType extends DataTypeDef[DurationType[Any]] {

  lazy val datatype: DurationType[Any] = new DurationType[Any] {
    val iri: String = NS.types.`@duration`
    override val iris: Set[String] =
      Set(NS.types.`@duration`, NS.types.schemaDuration, "http://schema.org/Duration", NS.types.xsdDuration)
    labelMap ++= Map("en" -> NS.types.`@duration`)
    override lazy val _extendedClasses: List[_ <: DataType[_]] = List(QuantityType.datatype)
  }

  object keys extends QuantityType.Properties
  override lazy val properties: List[Property] = QuantityType.properties
  trait Properties extends QuantityType.Properties
}
trait DurationType[+T] extends QuantityType[T]
