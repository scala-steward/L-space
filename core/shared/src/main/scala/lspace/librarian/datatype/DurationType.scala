package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.structure.Property
import squants.time.Time

object DurationType extends DataTypeDef[DurationType] {

  lazy val datatype: DurationType = new DurationType {
    val iri: String                                             = NS.types.`@duration`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@duration`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(QuantityType.datatype)
  }

  object keys extends QuantityType.Properties
  override lazy val properties: List[Property] = QuantityType.properties
  trait Properties extends QuantityType.Properties
}
trait DurationType extends QuantityType[Time]
