package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.structure.{DataType, QuantityType}
import squants.time.Time

object DurationType extends DurationType {
  def default = DurationType
}
trait DurationType extends QuantityType[Time] {
  val iri: String = NS.types.`@duration`

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(QuantityType)
}
