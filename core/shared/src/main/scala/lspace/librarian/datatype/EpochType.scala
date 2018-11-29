package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.structure.{CalendarType, DataType}

object EpochType extends EpochType {
  def default = EpochType
}
trait EpochType extends CalendarType[Long] {
  val iri: String = NS.types.`@epoch`

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CalendarType)
}
