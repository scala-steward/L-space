package lspace.datatype

import java.time.LocalDateTime

import lspace.NS
import lspace.structure.Property
import lspace.structure.util.ClassTypeable

object LocalDateTimeType extends DataTypeDef[DateTimeType[LocalDateTime]] {

  lazy val datatype: DateTimeType[LocalDateTime] = new DateTimeType[LocalDateTime] {
    val iri: String                = NS.types.`@localdatetime`
    override val iris: Set[String] = Set(NS.types.`@localdatetime`)
    labelMap = Map("en" -> NS.types.`@localdatetime`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CalendarType.datatype)
  }

  object keys extends CalendarType.Properties
  override lazy val properties: List[Property] = CalendarType.properties
  trait Properties extends CalendarType.Properties

  implicit val defaultLocalDateTimeType
    : ClassTypeable.Aux[DateTimeType[LocalDateTime], LocalDateTime, DateTimeType[LocalDateTime]] =
    new ClassTypeable[DateTimeType[LocalDateTime]] {
      type C  = LocalDateTime
      type CT = DateTimeType[LocalDateTime]
      def ct: CT = datatype
    }
}
