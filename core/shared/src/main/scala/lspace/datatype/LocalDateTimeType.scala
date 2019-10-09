package lspace.datatype

import java.time.LocalDateTime

import lspace.NS
import lspace.structure.Property
import lspace.structure.util.ClassTypeable

object LocalDateTimeType extends DataTypeDef[LocalDateTimeType[LocalDateTime]] {

  lazy val datatype: LocalDateTimeType[LocalDateTime] = new LocalDateTimeType[LocalDateTime] {
    val iri: String                = NS.types.`@localdatetime`
    override val iris: Set[String] = Set(NS.types.`@localdatetime`)
    labelMap ++= Map("en" -> NS.types.`@localdatetime`)
    override lazy val _extendedClasses: List[_ <: DataType[_]] = List(CalendarType.datatype)
  }

  object keys extends CalendarType.Properties
  override lazy val properties: List[Property] = CalendarType.properties
  trait Properties extends CalendarType.Properties

  implicit val defaultLocalDateTimeType
    : ClassTypeable.Aux[LocalDateTimeType[LocalDateTime], LocalDateTime, LocalDateTimeType[LocalDateTime]] =
    new ClassTypeable[LocalDateTimeType[LocalDateTime]] {
      type C  = LocalDateTime
      type CT = LocalDateTimeType[LocalDateTime]
      def ct: CT = datatype
    }
}

trait LocalDateTimeType[+T] extends CalendarType[T]
