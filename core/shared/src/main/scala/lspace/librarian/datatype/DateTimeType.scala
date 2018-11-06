package lspace.librarian.datatype

import java.time.Instant

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.{CalendarType, DataType}

/**
  * Should be ZonedDateTime because then it can be asserted against LocalDate and LocalTime
  */
object DateTimeType {
  val datetimeType: DateTimeType[Instant] = new DateTimeType[Instant] { type Out = Instant }
//  implicit def default                                                                 = datetimeType
//  implicit def dt[T, CT[Z] <: DateTimeType[Z]](implicit ev: CT[T] <:< DateTimeType[T]) = DataType.urlType[CT[T]]
  implicit val defaultDateTimeType: ClassTypeable.Aux[DateTimeType[Instant], Instant, DateTimeType[Instant]] =
    new ClassTypeable[DateTimeType[Instant]] {
      type C  = Instant
      type CT = DateTimeType[Instant]
      def ct: CT = datetimeType
    }
}

trait DateTimeType[+T] extends CalendarType[T] {
  val iri: String                = NS.types.datetime
  override val iris: Set[String] = Set(NS.types.schemaDateTime)

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CalendarType)
}
