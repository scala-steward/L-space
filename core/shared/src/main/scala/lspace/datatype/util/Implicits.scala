package lspace.datatype.util

object Implicits {
  implicit class WithInstantString(s: String) {
    import java.time._
    import scala.util.Try

    /** ISO_INSTANT Date and Time of an Instant e.g. '2011-12-03T10:15:30Z'
      * @return
      */
    def toDateTime: Option[Instant]              = Try(Instant.parse(s)).toOption
    def toInstant: Option[Instant]               = Try(Instant.parse(s)).toOption
    def toOffsetDateTime: Option[OffsetDateTime] = Try(OffsetDateTime.parse(s)).toOption
    def toZonedDateTime: Option[ZonedDateTime]   = Try(ZonedDateTime.parse(s)).toOption

    /** ISO_LOCAL_DATE_TIME ISO Local Date and Time e.g. '2011-12-03T10:15:30'
      * @return
      */
    def toLocalDateTime: Option[LocalDateTime] =
      Try(LocalDateTime.parse(s)).toOption

    /** ISO_LOCAL_DATE ISO Local Date e.g.'2011-12-03'
      * @return
      */
    def toDate: Option[LocalDate] = Try(LocalDate.parse(s)).toOption

    /** ISO_LOCAL_TIME Time without offset e.g. '10:15:30'
      * @return
      */
    def toTime: Option[LocalTime]        = Try(LocalTime.parse(s)).toOption
    def toOffsetTime: Option[OffsetTime] = Try(OffsetTime.parse(s)).toOption
  }
}
