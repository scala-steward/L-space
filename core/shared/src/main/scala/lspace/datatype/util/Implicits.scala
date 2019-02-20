package lspace.datatype.util

object Implicits {
  implicit class WithInstantString(s: String) {
    import java.time.{LocalDateTime => JLocalDateTime, _}
    import format._
    import scala.util.Try

    /**
      * ISO_INSTANT	Date and Time of an Instant	e.g. '2011-12-03T10:15:30Z'
      * @return
      */
    def toDateTime: Option[Instant] = Try(Instant.parse(s)).toOption

    /**
      * ISO_LOCAL_DATE_TIME	ISO Local Date and Time	e.g. '2011-12-03T10:15:30'
      * @return
      */
    def toLocalDateTime: Option[JLocalDateTime] =
      Try(JLocalDateTime.parse(s, DateTimeFormatter.ISO_LOCAL_DATE_TIME)).toOption

    /**
      * ISO_LOCAL_DATE	ISO Local Date e.g.'2011-12-03'
      * @return
      */
    def toDate: Option[LocalDate] = Try(LocalDate.parse(s, DateTimeFormatter.ISO_LOCAL_DATE)).toOption

    /**
      * ISO_LOCAL_TIME	Time without offset	e.g. '10:15:30'
      * @return
      */
    def toTime: Option[LocalTime] = Try(LocalTime.parse(s, DateTimeFormatter.ISO_LOCAL_TIME)).toOption
  }
}
