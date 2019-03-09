package lspace.datatype

import java.time.{Instant, LocalDateTime}

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure.Property

/**
  * Should be ZonedDateTime because then it can be asserted against LocalDate and LocalTime
  */
object DateTimeType extends DataTypeDef[DateTimeType[Instant]] {

  lazy val datatype: DateTimeType[Instant] = new DateTimeType[Instant] {
    val iri: String                = NS.types.`@datetime`
    override val iris: Set[String] = Set(NS.types.schemaDateTime, NS.types.xsdDateTimeStamp)
    labelMap = Map("en" -> NS.types.`@datetime`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CalendarType.datatype)
  }

  object keys extends CalendarType.Properties
  override lazy val properties: List[Property] = CalendarType.properties
  trait Properties extends CalendarType.Properties

//  implicit def default                                                                 = datetimeType
//  implicit def dt[T, CT[Z] <: DateTimeType[Z]](implicit ev: CT[T] <:< DateTimeType[T]) = DataType.urlType[CT[T]]
  implicit val defaultDateTimeType: ClassTypeable.Aux[DateTimeType[Instant], Instant, DateTimeType[Instant]] =
    new ClassTypeable[DateTimeType[Instant]] {
      type C  = Instant
      type CT = DateTimeType[Instant]
      def ct: CT = datatype
    }
}

trait DateTimeType[+T] extends CalendarType[T]
