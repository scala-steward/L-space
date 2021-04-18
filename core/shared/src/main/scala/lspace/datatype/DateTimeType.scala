package lspace.datatype

import lspace.NS
import lspace.structure.Property
import lspace.structure.util.ClassTypeable

import java.time.Instant

/** Should be ZonedDateTime because then it can be asserted against LocalDate and LocalTime
  */
object DateTimeType extends DataTypeDef[DateTimeType[Instant]] {

  lazy val datatype: DateTimeType[Instant] = new DateTimeType[Instant] {
    val iri: String = NS.types.`@datetime`
    override val iris: Set[String] =
      Set(NS.types.`@datetime`, NS.types.schemaDateTime, "http://schema.org/DateTime", NS.types.xsdDateTimeStamp)
    labelMap ++= Map("en" -> NS.types.`@datetime`)
    override lazy val _extendedClasses: List[_ <: DataType[_]] = List(CalendarType.datatype)
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
