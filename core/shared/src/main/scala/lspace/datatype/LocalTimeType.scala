package lspace.datatype

import java.time.LocalTime
import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure.{ClassType, Property}

object LocalTimeType extends DataTypeDef[LocalTimeType[LocalTime]] {

  lazy val datatype: LocalTimeType[LocalTime] = new LocalTimeType[LocalTime] {
    val iri: String                = NS.types.`@time`
    override val iris: Set[String] = Set(NS.types.`@time`, NS.types.schemaTime, "https://schema.org/Time")
    labelMap ++= Map("en" -> NS.types.`@time`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(CalendarType.datatype)
  }

  object keys extends CalendarType.Properties
  override lazy val properties: List[Property] = CalendarType.properties
  trait Properties extends CalendarType.Properties

  implicit val defaultLocalTime: ClassTypeable.Aux[LocalTimeType[LocalTime], LocalTime, LocalTimeType[LocalTime]] =
    new ClassTypeable[LocalTimeType[LocalTime]] {
      type C  = LocalTime
      type CT = LocalTimeType[LocalTime]
      def ct: CT = datatype
    }
}
trait LocalTimeType[+T] extends CalendarType[T]
