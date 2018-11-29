package lspace.librarian.datatype

import java.time.LocalTime

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.{CalendarType, DataType}

object LocalTimeType {
  val default: LocalTimeType[LocalTime] = new LocalTimeType[LocalTime] { type Out = LocalTime }

  implicit val defaultLocalTime: ClassTypeable.Aux[LocalTimeType[LocalTime], LocalTime, LocalTimeType[LocalTime]] =
    new ClassTypeable[LocalTimeType[LocalTime]] {
      type C  = LocalTime
      type CT = LocalTimeType[LocalTime]
      def ct: CT = default
    }
}
trait LocalTimeType[+T] extends CalendarType[T] {
  val iri: String                = NS.types.`@time`
  override val iris: Set[String] = Set(NS.types.schemaTime)

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CalendarType)
}
