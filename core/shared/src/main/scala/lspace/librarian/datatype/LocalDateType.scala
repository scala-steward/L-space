package lspace.librarian.datatype

import java.time.LocalDate

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.{CalendarType, DataType}

object LocalDateType {
  val default: LocalDateType[LocalDate] = new LocalDateType[LocalDate] { type Out = LocalDate }

  implicit val defaultLocalDateType: ClassTypeable.Aux[LocalDateType[LocalDate], LocalDate, LocalDateType[LocalDate]] =
    new ClassTypeable[LocalDateType[LocalDate]] {
      type C  = LocalDate
      type CT = LocalDateType[LocalDate]
      def ct: CT = LocalDateType.default
    }
}
trait LocalDateType[+T] extends CalendarType[T] {
  val iri: String                = NS.types.`@date`
  override val iris: Set[String] = Set(NS.types.schemaDate)

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CalendarType)
}
