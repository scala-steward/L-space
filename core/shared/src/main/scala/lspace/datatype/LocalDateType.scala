package lspace.datatype

import java.time.LocalDate

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure.Property

object LocalDateType extends DataTypeDef[LocalDateType[LocalDate]] {

  lazy val datatype: LocalDateType[LocalDate] = new LocalDateType[LocalDate] {
    val iri: String                = NS.types.`@date`
    override val iris: Set[String] = Set(NS.types.schemaDate)
    labelMap = Map("en" -> NS.types.`@date`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CalendarType.datatype)
  }

  object keys extends CalendarType.Properties
  override lazy val properties: List[Property] = CalendarType.properties
  trait Properties extends CalendarType.Properties

  implicit val defaultLocalDateType: ClassTypeable.Aux[LocalDateType[LocalDate], LocalDate, LocalDateType[LocalDate]] =
    new ClassTypeable[LocalDateType[LocalDate]] {
      type C  = LocalDate
      type CT = LocalDateType[LocalDate]
      def ct: CT = datatype
    }
}
trait LocalDateType[+T] extends CalendarType[T]
