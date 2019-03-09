package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure.Property

object CalendarType extends DataTypeDef[CalendarType[Any]] {

  val datatype: CalendarType[Any] = new CalendarType[Any] {
    val iri: String = NS.types.`@temporal`
    labelMap = Map("en" -> NS.types.`@temporal`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(LiteralType.datatype)
  }

  object keys extends LiteralType.Properties
  override lazy val properties: List[Property] = LiteralType.properties
  trait Properties extends LiteralType.Properties

  implicit val clsCalendar: ClassTypeable.Aux[CalendarType[Any], Any, CalendarType[Any]] =
    new ClassTypeable[CalendarType[Any]] {
      type C  = Any
      type CT = CalendarType[Any]
      def ct: CT = datatype
    }
}

trait CalendarType[+T] extends LiteralType[T]
