package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.Property

object ColorType extends DataTypeDef[ColorType[Any]] { //TODO RgbType, CMYK, PMS, NamedColor

  val datatype: ColorType[Any] = new ColorType[Any] {
    val iri: String                                             = NS.types.`@color`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@color`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(StructuredValue.datatype)
  }

  object keys extends StructuredValue.Properties
  override lazy val properties: List[Property] = StructuredValue.properties
  trait Properties extends StructuredValue.Properties

  implicit val clsColor: ClassTypeable.Aux[ColorType[Any], Any, ColorType[Any]] = new ClassTypeable[ColorType[Any]] {
    type C  = Any
    type CT = ColorType[Any]
    def ct: CT = datatype
  }
}

trait ColorType[+T] extends StructuredValue[T]
