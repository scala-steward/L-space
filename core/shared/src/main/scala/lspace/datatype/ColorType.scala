package lspace.datatype

import lspace.{ClassType, NS}
import lspace.structure.util.ClassTypeable
import lspace.structure.Property

object ColorType extends DataTypeDef[ColorType[Any]] { //TODO RgbType, CMYK, PMS, NamedColor

  val datatype: ColorType[Any] = new ColorType[Any] {
    val iri: String                = NS.types.`@color`
    override val iris: Set[String] = Set(NS.types.`@color`)
    labelMap ++= Map("en" -> NS.types.`@color`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(StructuredType.datatype)
  }

  object keys extends StructuredType.Properties
  override lazy val properties: List[Property] = StructuredType.properties
  trait Properties extends StructuredType.Properties

  implicit val clsColor: ClassTypeable.Aux[ColorType[Any], Any, ColorType[Any]] = new ClassTypeable[ColorType[Any]] {
    type C  = Any
    type CT = ColorType[Any]
    def ct: CT = datatype
  }
}

trait ColorType[+T] extends StructuredType[T]
