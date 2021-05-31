package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._

object TextType extends DataTypeDef[TextType[String]] {

  lazy val datatype: TextType[String] = new TextType[String] {
    val iri: String = NS.types.`@string`
    override val iris: Set[String] =
      Set(NS.types.`@string`, NS.types.schemaText, "https://schema.org/Text", NS.types.xsdString)
    labelMap ++= Map("en" -> NS.types.`@string`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(LiteralType.datatype)
  }

  object keys extends LiteralType.Properties
  override lazy val properties: List[Property] = LiteralType.properties
  trait Properties extends LiteralType.Properties

  implicit val defaultString: ClassTypeable.Aux[TextType[String], String, TextType[String]] =
    new ClassTypeable[TextType[String]] {
      type C  = String
      type CT = TextType[String]
      def ct: CT = datatype
    }
}
trait TextType[+T] extends LiteralType[T]

//trait IriStringType extends TextType[Iri] {
//  override val iri: String       = NS.types.`@iri`
//  override val iris: Set[String] = Set()
//
//  override protected def _extendedClasses: List[ClassType[Any]] = List(TextType.textType)
//}
