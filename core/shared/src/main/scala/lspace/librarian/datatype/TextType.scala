package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure._
import lspace.types.string.Iri

object TextType {
  val textType: TextType[String] = new TextType[String] {}
//  implicit def default           = textType
  implicit val defaultString: ClassTypeable.Aux[TextType[String], String, TextType[String]] =
    new ClassTypeable[TextType[String]] {
      type C  = String
      type CT = TextType[String]
      def ct: CT = textType
    }
}
trait TextType[+T] extends LiteralType[T] {
  val iri: String                = NS.types.string
  override val iris: Set[String] = Set(NS.types.schemaText)

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(LiteralType)
}

trait IriStringType extends TextType[Iri] {
  override val iri: String       = NS.types.iri
  override val iris: Set[String] = Set()

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(TextType.textType)
}
