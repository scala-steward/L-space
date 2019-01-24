package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.Property

object IntType extends DataTypeDef[IntType[Int]] {

  lazy val datatype: IntType[Int] = new IntType[Int] {
    val iri: String                                             = NS.types.`@int`
    override val iris: Set[String]                              = Set(NS.types.schemaInteger, NS.types.xsdInt)
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@int`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(NumericType.datatype)
  }

  object keys extends NumericType.Properties
  override lazy val properties: List[Property] = NumericType.properties
  trait Properties extends NumericType.Properties

  implicit val defaultIntType: ClassTypeable.Aux[IntType[Int], Int, IntType[Int]] =
    new ClassTypeable[IntType[Int]] {
      type C  = Int
      type CT = IntType[Int]
      def ct: CT = datatype
    }
}

trait IntType[+T] extends NumericType[T]
