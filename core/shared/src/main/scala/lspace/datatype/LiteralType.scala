package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure.Property

trait LiteralType[+T] extends DataType[T]

object LiteralType extends DataTypeDef[LiteralType[Any]] {

  lazy val datatype: LiteralType[Any] = new LiteralType[Any] {
    val iri: String                = NS.types.`@literal`
    override val iris: Set[String] = Set(NS.types.`@literal`)
    labelMap ++= Map("en" -> NS.types.`@literal`)
    override lazy val _extendedClasses: List[_ <: DataType[_]] = List(DataType.datatype)
  }

  object keys
  override lazy val properties: List[Property] = List()
  trait Properties extends DataTypeDef.Properties

  implicit def clsLiteral[T]: ClassTypeable.Aux[LiteralType[T], T, LiteralType[T]] = new ClassTypeable[LiteralType[T]] {
    type C  = T
    type CT = LiteralType[T]
    def ct: CT = new LiteralType[T] { val iri: String = NS.types.`@literal` }
  }
}
