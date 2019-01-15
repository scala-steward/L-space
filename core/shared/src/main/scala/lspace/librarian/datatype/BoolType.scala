package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure._

object BoolType extends DataTypeDef[BoolType[Any]] {

  lazy val datatype: BoolType[Boolean] = new BoolType[Boolean] {
    val iri: String                                             = NS.types.`@boolean`
    override val iris: Set[String]                              = Set(NS.types.schemaBoolean)
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@boolean`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(LiteralType.datatype)
  }

  object keys extends LiteralType.Properties
  override lazy val properties: List[Property] = LiteralType.properties
  trait Properties extends LiteralType.Properties

//  implicit def default            = boolType
  implicit val defaultBoolean: ClassTypeable.Aux[BoolType[Boolean], Boolean, BoolType[Boolean]] =
    new ClassTypeable[BoolType[Boolean]] {
      type C  = Boolean
      type CT = BoolType[Boolean]
      def ct: CT = datatype
    }
}

trait BoolType[+T] extends LiteralType[T]
