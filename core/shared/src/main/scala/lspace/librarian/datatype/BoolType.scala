package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure._

object BoolType {
  val boolType: BoolType[Boolean] = new BoolType[Boolean] { type Out = Boolean }
//  implicit def default            = boolType
  implicit val defaultBoolean: ClassTypeable.Aux[BoolType[Boolean], Boolean, BoolType[Boolean]] =
    new ClassTypeable[BoolType[Boolean]] {
      type C  = Boolean
      type CT = BoolType[Boolean]
      def ct: CT = boolType
    }
}

trait BoolType[+T] extends LiteralType[T] {
  override val iri: String       = NS.types.boolean
  override val iris: Set[String] = Set(NS.types.schemaBoolean)

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(LiteralType)
}
