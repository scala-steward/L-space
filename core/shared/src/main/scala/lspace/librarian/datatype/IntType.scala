package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.{DataType, NumericType}

object IntType {
  val intType: IntType[Int] = new IntType[Int] { type Out = Int }
//  implicit def default: IntType[Int] = intType
  implicit val defaultIntType: ClassTypeable.Aux[IntType[Int], Int, IntType[Int]] =
    new ClassTypeable[IntType[Int]] {
      type C  = Int
      type CT = IntType[Int]
      def ct: CT = IntType.intType
    }
}

trait IntType[+T] extends NumericType[T] {
  val iri: String                = NS.types.int
  override val iris: Set[String] = Set(NS.types.schemaInteger)

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(NumericType.numType)
}
