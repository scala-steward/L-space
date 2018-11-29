package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.{DataType, NumericType}

object LongType {
  val longType: LongType[Long] = new LongType[Long] { type Out = Long }
//  implicit def default: LongType[Long] = longType
  implicit val defaultLongType: ClassTypeable.Aux[LongType[Long], Long, LongType[Long]] =
    new ClassTypeable[LongType[Long]] {
      type C  = Long
      type CT = LongType[Long]
      def ct: CT = LongType.longType
    }
}

trait LongType[+T] extends NumericType[T] {
  val iri: String = NS.types.`@long`

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(NumericType.numType)
}
