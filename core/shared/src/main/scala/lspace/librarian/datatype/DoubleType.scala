package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure._

object DoubleType {
  val doubleType: DoubleType[Double] = new DoubleType[Double] { type Out = Double }
//  implicit def default: DoubleType[Double] = doubleType
  implicit val defaultDoubleTypeDouble: ClassTypeable.Aux[DoubleType[Double], Double, DoubleType[Double]] =
    new ClassTypeable[DoubleType[Double]] {
      type C  = Double
      type CT = DoubleType[Double]
      def ct: CT = DoubleType.doubleType
    }
}
trait DoubleType[+T] extends NumericType[T] {
  val iri: String                = NS.types.`@double`
  override val iris: Set[String] = Set(NS.types.schemaFloat)

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(NumericType.numType)
}
