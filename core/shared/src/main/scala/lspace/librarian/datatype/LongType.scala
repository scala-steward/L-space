package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.Property

object LongType extends DataTypeDef[LongType[Long]] {

  lazy val datatype: LongType[Long] = new LongType[Long] {
    val iri: String                                             = NS.types.`@long`
    override val iris: Set[String]                              = Set(NS.types.xsdLong)
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@long`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(NumericType.datatype)
  }

  object keys extends NumericType.Properties
  override lazy val properties: List[Property] = NumericType.properties
  trait Properties extends NumericType.Properties

//  implicit def default: LongType[Long] = longType
  implicit val defaultLongType: ClassTypeable.Aux[LongType[Long], Long, LongType[Long]] =
    new ClassTypeable[LongType[Long]] {
      type C  = Long
      type CT = LongType[Long]
      def ct: CT = datatype
    }
}

trait LongType[+T] extends NumericType[T]
