package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure.Property
import lspace.types.geo.Geometry

object GeometricType extends DataTypeDef[GeometricType[Geometry]] {

  lazy val datatype: GeometricType[Geometry] = new GeometricType[Geometry] {
    val iri: String                = NS.types.`@geo`
    override val iris: Set[String] = Set(NS.types.`@geo`)
    labelMap = Map("en" -> NS.types.`@geo`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(StructuredType.datatype)
  }

  object keys extends StructuredType.Properties
  override lazy val properties: List[Property] = StructuredType.properties
  trait Properties extends StructuredType.Properties

  implicit val clsGeometric: ClassTypeable.Aux[GeometricType[Geometry], Geometry, GeometricType[Geometry]] =
    new ClassTypeable[GeometricType[Geometry]] {
      type C  = Geometry
      type CT = GeometricType[Geometry]
      def ct: CT = datatype
    }
}

trait GeometricType[+T] extends StructuredType[T]
