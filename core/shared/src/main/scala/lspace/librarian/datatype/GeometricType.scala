package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.Property
import lspace.types.vector.Geometry

object GeometricType extends DataTypeDef[GeometricType[Geometry]] {

  lazy val datatype: GeometricType[Geometry] = new GeometricType[Geometry] {
    val iri: String                                             = NS.types.`@geo`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@geo`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(StructuredValue.datatype)
  }

  object keys extends StructuredValue.Properties
  override lazy val properties: List[Property] = StructuredValue.properties
  trait Properties extends StructuredValue.Properties

  implicit val clsGeometric: ClassTypeable.Aux[GeometricType[Geometry], Geometry, GeometricType[Geometry]] =
    new ClassTypeable[GeometricType[Geometry]] {
      type C  = Geometry
      type CT = GeometricType[Geometry]
      def ct: CT = datatype
    }
}

trait GeometricType[+T] extends StructuredValue[T]
