package lspace.datatype

import lspace.{ClassType, NS}
import lspace.structure.{Graph, Property}

object GraphType extends DataTypeDef[GraphType[Graph]] {

  lazy val datatype: GraphType[Graph] = new GraphType[Graph] {
    val iri: String = NS.types.`@graph`
    labelMap ++= Map("en" -> NS.types.`@graph`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(DataType.datatype) // TODO: extend IriType?
  }

  object keys
  override lazy val properties: List[Property] = List()
  trait Properties

  def apply[T <: Graph] = new GraphType[T] {
    val iri: String = NS.types.`@graph`
    labelMap ++= Map("en" -> NS.types.`@graph`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(DataType.datatype)
  }
}

trait GraphType[+T <: Graph] extends DataType[T]
