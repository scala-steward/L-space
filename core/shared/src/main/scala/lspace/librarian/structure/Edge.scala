package lspace.librarian.structure

import lspace.NS.types
import lspace.librarian.datatype.EdgeURLType
import lspace.librarian.process.traversal.helper.ClassTypeable

object Edge {
  //  lazy val classType: ClassType[Edge[_, _]] = ClassType[Edge[_, _]](types.PROPERTY)

  implicit def default[T <: Edge[_, _]]: ClassTypeable.Aux[T, T, EdgeURLType[T]] = new ClassTypeable[T] {
    type C  = T
    type CT = EdgeURLType[T]
    def ct: CT = EdgeURLType.edgeUrlType[T]
  }
}

/**
  *
  * @tparam S outV-type, edge-start
  * @tparam E inV-type, edge-end
  */
trait Edge[S, E] extends Resource[Edge[S, E]] {
  def key: Property
  val value: Edge[S, E]      = this
  def labels: List[Property] = List(key)
  //    out(graph.TYPE).collect { case node: Node => node }.map(Property.wrap)

  //  override def start() = Traversal[Edge[S, E], Edge[S, E], step.E, HNil, HNil](step.E(List(this)))(graph, Structure(HNil), LabelsHList(HNil))

  /**
    * Edge to (value-object)
    * @return
    */
  def inV: Resource[E]
  def to: Resource[E] = inV

  /**
    * Edge from (resource-object)
    * @return
    */
  def outV: Resource[S]
  def from: Resource[S] = outV

  def remove(): Unit = graph.edges.delete(this)

  override def toString: String = s"edge:${if (iri.nonEmpty) iri else id.toString}"
}
