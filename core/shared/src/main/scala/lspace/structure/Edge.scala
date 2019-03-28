package lspace.structure

import lspace.NS
import lspace.datatype.{DataType, EdgeURLType, IriType}
import lspace.librarian.traversal.{step, Segment, Traversal}
import lspace.structure.util.ClassTypeable
import monix.eval.Task
import shapeless.{::, HNil}

object Edge {

  implicit def default[T <: Edge[_, _]]: ClassTypeable.Aux[T, T, EdgeURLType[T]] = new ClassTypeable[T] {
    type C  = T
    type CT = EdgeURLType[T]
    def ct: CT = EdgeURLType.apply[T]
  }

  lazy val edgeUrl = new EdgeURLType[Edge[Any, Any]] {
    val iri: String = NS.types.`@edgeURL`
    labelMap = Map("en" -> NS.types.`@edgeURL`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(IriType.datatype)
  }

  implicit class WithEdge[S, E](edge: Edge[S, E]) {
    def g: Traversal[ClassType[Any], EdgeURLType[Edge[S, E]], Segment[step.E :: HNil] :: HNil] = lspace.g.E(edge)
  }
}

/**
  *
  * @tparam S outV-type, edge-start
  * @tparam E inV-type, edge-end
  */
trait Edge[+S, +E] extends Resource[Edge[S, E]] {

  def key: Property
  val value: Edge[S, E]      = this
  def labels: List[Property] = List(key)

  def inV: Resource[E] = to

  /** Destination of the edge
    * @return [[Resource]]
    */
  def to: Resource[E]

  def outV: Resource[S] = from

  /** Origin of the edge
    * @return [[Resource]]
    */
  def from: Resource[S]

  def remove(): Task[Unit] = graph.edges.delete(this)

  override def equals(o: scala.Any): Boolean = o match {
    case resource: graph._Edge[_, _] => sameResource(resource)
    case _                           => false
  }

  /** Compares if the edges the same origin, label and destination
    *
    * @param o
    * @return
    */
  def equalValues(o: scala.Any): Boolean = o match {
    case resource: graph._Edge[_, _] => resource.from.id == from.id && resource.key == key && resource.to.id == to.id
    case _                           => false
  }

  def prettyPrint: String =
    s"e:${if (iri.nonEmpty) iri else id.toString}:${from.prettyPrint} --- ${key.iri} --> ${to.prettyPrint}"
}
