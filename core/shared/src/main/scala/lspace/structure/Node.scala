package lspace.structure

import lspace.NS
import lspace.datatype.{DataType, IriType, NodeURLType}
import lspace.librarian.traversal.{step, Segment, Traversal}
import lspace.structure.util.ClassTypeable
import monix.eval.Task
import shapeless.{::, HNil}

object Node {

  implicit def default[T <: Node]: ClassTypeable.Aux[T, T, NodeURLType[T]] = new ClassTypeable[T] {
    type C  = T
    type CT = NodeURLType[T]
    def ct: CT = NodeURLType.apply[T]
  }

  def nodeUrl: NodeURLType[Node] = new NodeURLType[Node] {
    val iri: String = NS.types.`@nodeURL`
    labelMap = Map("en" -> NS.types.`@nodeURL`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(IriType.datatype)
  }

  implicit class WithNode[T](node: Node) {
    def g: Traversal[ClassType[Any], NodeURLType[Node], Segment[step.N :: HNil] :: HNil]     = lspace.g.N(node)
    def start: Traversal[ClassType[Any], NodeURLType[Node], Segment[step.N :: HNil] :: HNil] = g
  }
}

/** Implement this trait with graph specific node functions */
trait Node extends Resource[Node] {

  val value: Node = this

  def labels: List[Ontology]

  protected def _addLabel(ontology: Ontology): Task[Unit] = {
    graph.ns.ontologies.store(ontology).forkAndForget //.runToFuture(monix.execution.Scheduler.global)
//    graph.ns.ontologies
//      .get(ontology.iri)
//      .flatMap { ontologyOption =>
//        if (ontologyOption.isEmpty || !graph.ns.ontologies.byIri.contains(ontology.iri))
//          graph.ns.ontologies.store(ontology)
//        else Task.unit
//      }
//      .runToFuture(monix.execution.Scheduler.global)
  }
  def addLabel(ontology: Ontology): Task[Unit]

  def remove(): Task[Unit] = graph.nodes.delete(this)

  def removeLabel(classType: Ontology)

  override def equals(o: scala.Any): Boolean = o match {
    case resource: graph._Node => sameResource(resource)
    case _                     => false
  }

  def equalValues(o: scala.Any): Boolean = o match {
    case resource: graph._Node => resource.iri == iri || resource.iris.intersect(iris).nonEmpty
    case p: Property           => iri == p.iri || iris.contains(p.iri)
    case _                     => false
  }

  def prettyPrint: String = s"n:${labels.map(_.iri).mkString("::")}:${if (iri.nonEmpty) iri else id.toString}"
}
