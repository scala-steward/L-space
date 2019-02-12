package lspace.librarian.structure

import lspace.librarian.datatype.NodeURLType
import lspace.librarian.process.traversal.helper.ClassTypeable
import monix.eval.Task

object Node {

  implicit def default[T <: Node]: ClassTypeable.Aux[T, T, NodeURLType[T]] = new ClassTypeable[T] {
    type C  = T
    type CT = NodeURLType[T]
    def ct: CT = NodeURLType.apply[T]
  }
}

/** Implement this trait with graph specific node functions */
trait Node extends Resource[Node] {

  val value: Node = this

  def labels: List[Ontology]

  protected def _addLabel(ontology: Ontology): Unit = {
    graph.ns.ontologies
      .get(ontology.iri)
      .flatMap { ontologyOption =>
        if (ontologyOption.isEmpty) graph.ns.ontologies.store(ontology)
        else Task.unit
      }
      .runToFuture(monix.execution.Scheduler.global)
  }
  def addLabel(ontology: Ontology): Unit

  def remove(): Unit = graph.nodes.delete(this)

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
