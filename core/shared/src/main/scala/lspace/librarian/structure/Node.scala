package lspace.librarian.structure

import lspace.librarian.datatype.NodeURLType
import lspace.librarian.process.traversal.helper.ClassTypeable

object Node {

  implicit def default[T <: Node]: ClassTypeable.Aux[T, T, NodeURLType[T]] = new ClassTypeable[T] {
    type C  = T
    type CT = NodeURLType[T]
    def ct: CT = NodeURLType.nodeType[T]
  }
}

/** Implement this trait with graph specific node functions */
trait Node extends Resource[Node] {

  val value: Node = this

  def labels: List[Ontology]

  protected def _addLabel(ontology: Ontology): Unit = {
    if (!Ontology.allOntologies.byIri.contains(ontology.iri) && graph.ns.getOntology(ontology.iri).isEmpty)
      graph.ns.storeOntology(ontology)
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
    case _                     => false
  }

  def prettyPrint: String = s"n:${labels.map(_.iri).mkString("::")}:${if (iri.nonEmpty) iri else id.toString}"
}
