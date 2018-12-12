package lspace.librarian.structure

import lspace.librarian.datatype.NodeURLType
import lspace.librarian.process.traversal.helper.ClassTypeable

object Node {
//  implicit def nodeType: NodeURLType[Node] = new NodeURLType[Node] {}

  implicit def default[T <: Node]: ClassTypeable.Aux[T, T, NodeURLType[T]] = new ClassTypeable[T] {
    type C  = T
    type CT = NodeURLType[T]
    def ct: CT = NodeURLType.nodeType[T]
  }
}

/**
  * Implement this trait with graph specific node functions
  */
trait Node extends Resource[Node] {

  val value: Node = this

  //  override def start(): Traversal[Node, Node, N :: HNil, HNil] = Traversal[Node, Node, HNil]()(graph, LabelsHList(HNil)).N(this)

  def labels: List[Ontology]
//  def addLabel(iri: String): Unit = addLabel(graph.ns.getOntology(iri).getOrElse(Ontology(iri)))
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

  def prettyPrint: String = s"n:${labels.map(_.iri).mkString("::")}:${if (iri.nonEmpty) iri else id.toString}"
}
