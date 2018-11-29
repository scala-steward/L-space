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
  def addLabel(classType: Ontology): Ontology

  def remove(): Unit = graph.nodes.delete(this)

  def removeLabel(classType: Ontology)

  def prettyPrint: String = s"node:${if (iri.nonEmpty) iri else id.toString}"
}
