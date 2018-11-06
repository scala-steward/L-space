package lspace.librarian.structure

trait DataGraph extends Graph {

  def +(label: String): Node = createNode(ns.getOntology(label).getOrElse(Ontology(label)))
}
