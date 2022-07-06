package lspace.librarian.logic

import lspace.structure.{Node, Ontology, OntologyDef}

package object predicate {
  trait PredicateWrapper[+T] {
    //  def ontology: Ontology
    def toP(node: Node): T
  }

  abstract class PredicateDef(label: String, comment: String = "", `@extends`: => List[Ontology] = List(P.ontology))
      extends OntologyDef(lspace.NS.vocab.Lspace + s"librarian/p/$label", Set(), label, comment, `@extends`)
}
