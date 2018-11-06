package lspace.librarian.structure.util

import lspace.librarian.structure.Ontology

object OntologyDef {
  implicit def defToOntology(ontDef: OntologyDef): Ontology = ontDef.ontology
}

trait OntologyDef {
  def ontology: Ontology
  trait Properties
}
