package lspace.librarian.provider.mem

import lspace.librarian.structure.{DataType, Ontology, Property}

object MemGraphDefault extends MemDataGraph {
  val iri: String       = "memgraphdefault"
  private lazy val self = this
  val ns: MemNSGraph = new MemNSGraph {
    def iri: String          = "memgraphdefault" + ".ns"
    lazy val graph: MemGraph = self
  }
  init()

  Ontology.allOntologies.values.foreach(ns.storeOntology)
  Property.allProperties.values.foreach(ns.storeProperty)
  DataType.allDataTypes.foreach(ns.storeDataType(_))
}
