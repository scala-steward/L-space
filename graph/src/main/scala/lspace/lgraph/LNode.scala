package lspace.lgraph

import lspace.librarian.structure.{Node, Ontology, Property}

import scala.collection.mutable

object LNode {
  def apply(id: Long, graph: LGraph): graph._Node with LNode = {
    val _id    = id
    val _graph = graph
    new graph._Node with LNode {
      val id    = _id
      val graph = _graph
    }
  }

}

trait LNode extends LResource[Node] with Node {
  private val types          = mutable.HashSet[Ontology]()
  def labels: List[Ontology] = types.toList
  def addLabel(ontology: Ontology): Ontology = {
    //    val o = if (ontology.graph != graph) graph.getOntology(ontology.iri).getOrElse(graph.storeOntology(ontology)) else ontology
    val o       = ontology
    val labels2 = labels
    if (!labels2.contains(o)) {
      if (!labels2.exists(_.`extends`(o))) {
        labels2.filter(ct => ontology.`extends`(ct)).foreach { ct =>
          //          outE(graph.TYPE).filter(p => ct.iri == p.inV.iri).foreach(_.remove())
          types -= ct
        }
//        addOut(Property.default.`@type`, o)
        types += o
      }
    }
    //TODO: store and index
    o
  }

  def removeLabel(classType: Ontology): Unit = {
    //TODO: option to remove only specified class and keep extended class,
    // or transform 'addLabel' to add each class explicitly, included redundancy by extending
//    outE(Property.default.`@type`).find(_.to.iri == classType.iri).foreach(_.remove())
    types -= classType
  }
}
