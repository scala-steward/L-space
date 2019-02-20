package lspace.lgraph

import lspace.structure.{Node, Ontology, Property}

import scala.collection.mutable

object LNode {}

trait LNode extends LResource[Node] with Node {
  private val types = mutable.HashSet[Ontology]()

  /**
    * add ontology, do not store
    * @param ontology
    */
  override protected[lgraph] def _addLabel(ontology: Ontology): Unit = synchronized {
    super._addLabel(ontology)
    //    val o = if (ontology.graph != graph) graph.getOntology(ontology.iri).getOrElse(graph.storeOntology(ontology)) else ontology
    val o       = ontology
    val labels2 = labels
    if (!labels2.contains(o)) {
      if (!labels2.exists(_.`extends`(o))) {
        labels2.filter(ct => ontology.`extends`(ct)).foreach { ct =>
          //          outE(graph.TYPE).filter(p => ct.iri == p.inV.iri).foreach(_.remove())
          types -= ct
        }
        //        addOut(graph.TYPE, classType)
        types += o
      }
    }
  }

  def labels: List[Ontology] = types.toList
  def addLabel(ontology: Ontology): Unit = synchronized {
    _addLabel(ontology)
    graph.storeNode(this.asInstanceOf[graph.GNode])
    //TODO: index
  }

  def removeLabel(classType: Ontology): Unit = {
    //TODO: option to remove only specified class and keep extended class,
    // or transform 'addLabel' to add each class explicitly, included redundancy by extending
//    outE(Property.default.`@type`).find(_.to.iri == classType.iri).foreach(_.remove())
    types -= classType
  }
}
