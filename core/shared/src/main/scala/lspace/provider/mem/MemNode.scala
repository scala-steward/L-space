package lspace.provider.mem

import lspace.structure._

import scala.collection.mutable

object MemNode {}

trait MemNode extends MemResource[Node] with Node {

  private val types = mutable.HashSet[Ontology]()

  /**
    * add ontology, do not store
    * @param ontology
    */
  override protected[mem] def _addLabel(ontology: Ontology): Unit = synchronized {
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

  /**
    * adds and stores ontology
    * @param ontology
    */
  def addLabel(ontology: Ontology): Unit = synchronized {
    _addLabel(ontology)
    graph.storeNode(this.asInstanceOf[graph.GNode])
    //TODO: index
  }

  def removeLabel(classType: Ontology): Unit = types -= classType
}
