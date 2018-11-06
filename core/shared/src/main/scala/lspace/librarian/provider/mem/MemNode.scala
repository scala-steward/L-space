package lspace.librarian.provider.mem

import lspace.librarian.structure._

import scala.collection.mutable

object MemNode {
  protected[provider] def apply(implicit _graph: MemGraph): MemNode = new MemNode {
    implicit val graph: MemGraph = _graph
  }
}

trait MemNode extends MemResource[Node] with Node {

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
        //        addOut(graph.TYPE, classType)
        types += o
      }
    }
    o
  }

  def removeLabel(classType: Ontology): Unit = types -= classType

  override def _remove(): Unit = {
    super._remove()
  }
}
