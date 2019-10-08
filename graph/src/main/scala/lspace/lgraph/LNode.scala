package lspace.lgraph

import lspace.structure.{Node, Ontology, Property}
import monix.eval.Task

import scala.collection.mutable

object LNode {}

trait LNode extends LResource[Node] with Node {
  private val types = mutable.HashSet[Ontology]()

  /**
    * add ontology, do not store
    * @param ontology
    */
  protected[lgraph] def _cacheLabel(ontology: Ontology): Unit = {
    types.synchronized {

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
  }

  def labels: List[Ontology] = types.toList
  def addLabel(ontology: Ontology): Task[Unit] = Task.defer {
    if (ontology != Ontology.empty)
      for {
        _ <- super._addLabel(ontology)
        _ = _cacheLabel(ontology)
        _ <- graph.storeNode(this.asInstanceOf[graph._Node])
      } yield ()
    //TODO: index
    else Task.unit
  }

  def removeLabel(classType: Ontology): Unit = {
    //TODO: option to remove only specified class and keep extended class,
    // or transform 'addLabel' to add each class explicitly, included redundancy by extending
//    outE(Property.default.`@type`).find(_.to.iri == classType.iri).foreach(_.remove())
    types -= classType
  }
}
