package lspace.provider.mem

import lspace.structure._
import monix.eval.Task

import scala.collection.mutable

object MemNode {}

trait MemNode extends MemResource[Node] with Node {

  private val types = mutable.HashSet[Ontology]()

  /**
    * add ontology, do not store
    * @param ontology
    */
  protected[lspace] def _cacheLabel(ontology: Ontology): Unit =
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

  def labels: List[Ontology] = types.toList

  /**
    * adds and stores ontology
    * @param ontology
    */
  def addLabel(ontology: Ontology): Task[Unit] = Task.defer {
    synchronized {
      for {
        _ <- super._addLabel(ontology)
        _ = _cacheLabel(ontology)
        _ <- graph.storeNode(this.asInstanceOf[graph._Node])
      } yield ()
      //TODO: index
    }
  }

  def removeLabel(classType: Ontology): Unit = types -= classType
}
