package lspace.librarian.structure.util

import java.time.Instant

import lspace.librarian.structure.Property.default
import lspace.librarian.structure.{Graph, Node, Ontology}

import scala.collection.mutable
import scala.util.control.NonFatal

object GraphUtils {
  def mergeNodes(nodes: Set[Node]): Node = {
    //    val nodesByCreatedOnDateTime =
    //      nodes.toList.sortBy(_.out(default.typed.createdonDateTime).take(1).map(_.toEpochMilli).head)
    val unmerged =
      nodes.toList.sortBy(_.id)

//    val (unmerged, transcended) =
//      nodesSortedById.partition(_.out(default.typed.transcendedOnDateTime).isEmpty)
    unmerged.tail.foreach { slave =>
      val masterVertexOntologies = unmerged.head.labels //out(this.typeOntology).map(Ontology.wrap)
      val suborVertexOntologies  = slave.labels //out(this.typeOntology).map(Ontology.wrap)
      val ontologyGap            = suborVertexOntologies.diff(masterVertexOntologies)
      val typesToAdd             = mutable.HashSet[Ontology]()
      val typesToRemove          = mutable.HashSet[Ontology]()
      ontologyGap.foreach { ontology =>
        if (!masterVertexOntologies.exists(_.`extends`(ontology))) {
          masterVertexOntologies.find(o => ontology.`extends`(o)) match {
            case Some(inheritedOntology) =>
              typesToRemove += inheritedOntology
            case None =>
          }
          typesToAdd += ontology
        }
      }
      //      typesToRemove.foreach(_.remove()) ???
      typesToAdd
        .filterNot(tpe => typesToAdd.exists(_.`extends`(tpe)))
        .foreach(tpe => unmerged.head.addLabel(tpe))
      val linksIn = slave.inEMap()

      linksIn.foreach {
        case (key, properties) =>
          properties.foreach { property =>
            scala.util.control.NonFatal
            try {
              property.from.addOut(property.key, unmerged.head)
            } catch {
              case NonFatal(e) =>
                println(property.key.iri)
                println(property.from.iri)
                println(property.from.value)
                throw e
            }
          }
      }
      val linksOut = slave.outEMap().filterNot(p => Graph.baseKeys.contains(p._1))
      linksOut.foreach {
        case (key, properties) =>
          properties.foreach { property =>
            unmerged.head.addOut(property.key, property.to)
          }
      }
//      slave.addOut(default.typed.transcendedOnDateTime, Instant.now())
      slave.remove()
    }
//    transcended.foreach(_.remove())
    unmerged.head
  }
}
