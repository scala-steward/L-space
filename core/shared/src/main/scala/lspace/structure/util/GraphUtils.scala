package lspace.structure.util

import java.util.concurrent.ConcurrentHashMap

import lspace.structure.{Graph, Node, Ontology, Value}
import monix.eval.Task

import scala.collection.concurrent
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.util.control.NonFatal

trait GraphUtils {

  private val nodeMergeTasks: concurrent.Map[String, Task[Node]] =
    new ConcurrentHashMap[String, Task[Node]]().asScala
  private def getOrAddNodeMergeTask(iri: String)(mergeTask: Task[Node]): Task[Node] =
    nodeMergeTasks.getOrElseUpdate(iri, mergeTask.memoize)

  def mergeNodes(nodes: Set[Node]): Task[Node] = {
    val iri = nodes.head.iri //TODO: mergetask for each iri/iris
    if (nodes.isEmpty) Task.raiseError(new Exception("mergeNodes cannot merge an empty set"))
    else {
      getOrAddNodeMergeTask(nodes.head.iri)(
        Task
          .defer {
            //    val nodesByCreatedOnDateTime =
            //      nodes.toList.sortBy(_.out(default.typed.createdonDateTime).take(1).map(_.toEpochMilli).head)
            val unmerged =
              nodes.toList.sortBy(_.id)

            //    val (unmerged, transcended) =
            //      nodesSortedById.partition(_.out(default.typed.transcendedOnDateTime).isEmpty)
            unmerged.tail.foreach {
              slave =>
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
                          scribe.error(e.getMessage)
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
            unmerged.head.graph.nodes.hasIri(List(unmerged.head.iri)) match {
              case List(node) => Task(node)
              case List()     => Task.raiseError(new Exception(s"after merging no node ${unmerged.head.iri} left?"))
              case list       => mergeNodes(list.toSet)
            }
          }
          .doOnFinish(f =>
            Task {
              nodeMergeTasks.remove(iri)
          })
      )
    }
  }

  private val valueMergeTasks: concurrent.Map[Any, Task[Value[Any]]] =
    new ConcurrentHashMap[Any, Task[Value[Any]]]().asScala
  private def getOrAddValueMergeTask[V](v: V)(mergeTask: Task[Value[V]]): Task[Value[V]] =
    valueMergeTasks.getOrElseUpdate(v, mergeTask.memoize).asInstanceOf[Task[Value[V]]]

  def mergeValues[V](values: Set[Value[V]]): Task[Value[V]] = {
    if (values.isEmpty) Task.raiseError(new Exception("cannot merge empty set of values"))
    else if (values.map(_.value).size > 1) Task.raiseError(new Exception("cannot merge set of unequal values"))
    else {
      getOrAddValueMergeTask(values.head.value)(
        Task
          .defer {
            //    val nodesByCreatedOnDateTime =
            //      nodes.toList.sortBy(_.out(default.typed.createdonDateTime).take(1).map(_.toEpochMilli).head)
            if (values.map(v => v.value -> v.label).size != 1)
              throw new Exception(s"cannot merge unequal values ${values.map(v => v.value -> v.label)}")
            val unmerged =
              values.toList.sortBy(_.id)

            //    val (unmerged, transcended) =
            //      nodesSortedById.partition(_.out(default.typed.transcendedOnDateTime).isEmpty)
            unmerged.tail.foreach {
              slave =>
                //      typesToRemove.foreach(_.remove()) ???

                val linksIn = slave.inEMap()

                linksIn.foreach {
                  case (key, properties) =>
                    properties.foreach { property =>
                      try {
                        property.from.addOut(property.key, unmerged.head)
                      } catch {
                        case NonFatal(e) =>
                          scribe.error(e.getMessage)
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
            unmerged.head.graph.values.byValue(List(unmerged.head.value -> unmerged.head.label)) match {
              case List(value) => Task(unmerged.head)
              case List()      => Task(unmerged.head)
              case list        => mergeValues(list.toSet)
            }
          }
          .onErrorHandle { f =>
            f.printStackTrace(); throw f
          }
          .doOnFinish(f => Task(valueMergeTasks.remove(values.head.value)))
      )
    }
  }
}
