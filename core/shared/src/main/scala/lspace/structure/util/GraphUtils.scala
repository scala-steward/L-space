package lspace.structure.util

import java.util.concurrent.ConcurrentHashMap

import lspace.Label
import lspace.structure.{Graph, Node, Ontology, Value}
import monix.eval.Task

import scala.jdk.CollectionConverters._
import scala.collection.{concurrent, mutable}

trait GraphUtils {

  private val nodeMergeTasks: concurrent.Map[String, Task[Node]] =
    new ConcurrentHashMap[String, Task[Node]]().asScala
  private def getOrAddNodeMergeTask(iri: String)(mergeTask: Task[Node]): Task[Node] =
    nodeMergeTasks.getOrElseUpdate(iri, mergeTask.memoize)

  def mergeNodes(_nodes: Set[Node]): Task[Node] = {
    val nodes = _nodes.toList.sortBy(_.id)
    val iri   = nodes.head.iri // TODO: mergetask for each iri/iris, handle empty iri
//    val iris  = nodes.head.iris
    if (nodes.isEmpty) Task.raiseError(new Exception("mergeNodes cannot merge an empty set"))
    else {
      for {
        node <- getOrAddNodeMergeTask(nodes.head.iri)(
          Task
            .defer {
              //    val nodesByCreatedOnDateTime =
              //      nodes.toList.sortBy(_.out(default.typed.createdonDateTime).take(1).map(_.toEpochMilli).head)
              val unmerged = nodes
              //              nodes.toList.sortBy(_.id)

              //    val (unmerged, transcended) =
              //      nodesSortedById.partition(_.out(default.typed.transcendedOnDateTime).isEmpty)
              for {
                _ <- Task.sequence(unmerged.tail.map { slave =>
                  val masterVertexOntologies = unmerged.head.labels // out(this.typeOntology).map(Ontology.wrap)
                  val suborVertexOntologies  = slave.labels         // out(this.typeOntology).map(Ontology.wrap)
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
                  val newIris = slave.iris.diff(nodes.head.iris).toList.filter(_.nonEmpty)
                  //      typesToRemove.foreach(_.remove()) ???
                  val linksIn  = slave.inEMap()
                  val linksOut = slave.outEMap().filterNot(p => Graph.baseKeys.contains(p._1))
                  for {
                    _ <- Task.sequence(newIris.map(nodes.head.addOut(Label.P.`@ids`, _)))
                    _ <- Task.sequence(
                      typesToAdd
                        .filterNot(tpe => typesToAdd.exists(_.`extends`(tpe)))
                        .map(tpe => unmerged.head.addLabel(tpe))
                    )
                    _ <- Task.parSequenceUnordered(linksIn.map { case (_, properties) =>
                      Task.parSequenceUnordered(properties.map { property =>
                        for {
                          _ <- property.from.addOut(property.key, unmerged.head)
                          _ <- property.remove()
                        } yield ()
                      })
                    })
                    _ <- Task.parSequenceUnordered(linksOut.map { case (_, properties) =>
                      Task.parSequenceUnordered(properties.map { property =>
                        for {
                          _ <- unmerged.head.addOut(property.key, property.to)
                          _ <- property.remove()
                        } yield ()
                      })
                    })
                    // _ <- slave.addOut(default.typed.transcendedOnDateTime, Instant.now())
                    _ <- slave.remove()
                  } yield ()
                })
                // _ <- Task.parSequence(transcended.map(_.remove()))
              } yield unmerged.head
            }
            .doOnFinish(_ => Task(nodeMergeTasks.remove(iri)).void)
            .memoizeOnSuccess
        )
        node2 <- node.graph.nodes.hasIri((node.iris + node.iri).toList).toListL.flatMap {
          case List(node) =>
            Task(node)
          case List() =>
//            Task.raiseError(new Exception(s"after merging no node ${node.iri} left?"))
            Task.now(node) // TODO: this should not be needed, somehow after merging the node is sometimes not found
          case list =>
            mergeNodes(list.toSet)
        }
      } yield node2
    }
  }

  private val valueMergeTasks: concurrent.Map[Any, Task[Value[Any]]] =
    new ConcurrentHashMap[Any, Task[Value[Any]]]().asScala
  private def getOrAddValueMergeTask[V](v: V)(mergeTask: Task[Value[V]]): Task[Value[V]] =
    valueMergeTasks.getOrElseUpdate(v, mergeTask.memoize).asInstanceOf[Task[Value[V]]]

  def mergeValues[V](_values: Set[Value[V]]): Task[Value[V]] = {
    val values = _values.toList.sortBy(_.id)
    if (values.isEmpty) Task.raiseError(new Exception("cannot merge empty set of values"))
    else if (values.map(_.value).toSet.size > 1)
      Task.raiseError(new Exception(s"cannot merge set of unequal values ${values.map(_.value)}"))
    else {
      getOrAddValueMergeTask(values.head.value)(
        Task
          .defer {
            //    val nodesByCreatedOnDateTime =
            //      nodes.toList.sortBy(_.out(default.typed.createdonDateTime).take(1).map(_.toEpochMilli).head)
            if (values.map(v => v.value -> v.label).toSet.size != 1)
              throw new Exception(s"cannot merge unequal values ${values.map(v => v.value -> v.label)}")
            val unmerged = values

            //    val (unmerged, transcended) =
            //      nodesSortedById.partition(_.out(default.typed.transcendedOnDateTime).isEmpty)
            for {
              _ <- Task.sequence(unmerged.tail.map { slave =>
                //      typesToRemove.foreach(_.remove()) ???

                val linksIn  = slave.inEMap()
                val linksOut = slave.outEMap().filterNot(p => Graph.baseKeys.contains(p._1))
                for {
                  _ <- Task.parSequenceUnordered(linksIn.map { case (_, properties) =>
                    Task.parSequenceUnordered(properties.map { property =>
                      property.from.addOut(property.key, unmerged.head)
                    })
                  })
                  _ <- Task.parSequenceUnordered(linksOut.map { case (_, properties) =>
                    Task.parSequenceUnordered(properties.map { property =>
                      unmerged.head.addOut(property.key, property.to)
                    })
                  })
                  _ <- Task.defer {
                    slave.remove()
                  }
                } yield ()
              //      slave.addOut(default.typed.transcendedOnDateTime, Instant.now())
              })
//              value <- unmerged.head.graph.values
//                .byValue(List(unmerged.head.value -> unmerged.head.label))
//                .toListL
//                .flatMap {
//                  case List(value) =>
//                    Task(unmerged.head)
//                  case List() => Task(unmerged.head)
//                  case list =>
//                    mergeValues(list.toSet)
//                }
              // _ <- Task.parSequence(transcended.map(_.remove()))
            } yield unmerged.head
          }
          .onErrorHandle { f =>
            f.printStackTrace(); throw f
          }
          .doOnFinish(_ => Task(valueMergeTasks.remove(values.head.value)).void)
          .memoizeOnSuccess
      )
    }
  }
}
