package lspace.librarian.process.computer

import java.time.Instant

import lspace.librarian.process.traversal.{
  FilterStep,
  ResourceStep,
  Step,
  Traversal,
  TraversalPath,
  Traverser,
  UntypedTraversal
}
import lspace.librarian.process.traversal.step._
import lspace.librarian.structure.{ClassType, Graph, Node, Value}
import lspace.util.types.DefaultsToAny
import monix.eval.Task
import shapeless.HList

object TaskBuilder {
  private class GraphComputerTraverser[+T](val get: T,
                                           val path: TraversalPath = TraversalPath(),
                                           val loops: Int = 0,
                                           val mit: Option[Instant] = None,
                                           val permissions: List[String] = List())
      extends Traverser[T] {
    def apply[V](get: V,
                 path: TraversalPath = TraversalPath(),
                 loops: Int = 0,
                 mit: Option[Instant] = None,
                 permissions: List[String] = List()): Traverser[V] =
      new GraphComputerTraverser[V](get, path, loops, mit, permissions)
  }
  //  def createTraverser[T](get: T): Traverser[T] = GraphComputerTraverser(get)

  def createTraverser[T](get: T,
                         path: TraversalPath = TraversalPath(),
                         loops: Int = 0,
                         mit: Option[Instant] = None,
                         permissions: List[String] = List()): Traverser[T] =
    new GraphComputerTraverser[T](get, path, loops, mit, permissions)

  def traverse[Out: DefaultsToAny, GT <: Graph](traversal: UntypedTraversal): Task[List[Out]] = {
    Task.eval(traversal.validate()).flatMap {
      case Right(success) =>
        Task.eval(traversal.segments.toList).flatMap {
          case head :: tail =>
            startProcessingSteps(traversal.steps)(traversal.target).map(_.map(_.get).asInstanceOf[List[Out]])
          case Nil => Task.now(List[Out]())
        }
      case Left(error) => throw error
    }
  }

  private def startProcessingSteps(steps: List[Step])(implicit graph: Graph): Task[Stream[Traverser[_]]] = {
    val analyseForIndexedPattern = Task {
//      steps.tail.span {
//        case step: FilterStep =>
//      }
    }

    Task
      .eval(steps)
      .flatMap[Stream[Traverser[_]]] {
        case (step: ResourceStep) :: tail =>
          processSteps(
            tail,
            step match {
              case step: N =>
                if (step.nodes.forall(_.graph == this)) {
                  step.nodes match {
                    case Nil =>
                      graph
                        .nodes()
                        .map(createTraverser(_))
                    case list: List[Node] =>
                      list.toStream
                        .map(createTraverser(_))
                  }
                } else {
                  val iris = step.nodes.flatMap(n => n.iri :: n.iris.toList).filter(_.nonEmpty).toSet
                  graph
                    .nodes()
                    .filter(n => iris.intersect(n.iris).nonEmpty)
                    .map(createTraverser(_))
                }
              case step: E =>
                if (step.links.forall(_.graph == this)) {
                  step.links match {
                    case Nil =>
                      graph
                        .edges()
                        .map(createTraverser(_))
                    case list: List[Node] =>
                      list.toStream
                        .map(createTraverser(_))
                  }
                } else {
                  //TODO: filter by from-iri -> key --> to-iri
                  val iris = step.links.flatMap(e => e.iri :: e.iris.toList).filter(_.nonEmpty).toSet
                  graph
                    .edges()
                    .filter(e => iris.intersect(e.iris).nonEmpty)
                    .map(createTraverser(_))
                }
              case step: V =>
                if (step.values.forall {
                      case value: Value[Any] => value.graph == this
                      case v                 => false
                    }) {
                  step.values match {
                    case Nil =>
                      graph
                        .values()
                        .map(createTraverser(_))
                    case list: List[Node] =>
                      list.toStream
                        .map(createTraverser(_))
                  }
                } else {
                  val values = step.values.map {
                    case value: Value[Any] => value.value
                    case v                 => v
                  }
                  val iris = step.values
                    .collect { case v: Value[_] => v }
                    .flatMap(v => v.iri :: v.iris.toList)
                    .filter(_.nonEmpty)
                    .toSet
                  graph
                    .values()
                    .filter(v => iris.intersect(v.iris).nonEmpty || values.contains(v.value))
                    .map(createTraverser(_))
                }
              case step: R =>
                if (step.resources.forall(_.graph == this)) {
                  step.resources match {
                    case Nil =>
                      graph
                        .resources()
                        .map(createTraverser(_))
                    case list: List[Node] =>
                      list.toStream
                        .map(createTraverser(_))
                  }
                } else {
                  graph
                    .resources()
                    .filter(step.resources.contains)
                    .map(createTraverser(_))
                  val values = step.resources.collect {
                    case value: Value[Any] => value.value
                  }
                  val iris = step.resources
                    .flatMap(r => r.iri :: r.iris.toList)
                    .filter(_.nonEmpty)
                    .toSet
                  graph
                    .resources()
                    .filter(r => iris.intersect(r.iris).nonEmpty || values.contains(r.value))
                    .map(createTraverser(_))
                }
            }
          )
        case steps => processSteps(steps, graph.resources().map(createTraverser(_)))
      }
  }

  private def processSteps(steps: List[Step], traverers: Stream[Traverser[_]])(
      implicit graph: Graph): Task[Stream[Traverser[_]]] =
    Task.eval(steps.head).flatMap {
      case step =>
        Task { traverers }
    }
}
