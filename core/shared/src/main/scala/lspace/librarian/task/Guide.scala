package lspace.librarian.task

import java.time.Instant

import lspace.librarian.logic.Assistent
import lspace.librarian.traversal.step.Project
import lspace.librarian.traversal.{
  BranchStep,
  ClipStep,
  CollectingBarrierStep,
  FilterBarrierStep,
  FilterStep,
  Librarian,
  MoveStep,
  RearrangeBarrierStep,
  ReducingBarrierStep,
  ResourceStep,
  Segment,
  Step,
  TraversalPath
}
import lspace.structure.{Graph, Resource}

trait Guide[F[_]] {
  def assistent: Assistent

  def toValue(v: Any): Any = v match {
    case librarian: Librarian[Any] => toValue(librarian.get)
    case resource: Resource[Any]   => resource.value
    case it: Map[Any, Any]         => it.map(t => toValue(t._1) -> toValue(t._2))
    case it: Iterable[Any]         => it.map(toValue)
    case (v1, v2)                  => (toValue(v1), toValue(v2))
    case (v1, v2, v3)              => (toValue(v1), toValue(v2), toValue(v3))
    case (v1, v2, v3, v4)          => (toValue(v1), toValue(v2), toValue(v3), toValue(v4))
    case value                     => value
  }

  private case class SimpleLibrarian[+T](get: T,
                                         path: TraversalPath = TraversalPath(),
                                         loops: Int = 0,
                                         mit: Option[Instant] = None,
                                         permissions: List[String] = List())
      extends Librarian[T] {
    def apply[V](get: V,
                 path: TraversalPath = TraversalPath(),
                 loops: Int = 0,
                 mit: Option[Instant] = None,
                 permissions: List[String] = List()): Librarian[V] =
      new SimpleLibrarian[V](get, path, loops, mit, permissions)
  }

  def createLibrarian[T](get: T,
                         path: TraversalPath = TraversalPath(),
                         loops: Int = 0,
                         mit: Option[Instant] = None,
                         permissions: List[String] = List()): Librarian[T] =
    new SimpleLibrarian[T](get, path, loops, mit, permissions)

  def buildTraversal[Out](segments: List[Segment[_]]): Graph => F[Out]
  def traversalToF(segments: List[Segment[_]])(implicit graph: Graph): Librarian[Any] => F[Any]

  def buildNextStep(steps: List[Step], segments: List[Segment[_]])(implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def resourceStep(step: ResourceStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def moveStep(step: MoveStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def filterStep(step: FilterStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def clipStep(step: ClipStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def branchStep(step: BranchStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def collectingBarrierStep(step: CollectingBarrierStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def reducingBarrierStep(step: ReducingBarrierStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def filterBarrierStep(step: FilterBarrierStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def rearrangeBarrierStep(step: RearrangeBarrierStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def projectStep(step: Project, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]
}
