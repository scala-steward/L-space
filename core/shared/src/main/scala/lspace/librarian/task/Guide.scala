package lspace.librarian.task

import java.time.Instant

import lspace.librarian.logic.Assistent
import lspace.librarian.traversal.step._
import lspace.librarian.traversal.{
  BranchStep,
  ClipStep,
  CollectingBarrierStep,
  FilterBarrierStep,
  FilterStep,
  Librarian,
  MapStep,
  MoveStep,
  RearrangeBarrierStep,
  ReducingBarrierStep,
  ResourceStep,
  Segment,
  Step,
  Traversal,
  TraversalPath,
  UntypedTraversal
}
import lspace.structure.{ClassType, Graph, Resource}
import shapeless.HList

trait Guide[F[_]] {

  def executeTraversal[Out](
      traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList]): Graph => F[Out] = { graph =>
    graph.executeTraversal(traversal, this).asInstanceOf[F[Out]]
  }

  def buildTraversal[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList]): Graph => F[Out]
}
trait LocalGuide[F[_]] extends Guide[F] {
  def assistent: Assistent

  def toValue(v: Any): Any = v match {
    case librarian: Librarian[_] => toValue(librarian.get)
    case Some(value)             => Some(toValue(value))
    case resource: Resource[_]   => resource.value
    case it: Map[_, _]           => it.map(t => toValue(t._1) -> toValue(t._2))
    case it: Iterable[_]         => it.map(toValue)
    case product: Product =>
      product match {
        case (v1, v2)                 => (toValue(v1), toValue(v2))
        case (v1, v2, v3)             => (toValue(v1), toValue(v2), toValue(v3))
        case (v1, v2, v3, v4)         => (toValue(v1), toValue(v2), toValue(v3), toValue(v4))
        case (v1, v2, v3, v4, v5)     => (toValue(v1), toValue(v2), toValue(v3), toValue(v4), toValue(v5))
        case (v1, v2, v3, v4, v5, v6) => (toValue(v1), toValue(v2), toValue(v3), toValue(v4), toValue(v5), toValue(v6))
        case (v1, v2, v3, v4, v5, v6, v7) =>
          (toValue(v1), toValue(v2), toValue(v3), toValue(v4), toValue(v5), toValue(v6), toValue(v7))
        case value => value
      }
    case value => value
  }

  def findFirstContainer(steps: List[Step]): Option[Step] = {
    steps.collectFirst {
      case step: Group[_, _, _, _] => step
      case step: Project[_]        => step
      case step: MapStep           => step
//      case step: Head  => step
//      case step: Last  => step
//      case step: Count  => step
//      case step: Mean  => step
//      case step: Min  => step
//      case step: Max  => step
    }
  }
  def collectContainers(steps: List[Step]): List[Step] = {
    steps.collect {
      case step: Group[_, _, _, _] => step
      case step: Project[_]        => step
      case step: MapStep           => step
      case step: Head              => step
      case step: Last              => step
      case step: Count             => step
      case step: Mean              => step
      case step: Min               => step
      case step: Max               => step
      case step: Sum               => step
    }
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

  def traversalToF(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList])(
      implicit graph: Graph): Librarian[Any] => F[Any]

  def buildNextStep(steps: List[Step], segments: List[Segment[_]])(implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def resourceStep(step: ResourceStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def moveStep(step: MoveStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def filterStep(step: FilterStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def clipStep[T](step: ClipStep)(implicit graph: Graph): F[T] => F[T]

  def branchStep(step: BranchStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def collectingBarrierStep(step: CollectingBarrierStep,
                            steps: List[Step],
                            segments: List[Segment[_]],
                            isRootGroup: Boolean = false)(implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def reducingBarrierStep(step: ReducingBarrierStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def filterBarrierStep(step: FilterBarrierStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def rearrangeBarrierStep(step: RearrangeBarrierStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]

  def projectStep[Traversals <: HList](step: Project[Traversals], steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Graph): F[Librarian[Any]] => F[Any]
}
