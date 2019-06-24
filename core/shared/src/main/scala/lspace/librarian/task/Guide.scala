package lspace.librarian.task

import java.time.Instant

import cats.Functor
import cats.implicits._
import lspace.librarian.logic.Assistent
import lspace.librarian.traversal.step._
import lspace.librarian.traversal.{
  BarrierStep,
  BranchStep,
  ClipStep,
  EnvironmentStep,
  FilterBarrierStep,
  FilterStep,
  GraphStep,
  GroupingBarrierStep,
  Librarian,
  MapStep,
  MoveStep,
  ProjectionStep,
  RearrangeBarrierStep,
  RearrangeStep,
  ReducingBarrierStep,
  ReducingStep,
  ResourceStep,
  Step,
  Traversal,
  TraversalPath,
  TraverseStep,
  UntypedTraversal
}
import lspace.structure.{ClassType, Graph, Resource}
import shapeless.HList

import scala.concurrent.duration.FiniteDuration

abstract class Guide[F[_]: Functor] {

  type K[_]
//  implicit def kFunctor: Functor[F]

  def executeTraversal[Out](
      traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList]): Graph => F[Out] = { graph =>
    graph.executeTraversal(traversal, this).asInstanceOf[F[Out]]
  }

//  implicit class WithF[A](list: F[A]) {
//    def map[B](f: A => B)(implicit func: Functor[F]): F[B] = func.map(list)(f)

  def emptyF[T]: F[T]
  def createF[T](t: T): F[T]
  def createLibrarian[T](get: T,
                         path: TraversalPath = TraversalPath(),
                         loops: Int = 0,
                         mit: Option[Instant] = None,
                         permissions: List[String] = List()): Librarian[T] =
    SimpleLibrarian[T](get)(path, loops, mit, permissions)

  protected def head(f: F[Librarian[Any]]): K[Librarian[Any]]
  protected def headOption(f: F[Librarian[Any]]): K[Librarian[Any]]
  protected def headOptionOption(f: F[Librarian[Any]]): K[Librarian[Any]]
  protected def toList(f: F[Librarian[Any]]): K[Librarian[Any]]
  protected def toSet(f: F[Librarian[Any]]): K[Librarian[Any]]
  protected def toMap(f: F[Librarian[(Any, Any)]]): K[Librarian[Any]]
  protected def takeByTimeSpan(f: F[Librarian[Any]], timespan: FiniteDuration): F[Librarian[Any]]

  def raiseError[T](ex: Exception): F[T]

  def buildTraversal[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList]): Graph => F[Out] = {
    implicit graph: Graph =>
      traversal.stepsList match {
        case Nil => emptyF[Out].asInstanceOf[F[Any]]
        case steps =>
          createF(1)
          val nextStep = buildNextStep(steps)
          nextStep(createF(createLibrarian[Any](null))).asInstanceOf[F[Any]]
      }
  }.andThen(_.map(toValue).asInstanceOf[F[Out]])

  def buildNextStep(steps: List[Step])(implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]

  def traversalToF(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList])(
      implicit graph: Graph): Librarian[Any] => F[Librarian[Any]] = {
    traversal.stepsList match {
      case Nil =>
        librarian: Librarian[Any] =>
          createF(librarian)
      case steps =>
        val nextStep = buildNextStep(steps)
        librarian: Librarian[Any] =>
          nextStep(createF(librarian))
    }
  }

  def traversalsToF(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList])(
      implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]] = {
    traversal.stepsList match {
      case Nil =>
        librarians: F[Librarian[Any]] =>
          librarians
      case steps =>
        val nextStep = buildNextStep(steps)
        librarians: F[Librarian[Any]] =>
          nextStep(librarians)
    }
  }

  //transform a nested traversal result to it's expected type
  def tweakEnd(traversal: Traversal[ClassType[Any], ClassType[Any], HList]): F[Librarian[Any]] => K[Librarian[Any]] = {
    //FilterBarrierStep
    //ReducingBarrierStep
    import scala.collection.immutable.::
    traversal.stepsList.reverse.span {
      case _: Head | _: Last | _: Min | _: Max | _: Dedup                               => false
      case _: FilterStep | _: EnvironmentStep | _: Project[_] | _: Id | _: To | _: From => true
      case _                                                                            => false
    } match {
      case (toIgnore, (_: Head | _: Last | _: Min | _: Max | _: Mean | _: Sum) :: steps) =>
        steps.span {
          case _: Head | _: Last | _: Min | _: Max | _: Mean | _: Sum | _: FilterStep | _: EnvironmentStep => true
          case _                                                                                           => false
        } match {
          case (toIgnore, List(step: Group[_, _, _, _], _*)) =>
            observable: F[Librarian[Any]] =>
              headOption(observable)
//          case (toIgnore, any) if toIgnore.exists(s => s.isInstanceOf[Mean] || s.isInstanceOf[Sum]) =>
//            observable: F[Librarian[Any]] =>
//              headOption(observable)
          case _ =>
            observable: F[Librarian[Any]] =>
              headOption(observable)
        }
      case (toIgnore, List(step: Dedup, _*)) =>
        observable: F[Librarian[Any]] =>
          toSet(observable)
      case (List(), List(step: Group[_, _, _, _], _*)) =>
        observable: F[Librarian[Any]] =>
          toMap(observable.asInstanceOf[F[Librarian[(Any, Any)]]])
      case (toIgnore, (step: Count) :: steps) if toIgnore.nonEmpty && toIgnore.exists(_.isInstanceOf[FilterStep]) =>
        observable: F[Librarian[Any]] =>
          headOption(observable)
      case (List(), (step: Count) :: steps) =>
        observable: F[Librarian[Any]] =>
          head(observable)
      case (List(), (step: ProjectionStep) :: steps) =>
        observable: F[Librarian[Any]] =>
          head(observable)
      case _ =>
        observable: F[Librarian[Any]] =>
          toList(observable)
    }
  }

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
        case (v1, v2, v3, v4, v5, v6, v7, v8) =>
          (toValue(v1), toValue(v2), toValue(v3), toValue(v4), toValue(v5), toValue(v6), toValue(v7), toValue(v8))
        case (v1, v2, v3, v4, v5, v6, v7, v8, v9) =>
          (toValue(v1),
           toValue(v2),
           toValue(v3),
           toValue(v4),
           toValue(v5),
           toValue(v6),
           toValue(v7),
           toValue(v8),
           toValue(v9))
        case value => value
      }
    case value => value
  }
}

abstract class LocalGuide[F[_]: Functor] extends Guide[F] {
  def assistent: Assistent

  def buildNextStep(steps: List[Step])(implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]] = {
    steps match {
      case Nil =>
        obs: F[Librarian[Any]] =>
          obs
      case step :: steps =>
        step match {
          case step: GraphStep =>
            obs =>
              raiseError[Librarian[Any]](
                new Exception("AsyncGuide does not support federated queries, RemoteGuide can!"))
          case step: ResourceStep =>
            resourceStep(step, steps)
          case step: BranchStep =>
            step match {
              case step: MoveStep =>
                moveStep(step, steps)
              case _ => branchStep(step, steps)
            }
          case step: TraverseStep =>
            traverseStep(step, steps)
          case step: RearrangeStep =>
            step match {
              case step: RearrangeBarrierStep =>
                rearrangeBarrierStep(step, steps)
            }
          case step: ReducingStep =>
            step match {
              case step: ReducingBarrierStep =>
                reducingBarrierStep(step, steps)
              case _ =>
                reducingStep(step) andThen buildNextStep(steps)
            }
          case step: BarrierStep =>
            step match {
//              case step: FilterBarrierStep =>
//                filterBarrierStep(step, steps)
              case step: GroupingBarrierStep =>
                collectingBarrierStep(step, steps)
              case step: Count =>
                countStep(step, steps)
            }
          case step: FilterStep =>
            step match {
              case step: ClipStep =>
                clipStep(step) andThen buildNextStep(steps)
              case _ => filterStep(step, steps)
            }
          case step: ProjectionStep =>
            projectionStep(step, steps)
          case step: EnvironmentStep =>
            environmentStep(step, steps)
        }
    }
  }

  def traverseStep(step: TraverseStep, steps: List[Step])(implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]
  def resourceStep(step: ResourceStep, steps: List[Step])(implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]

  def moveStep(step: MoveStep, steps: List[Step])(implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]

  def filterStep(step: FilterStep, steps: List[Step])(implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]

  def reducingStep[T](step: ReducingStep)(implicit graph: Graph): F[Librarian[T]] => F[Librarian[T]]
  def clipStep[T](step: ClipStep)(implicit graph: Graph): F[Librarian[T]] => F[Librarian[T]]
  def countStep(step: Count, steps: List[Step])(implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]

  def branchStep(step: BranchStep, steps: List[Step])(implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]

  def collectingBarrierStep(step: GroupingBarrierStep, steps: List[Step], isRootGroup: Boolean = false)(
      implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]

  def reducingBarrierStep(step: ReducingBarrierStep, steps: List[Step])(
      implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]

//  def filterBarrierStep(step: FilterBarrierStep, steps: List[Step])(
//      implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]

  def rearrangeBarrierStep(step: RearrangeBarrierStep, steps: List[Step])(
      implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]

  def projectStep[Traversals <: HList](step: Project[Traversals], steps: List[Step])(
      implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]

  def projectionStep(step: ProjectionStep, steps: List[Step])(
      implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]

  def environmentStep(step: EnvironmentStep, steps: List[Step])(
      implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]] = step match {
    case step: TimeLimit => //todo, scan for other TimeLimit steps with time == None (cancels the nearest preceding time limit step)
      val nextStep = buildNextStep(steps)
      import scala.concurrent.duration._
      val f = (obs: F[Librarian[Any]]) =>
        step.time match {
          case Some(time) => takeByTimeSpan(obs, time.millis millis: FiniteDuration)
          case None       => obs
      }
      f andThen nextStep
    //                nextStep andThen f
  }
}
