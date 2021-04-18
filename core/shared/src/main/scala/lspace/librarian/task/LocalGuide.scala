package lspace.librarian.task

import cats.Functor
import lspace.librarian.logic.Assistent
import lspace.librarian.traversal.step._
import lspace.librarian.traversal._
import lspace.structure.Graph
import shapeless.HList

abstract class LocalGuide[F[_]: Functor] extends Guide[F] {
  def assistent: Assistent

  def buildNextStep(steps: List[Step])(implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]] =
    steps match {
      case Nil =>
        obs: F[Librarian[Any]] => obs
      case step :: steps =>
        step match {
          case _: GraphStep =>
            _ =>
              raiseError[Librarian[Any]](
                new Exception("AsyncGuide does not support federated queries, RemoteGuide can!")
              )
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
                reducingStep(step).andThen(buildNextStep(steps))
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
                clipStep(step).andThen(buildNextStep(steps))
              case _ => filterStep(step, steps)
            }
          case step: ProjectionStep =>
            projectionStep(step, steps)
          case step: EnvironmentStep =>
            environmentStep(step, steps)
          case step: LabelStep =>
            labelStep(step, steps)
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

  def collectingBarrierStep(step: GroupingBarrierStep, steps: List[Step], isRootGroup: Boolean = false)(implicit
    graph: Graph
  ): F[Librarian[Any]] => F[Librarian[Any]]

  def reducingBarrierStep(step: ReducingBarrierStep, steps: List[Step])(implicit
    graph: Graph
  ): F[Librarian[Any]] => F[Librarian[Any]]

//  def filterBarrierStep(step: FilterBarrierStep, steps: List[Step])(
//      implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]

  def rearrangeBarrierStep(step: RearrangeBarrierStep, steps: List[Step])(implicit
    graph: Graph
  ): F[Librarian[Any]] => F[Librarian[Any]]

  def projectStep[Traversals <: HList](step: Project[Traversals], steps: List[Step])(implicit
    graph: Graph
  ): F[Librarian[Any]] => F[Librarian[Any]]

  def projectionStep(step: ProjectionStep, steps: List[Step])(implicit
    graph: Graph
  ): F[Librarian[Any]] => F[Librarian[Any]]

  def environmentStep(step: EnvironmentStep, steps: List[Step])(implicit
    graph: Graph
  ): F[Librarian[Any]] => F[Librarian[Any]] = step match {
    case step: TimeLimit => //todo, scan for other TimeLimit steps with time == None (cancels the nearest preceding time limit step)
      val nextStep = buildNextStep(steps)
      import scala.concurrent.duration._
      val f = (obs: F[Librarian[Any]]) =>
        step.time match {
          case Some(time) => takeByTimeSpan(obs, time.millis: FiniteDuration)
          case None       => obs
        }
      f.andThen(nextStep)
    //                nextStep andThen f
  }
}
