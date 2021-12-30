package lspace

import lspace.librarian.step.Step

// import eu.timepit.refined._
// import eu.timepit.refined.api.Refined
// import eu.timepit.refined.string.Uri

/** @tparam R
  *   resource self-type
  */
sealed trait Resource[+R] extends Matchable derives CanEqual

open class Node extends Resource[Node]

/** @tparam S
  *   outV-type, edge-start
  * @tparam E
  *   inV-type, edge-end
  */
open class Edge[+S, +E] extends Resource[Edge[S, E]]

/** @tparam V
  *   value-type
  */
open class Value[+V](v: V) extends Resource[Value[V]]

open class Graph extends Resource[Graph]

final case class Traversal[ST <: ClassType[?], ET <: ClassType[?], Steps <: Tuple] private (steps: Steps)(
  val st: ST,
  val et: ET
) {
  type StartType = ST
  type EndType   = ET

  // given startType: ST = st
  // given endType: ET   = et
  // import lspace.librarian.step._
  // type AddableStep[X] = X match {
  //   case Max[Traversal[ET, e, steps]] => X
  // }
  
  protected[lspace] def addStep[ST <: ClassType[?], ET <: ClassType[?], NewStep <: Step](
    newStep: NewStep,
    et: ET = this.et,
    st: ST = this.st
  ): Traversal[ST, ET, Traversal.StepsConcat[Steps, NewStep]] =
    new Traversal[ST, ET, Traversal.StepsConcat[Steps, NewStep]](Traversal.StepsConcat(steps, newStep))(st, et)

  // def addSteps[ET <: ClassType[?], NewSteps <: Tuple](
  //   newSteps: NewSteps
  // ): Traversal[ST, ET, Traversal.StepsConcat[Steps, NewSteps]] =
  //   new Traversal[ST, ET, Traversal.StepsConcat[Steps, NewSteps]](Traversal.StepsConcat(steps, newSteps))(newSt, newEt)

  protected[lspace] def withEndType[nET <: ClassType[?]](newEt: nET): Traversal[ST, nET, Steps] =
    new Traversal[ST, nET, Steps](steps)(st, newEt)

  protected[lspace] def withStartType[nST <: ClassType[?]](newSt: nST): Traversal[nST, ET, Steps] =
    new Traversal[nST, ET, Steps](steps)(newSt, et)
}

object Traversal:

  def apply(): Traversal[ResourceType[Any], ResourceType[Any], EmptyTuple] =
    new Traversal(EmptyTuple)(ResourceType, ResourceType)

  def empty[ST <: ClassType[?], ET <: ClassType[?]](
    st: ST,
    et: ET
  ): Traversal[ST, ET, EmptyTuple] =
    new Traversal(EmptyTuple)(st, et)

  // def apply[ST <: ClassType[?], ET <: ClassType[?], Steps <: Tuple](
  //   steps: Steps
  // ): Traversal[ST, ET, StepsTuple[Steps]] =
  //   new Traversal[ST, ET, StepsTuple[Steps]](StepsTuple(steps))

  // def apply[ST <: ClassType[?], ET <: ClassType[?], Steps <: Tuple, NewStep <: Step](
  //   steps: Steps,
  //   newStep: NewStep
  // ): Traversal[ST, ET, StepsConcat[Steps, NewStep]] =
  //   new Traversal[ST, ET, StepsConcat[Steps, NewStep]](StepsConcat(steps, newStep))

  // def apply[ST <: ClassType[?], ET <: ClassType[?], Steps <: Tuple, NewSteps <: Tuple](
  //   steps: Steps,
  //   newSteps: NewSteps
  // ): Traversal[ST, ET, StepsConcat[Steps, NewSteps]] =
  //   new Traversal[ST, ET, StepsConcat[Steps, NewSteps]](StepsConcat(steps, newSteps))

  def StartType[traversal](traversal: traversal): StartType[traversal] = traversal match {
    case traversal: Traversal[s, e, steps] => traversal.st
  }
  type StartType[traversal] <: ClassType[?] = traversal match {
    case Traversal[st, et, steps] => st
  }

  def EndType[traversal](traversal: traversal): EndType[traversal] = traversal match {
    case traversal: Traversal[s, e, steps] => traversal.et
  }
  type EndType[traversal] <: ClassType[?] = traversal match {
    case Traversal[st, et, steps] => et
  }

  type StepType[X <: Step] <: Step = X match {
    case Step => X
  }
  def StepType[X <: Step](x: X): StepType[X] = x match {
    case step: Step => x.asInstanceOf[StepType[X]]
  }

  type StepsTuple[X] <: Tuple = X match {
    case step *: EmptyTuple => StepType[step] *: EmptyTuple
    case step *: steps      => StepType[step] *: StepsTuple[steps]
    case EmptyTuple         => EmptyTuple
    case _                  => StepType[X] *: EmptyTuple
  }
  def StepsTuple[X](x: X): StepsTuple[X] = (x match {
    case (step: Step) *: EmptyTuple => StepType(step) *: EmptyTuple
    case (step: Step) *: steps      => StepType(step) *: StepsTuple(steps)
    case EmptyTuple                 => EmptyTuple
    case (step: Step)               => StepType(step) *: EmptyTuple
  }).asInstanceOf[StepsTuple[X]]

  type StepsConcat[Steps <: Tuple, Step] = StepsTuple[Tuple.Concat[Steps, StepsTuple[Step]]]
  def StepsConcat[Steps <: Tuple, Step](steps: Steps, step: Step): StepsConcat[Steps, Step] =
    StepsTuple(steps ++ StepsTuple(step)).asInstanceOf[StepsConcat[Steps, Step]]

  // import lspace.librarian.step._
  // type TraversalStepConcat[traversal <: Traversal[?, ?, ?], step <: Step] <: Traversal[?, ?, ?] =
  //   traversal match {
  //     case Traversal[s, e, steps] =>
  //       step match {
  //         case Max[Traversal[e, e0, steps0]] => Traversal[s, e, StepsConcat[steps, step]]
  //       }
  //   }

  // import compiletime.asMatchable      

  // def TraversalStepConcat[traversal <: Traversal[?, ?, ?], step <: Step](
  //   traversal: traversal,
  //   step: step
  // ): TraversalStepConcat[traversal, step] =
  //   traversal.asMatchable match {
  //     case traversal: Traversal[s, e, steps] =>
  //       step match {
  //         case max: Max[t] if max.traversal.et == traversal.et =>
  //           // traversal.copy(StepsConcat(traversal.steps, max))()
  //           new Traversal(StepsConcat(traversal.steps, max))(traversal.st, traversal.et)
  //       }
  //   }

end Traversal

type AnyTraversal[X] = X match {
  case Traversal[s, e, steps] => Traversal[s, e, steps]
}
def AnyTraversal[X](traveral: X): AnyTraversal[X] = traveral match {
  case traversal: Traversal[s, e, steps] => traversal // .asInstanceOf[Traversals[X]]
}

type Traversals[X] <: Tuple = X match {
  case Traversal[s, e, steps] *: EmptyTuple => Traversal[s, e, steps] *: EmptyTuple
  case Traversal[s, e, steps] *: traversals => Traversal[s, e, steps] *: Traversals[traversals]
  case Traversal[s, e, steps]               => Traversal[s, e, steps] *: EmptyTuple
}
def Traversals[X](traverals: X): Traversals[X] = traverals match {
  case (traversal: Traversal[s, e, steps]) *: EmptyTuple => (traversal *: EmptyTuple).asInstanceOf[Traversals[X]]
  case (traversal: Traversal[s, e, steps]) *: traversals =>
    (traversal *: Traversals(traversals)).asInstanceOf[Traversals[X]]
  case traversal: Traversal[s, e, steps] => (traversal *: EmptyTuple).asInstanceOf[Traversals[X]]
}

def TraversalsApply[ET <: ClassType[?], X](in: Traversal[ET, ET, EmptyTuple], traverals: X): Traversals[X] =
  traverals match {
    case (traversal: Traversal[s, e, steps]) *: EmptyTuple => (traversal *: EmptyTuple).asInstanceOf[Traversals[X]]
    case (traversal: Traversal[s, e, steps]) *: traversals =>
      (traversal *: Traversals(traversals)).asInstanceOf[Traversals[X]]
    case traversal: Traversal[s, e, steps] => (traversal *: EmptyTuple).asInstanceOf[Traversals[X]]
  }
