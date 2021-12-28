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

final case class Traversal[+ST <: ClassType[?], +ET <: ClassType[?], Steps <: Tuple] private (steps: Steps) {

  def addStep[ST <: ClassType[?], ET <: ClassType[?], NewStep <: Step](
    newStep: NewStep
  ): Traversal[ST, ET, Traversal.StepsConcat[Steps, NewStep]] =
    new Traversal[ST, ET, Traversal.StepsConcat[Steps, NewStep]](Traversal.StepsConcat(steps, newStep))

  def addSteps[ST <: ClassType[?], ET <: ClassType[?], NewSteps <: Tuple](
    newSteps: NewSteps
  ): Traversal[ST, ET, Traversal.StepsConcat[Steps, NewSteps]] =
    new Traversal[ST, ET, Traversal.StepsConcat[Steps, NewSteps]](Traversal.StepsConcat(steps, newSteps))
}
// (
//   val st: ST,
//   val et: ET
// )

object Traversal:

  def apply(): Traversal[ResourceType[Any], ResourceType[Any], EmptyTuple] =
    new Traversal(EmptyTuple) // (ResourceType[Any](), ResourceType[Any]())

  def empty[ST <: ClassType[?], ET <: ClassType[?]]: Traversal[ST, ET, EmptyTuple] =
    new Traversal(EmptyTuple)

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

  type StartType[traversal] <: ClassType[?] = traversal match {
    case Traversal[st, et, steps] => st
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
