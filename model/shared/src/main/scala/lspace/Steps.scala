package lspace
package librarian
package step

sealed trait Step
// sealed trait Terminate        extends Step
sealed trait FilterStep       extends Step
sealed trait HasStep          extends FilterStep
sealed trait GlobalFilterStep extends FilterStep
sealed trait EnvironmentStep  extends Step
sealed trait TraverseStep     extends Step
sealed trait LabelStep        extends Step
sealed trait ProjectionStep   extends Step
sealed trait MapStep          extends ProjectionStep with GroupingStep
sealed trait ResourceStep     extends Step
sealed trait GraphStep        extends ResourceStep
sealed trait BranchStep       extends Step
sealed trait MoveStep         extends BranchStep
sealed trait ClipStep         extends FilterStep
sealed trait GroupingStep     extends Step
sealed trait BarrierStep      extends Step
sealed trait GroupingBarrierStep extends BarrierStep with GroupingStep //with TraverseStep
sealed trait ReducingStep extends Step
sealed trait ReducingBarrierStep extends BarrierStep with ReducingStep //with TraverseStep
sealed trait FilterBarrierStep extends BarrierStep with FilterStep     //with ReducingStep
sealed trait RearrangeStep        extends Step
sealed trait RearrangeBarrierStep extends RearrangeStep with BarrierStep

object And:
  type Traversals[X] = lspace.Traversals[X]
case class And[traversals](traversals: And.Traversals[traversals]) extends FilterStep

object As:
  type Label = String with Singleton

case class As[label <: As.Label](label: label) extends LabelStep

object Choose:
  type EndType[left, right] = Traversal.EndType[left] | Traversal.EndType[right]

case class Choose[by, left, right](
  by: AnyTraversal[by],
  left: AnyTraversal[left],
  right: AnyTraversal[right]
) extends BranchStep

object Coalesce:
  type Traversals[X] = lspace.Traversals[X]
  // type EndType[X] <: ClassType[?] = X match {
  //   case l | r => Traversal.EndType[l] | Traversal.EndType[r]
  // }

  type EndType[X] <: ClassType[?] = X match {
    case traversal *: EmptyTuple => Traversal.EndType[traversal]
    case traversal *: traversals => Traversal.EndType[traversal] | EndType[traversals]
    case _                       => Traversal.EndType[X]
  }

case class Coalesce[traversals](traversals: Coalesce.Traversals[traversals]) extends BranchStep

object Coin:
  import eu.timepit.refined.api._
  import eu.timepit.refined.numeric._

  type Min = 0.0
  type Max = 1.0
  type Probability = Double Refined Interval.Closed[Min, Max]
  
// type Between[X] = X match {
//   // case p if p > Min && p < Max => p
//   case p => p
// }
case class Coin(p: Coin.Probability) extends FilterStep

// object Constant:
case class Constant[T](value: T) extends TraverseStep

case object Count                extends BarrierStep

case object Dedup                extends GlobalFilterStep

// case class Drop()
case class E(edges: List[Edge[?, ?]] = Nil)                                   extends ResourceStep
case object From                                                              extends TraverseStep
case class G(graphs: List[Graph] = Nil)                                       extends GraphStep


case class Group[by, value](by: AnyTraversal[by], value: AnyTraversal[value]) extends GroupingBarrierStep

// enum Has[key <: Key] extends HasStep:
//   case HasKey[key <: Key](
//     key: key
//   ) extends Has[key]
//   case HasKeyWithValue[key <: Key, predicate <: P[_]](
//     key: key,
//     predicate: predicate
//   ) extends Has[key]

//   type HasType[X] = X match {
//     case X => X
//   }

case class HasId()
case class HasIri()

object HasLabel:
  type EndType[X <: ClassType[?]] <: ClassType[?] = X match {
    case ClassType[?] => X
  }

case class HasLabel[T]()
case class HasNot()
case class HasValue()

sealed trait Has[key <: Key] extends HasStep
case class HasKey[key <: Key](
  key: key
) extends Has[key]
case class HasKeyWithValue[key <: Key, predicate <: P[_]](
  key: key,
  predicate: predicate
) extends Has[key]

case class Head()
case class Id()
case class InE()
case class InEMap()
case class InMap()
case class Is()
case class Label()
case class Last()
case class Limit()
case class Local()
case class Max()
case class Mean()
case class Min()
case class N()
case class Not()
case class Or()
case class Order()
case class OutE()
case class OutEMap()
case class OutMap()
case class Path()
case class Project()
case class R()
case class Range()
case class Repeat()
case class Select()
case class Skip()
case class Sum()
case class Tail()
case class TimeLimit()
case object To
case class Union()
case class V()
case class Where()

// enum In[key <: Key] extends MoveStep:
//   case InKey[key <: Key](key: key)                                                   extends In[key]
//   case InKeyPredicate[key <: Key, predicate <: P[_]](key: key, predicate: predicate) extends In[key]
sealed trait In[key <: Key]                                                              extends MoveStep
case class InKey[key <: Key](key: key)                                                   extends In[key]
case class InKeyPredicate[key <: Key, predicate <: P[_]](key: key, predicate: predicate) extends In[key]

sealed trait Out[key <: Key]                                                              extends MoveStep
case class OutKey[key <: Key](key: key)                                                   extends Out[key]
case class OutKeyPredicate[key <: Key, predicate <: P[_]](key: key, predicate: predicate) extends Out[key]
