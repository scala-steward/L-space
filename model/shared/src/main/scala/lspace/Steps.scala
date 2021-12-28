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

object MapStep:
  import Key._

  type KeyTuple[X] <: Tuple = X match {
    case key *: EmptyTuple => KeyLike[key] *: EmptyTuple
    case key *: keys       => KeyLike[key] *: KeyTuple[keys]
    case _                 => KeyLike[X] *: EmptyTuple
  }
  def KeyTuple[X](x: X): KeyTuple[X] = (x match {
    case key *: EmptyTuple => (KeyLike(key) *: EmptyTuple)
    case key *: keys       => (KeyLike(key) *: KeyTuple(keys))
    case _                 => (KeyLike(x) *: EmptyTuple)
  }).asInstanceOf[KeyTuple[X]]

sealed trait MapStep extends ProjectionStep with GroupingStep

sealed trait ResourceStep extends Step
sealed trait GraphStep    extends ResourceStep
sealed trait BranchStep   extends Step
// object MoveStep:
//   type Label[X] <: Tuple = X match {
//     case Key[n] *: EmptyTuple => n *: EmptyTuple
//     case Key[n] *: keys       => n *: Label[keys]
//     case Key[n]               => n *: EmptyTuple
//     case Nothing              => EmptyTuple
//   }
//   implicit def Label[X](keys: X): Label[X] = (keys match {
//     case (key: Key[?]) *: EmptyTuple => key *: EmptyTuple
//     case (key: Key[?]) *: keys       => key *: Label(keys)
//     case EmptyTuple                  => EmptyTuple
//     case (key: Key[?])               => key *: EmptyTuple
//     case _                           => EmptyTuple
//   }).asInstanceOf[Label[X]]
sealed trait MoveStep     extends BranchStep
sealed trait ClipStep     extends FilterStep
sealed trait GroupingStep extends Step
sealed trait BarrierStep  extends Step
sealed trait GroupingBarrierStep extends BarrierStep with GroupingStep //with TraverseStep
sealed trait ReducingStep extends Step
sealed trait ReducingBarrierStep extends BarrierStep with ReducingStep //with TraverseStep
sealed trait FilterBarrierStep extends BarrierStep with FilterStep     //with ReducingStep
sealed trait RearrangeStep        extends Step
sealed trait RearrangeBarrierStep extends RearrangeStep with BarrierStep

object And:
  type Traversals[X] = lspace.Traversals[X]
  def Traversals[X](traversals: X): Traversals[X] = lspace.Traversals(traversals)
  type Step[X] = And[lspace.Traversals[X]]

  def apply[traversals](traversals: traversals): And[Traversals[traversals]] = new And(Traversals(traversals))

case class And[traversals] private (traversals: traversals) extends FilterStep

object As:
  type Label = String with Singleton

case class As[label <: As.Label](label: label) extends LabelStep

object Choose:
  type By[X] = lspace.AnyTraversal[X]
  def By[X](x: X) = lspace.AnyTraversal(x)

  type Left[X] = lspace.AnyTraversal[X]
  def Left[X](x: X) = lspace.AnyTraversal(x)

  type Right[X] = lspace.AnyTraversal[X]
  def Right[X](x: X) = lspace.AnyTraversal(x)

  type EndType[left, right] = Traversal.EndType[Left[left]] | Traversal.EndType[Right[right]]

  def apply[by, left, right](
    by: by,
    left: left,
    right: right
  ): Choose[Choose.By[by], Choose.Left[left], Choose.Right[right]] = new Choose(By(by), Left(left), Right(right))

case class Choose[by, left, right] private (
  by: by,
  left: left,
  right: right
) extends BranchStep

object Coalesce:
  type Traversals[X] = lspace.Traversals[X]
  def Traverals[X](x: X) = lspace.Traversals(x)
  // type EndType[X] <: ClassType[?] = X match {
  //   case l | r => Traversal.EndType[l] | Traversal.EndType[r]
  // }

  type EndType[X] <: ClassType[?] = Traversals[X] match {
    case traversal *: EmptyTuple => Traversal.EndType[traversal]
    case traversal *: traversals => Traversal.EndType[traversal] | EndType[traversals]
  }

  def apply[traversals](traversals: traversals): Coalesce[Coalesce.Traversals[traversals]] = new Coalesce(
    Traverals(traversals)
  )

case class Coalesce[traversals] private (traversals: traversals) extends BranchStep

object Coin:
  import eu.timepit.refined.api._
  import eu.timepit.refined.numeric._

  type Min                                     = 0.0
  type Max                                     = 1.0
  type Probability[d <: Double with Singleton] = d Refined Interval.Closed[Min, Max]

// type Between[X] = X match {
//   // case p if p > Min && p < Max => p
//   case p => p
// }
case class Coin[p <: Double with Singleton](p: Coin.Probability[p]) extends FilterStep

object Constant:
  type Value[X] = X match {
    case String  => X
    case Int     => X
    case Double  => X
    case Long    => X
    case Boolean => X
  }
  def Value[X](x: X): Value[X] = x match {
    case s: String  => s
    case i: Int     => i
    case d: Double  => d
    case l: Long    => l
    case b: Boolean => b
  }

  def apply[T](value: T): Constant[Value[T]] = new Constant(Value(value))

case class Constant[T] private (value: T) extends TraverseStep

case object Count extends BarrierStep

case object Dedup extends GlobalFilterStep

// case class Drop()
case class E(edges: List[Edge[?, ?]] = Nil) extends ResourceStep
case object From                            extends TraverseStep
case class G(graphs: List[Graph] = Nil)     extends GraphStep

object Group:
  type By[X] = lspace.AnyTraversal[X]
  def By[X](x: X) = lspace.AnyTraversal(x)

  type Value[X] = lspace.AnyTraversal[X]
  def Value[X](x: X) = lspace.AnyTraversal(x)

  type EndType[by, value] = TupleType[(Traversal.EndType[By[by]], Traversal.EndType[Value[value]])]

  def apply[by, value](by: by, value: value): Group[By[by], Value[value]] = new Group(By(by), Value(value))

case class Group[by, value] private (by: by, value: value) extends GroupingBarrierStep

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

case class HasId(id: Long)  extends FilterStep
case class HasIri(iri: Iri) extends FilterStep

object HasLabel:
  type EndType[X <: ClassType[?]] <: ClassType[?] = X match {
    case ClassType[?] => X
  }

case class HasLabel[CT <: ClassType[?]](label: CT) extends FilterStep

// case class HasNot() extends FilterStep // use Not(Has)

case class HasValue() extends FilterStep

case class Has[predicate <: P[_]](
  key: Key[?],
  predicate: Option[predicate] = None
) extends FilterStep

case object Head extends ReducingStep
case object Id   extends TraverseStep

// enum InStep[key <: Key] extends MoveStep:
//   case In[key <: Key](key: key)                                                   extends InStep[key]
// sealed trait In[key <: Key]                                                              extends MoveStep
object In:
  def apply[keys](keys: keys): In[MapStep.KeyTuple[keys]] = new In(MapStep.KeyTuple(keys))

case class In[keys] private (keys: keys) extends MoveStep
// case class InKeyPredicate[key <: Key, predicate <: P[_]](key: key, predicate: predicate) extends In[key]

object InE:
  type EndType[InX, OutX] = (InX, OutX) match {
    case (ClassType[in], ClassType[out]) => EdgeType[in, out]
  }
  def apply[keys](keys: keys): InE[MapStep.KeyTuple[keys]] = new InE(MapStep.KeyTuple(keys))

case class InE[keys] private (keys: keys) extends MoveStep

object InEMap:
  def apply[keys](keys: keys): InEMap[MapStep.KeyTuple[keys]] = new InEMap(MapStep.KeyTuple(keys))

case class InEMap[keys] private (keys: keys) extends MapStep

object InMap:
  def apply[keys](keys: keys): InMap[MapStep.KeyTuple[keys]] = new InMap(MapStep.KeyTuple(keys))

case class InMap[keys] private (keys: keys) extends MapStep

case class Is[predicate <: P[_]](predicate: predicate) extends FilterStep

case class Label(label: Set[Key[?]]) extends MoveStep

case object Last extends ReducingStep

object Limit:
  type Max = Int with Singleton
case class Limit[max <: Limit.Max](max: max) extends ClipStep

object Local:
  type Traversal[X] = lspace.AnyTraversal[X]
case class Local[traversal](traversal: Local.Traversal[traversal]) extends BranchStep

case class Max()
case class Mean()
case class Min()

case class N()

object Not:
  type Traversal[X] = lspace.AnyTraversal[X]
case class Not[traversal](traversal: Not.Traversal[traversal]) extends FilterStep

object Or:
  type Traversals[X] = lspace.Traversals[X]
  def Traversals[X](traversals: X): Traversals[X] = lspace.Traversals(traversals)
  type Step[X] = Or[lspace.Traversals[X]]

case class Or[traversals](traversals: Or.Traversals[traversals]) extends FilterStep

case class Order()

object Out:
  def apply[keys](keys: keys): Out[MapStep.KeyTuple[keys]] = new Out(MapStep.KeyTuple(keys))

case class Out[keys] private (keys: keys) extends MoveStep
// case class OutKeyPredicate[key <: Key, predicate <: P[_]](key: key, predicate: predicate) extends Out[key]

object OutE:
  type EndType[X] = X match {
    case ClassType[t] => EdgeType[t, Any]
  }
  def apply[keys](keys: keys): OutE[MapStep.KeyTuple[keys]] = new OutE(MapStep.KeyTuple(keys))

case class OutE[keys] private (keys: keys) extends MoveStep

object OutEMap:
  def apply[keys](keys: keys): OutEMap[MapStep.KeyTuple[keys]] = new OutEMap(MapStep.KeyTuple(keys))

case class OutEMap[keys] private (keys: keys) extends MapStep

object OutMap:
  def apply[keys](keys: keys): OutMap[MapStep.KeyTuple[keys]] = new OutMap(MapStep.KeyTuple(keys))

case class OutMap[keys] private (keys: keys) extends MapStep

case class Path() extends ProjectionStep

case class Project() extends ProjectionStep

case class R() extends ResourceStep

object Range:
  type N = Long with Singleton
case class Range[from <: Range.N, to <: Range.N](from: from, to: to) extends ClipStep

case class Repeat() extends BranchStep

object Select:
  type Label = String with Singleton

case class Select[label <: Select.Label](label: label) extends LabelStep //TODO: tuple of labels

object Skip:
  type N = Long with Singleton
case class Skip[n <: Skip.N](n: n) extends ClipStep

case class Sum() extends ReducingBarrierStep

case class Tail() extends ClipStep

case class TimeLimit() extends EnvironmentStep

case object To extends TraverseStep

object Union:
  type Traversals[X] = lspace.Traversals[X]
  // type EndType[X] <: ClassType[?] = X match {
  //   case l | r => Traversal.EndType[l] | Traversal.EndType[r]
  // }

  type EndType[X] <: ClassType[?] = Traversals[X] match {
    case traversal *: EmptyTuple => Traversal.EndType[traversal]
    case traversal *: traversals => Traversal.EndType[traversal] | EndType[traversals]
  }

case class Union[traversals](traversals: Union.Traversals[traversals]) extends BranchStep

case class V() extends ResourceStep

object Where:
  type Traversal[X] = lspace.AnyTraversal[X]
case class Where[traversal](traversal: Where.Traversal[traversal]) extends FilterStep
