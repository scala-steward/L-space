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

  type KeyNameTuple[X] <: Tuple = KeyTuple[X] match {
    case EmptyTuple        => EmptyTuple
    case Key[name] *: keys => name *: KeyNameTuple[keys]
    case Key[name] => name *: EmptyTuple
  }

  type KeyTuple[X] <: Tuple = X match {
    case EmptyTuple  => EmptyTuple
    case key *: keys => KeyLike[key] *: KeyTuple[keys]
    case _           => KeyLike[X] *: EmptyTuple
  }
  def KeyTuple[X](x: X): KeyTuple[X] = (x match {
    case EmptyTuple  => EmptyTuple
    case key *: keys => (KeyLike(key) *: KeyTuple(keys))
    case _           => (KeyLike(x) *: EmptyTuple)
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
sealed trait MoveStep extends BranchStep
object MoveEStep:
  type EndType[InX, OutX] <: ClassType[?] =
    (InX, OutX) match {
      case (ClassType[in], ClassType[out]) => EdgeType[in, out]
    }
  def EndType[InX, OutX](inX: InX, outX: OutX): EndType[InX, OutX] =
    (inX, outX) match {
      case (inX: ClassType[in], outX: ClassType[out]) => EdgeType(inX, outX).asInstanceOf[EndType[InX, OutX]]
    }

sealed trait MoveEStep    extends BranchStep
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

case class As[label <: As.Label, CT <: ClassType[?]](label: label, classType: CT) extends LabelStep

object Choose:
  type By[X] = lspace.AnyTraversal[X]
  def By[X](x: X) = lspace.AnyTraversal(x)

  type Left[X] = lspace.AnyTraversal[X]
  def Left[X](x: X) = lspace.AnyTraversal(x)

  type Right[X] = lspace.AnyTraversal[X]
  def Right[X](x: X) = lspace.AnyTraversal(x)

  type EndType[left, right] =
    UnionType[UnionType.Able[(Traversal.EndType[Left[left]], Traversal.EndType[Right[right]])]]
  def EndType[left, right](left: left, right: right): EndType[left, right] =
    UnionType(
      (Traversal.EndType(Left(left)), Traversal.EndType(Right(right)))
    )

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

  type EndType[X] = UnionType[UnionType.Able[Traversal.EndTypes[X]]]
  def EndType[X](x: X): EndType[X] = UnionType(Traversal.EndTypes(x))//.asInstanceOf[EndType[X]]

  //   {
  //   def buildTypes[X](x: X): Set[? <: ClassType[?]] = x match {
  //     case (traversal: Traversal[?, ?, ?]) *: EmptyTuple => Set(Traversal.EndType(traversal))
  //     case (traversal: Traversal[?, ?, ?]) *: traversals => Set(Traversal.EndType(traversal)) ++ buildTypes(traversals)
  //   }
  //   UnionType[EndType[X]]
  // }

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

  type EndType[by, value] = Traversal.EndType[Value[value]] match {
    case ClassType[t] => TupleType[(Traversal.EndType[By[by]], ListType[List[t]])]
  }
  def EndType[by, value](by: by, value: value): EndType[by, value] =
    TupleType((Traversal.EndType(By(by)), ListType(Traversal.EndType(Value(value))))).asInstanceOf[EndType[by, value]]

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
    case IntType[t] => IntType[t]
    case LongType[t] => LongType[t]
    case StringType[t] => StringType[t]
    case ClassType[?] => X
  }
  def EndType[X <: ClassType[?]](x: X): EndType[X] = x match {
    case ct: IntType[t] => ct
    case ct: LongType[t] => ct
    case ct: StringType[t] => ct
    case ct: ClassType[t] => ct
  }

case class HasLabel[CT <: ClassType[?]](label: CT) extends FilterStep

// case class HasNot() extends FilterStep // use Not(Has)

case class HasValue() extends FilterStep

case class Has[name <: Key.Name, predicate <: P[_]](
  key: Key[name],
  predicate: Option[predicate] = None
) extends FilterStep

case object Head extends ReducingStep
case object Id   extends TraverseStep

// enum InStep[key <: Key] extends MoveStep:
//   case In[key <: Key](key: key)                                                   extends InStep[key]
// sealed trait In[key <: Key]                                                              extends MoveStep
object In:
  def apply[keys](keys: keys): In[MapStep.KeyNameTuple[keys]] = new In(MapStep.KeyTuple(keys))

case class In[keys] private (keys: MapStep.KeyTuple[?]) extends MoveStep
// case class InKeyPredicate[key <: Key, predicate <: P[_]](key: key, predicate: predicate) extends In[key]

object InE:
  type EndType[InX, OutX] = MoveEStep.EndType[InX, OutX]
  def apply[keys](keys: keys): InE[MapStep.KeyNameTuple[keys]] = new InE(MapStep.KeyTuple(keys))

case class InE[keys] private (keys: MapStep.KeyTuple[?]) extends MoveStep

object InEMap:
  def apply[keys](keys: keys): InEMap[MapStep.KeyNameTuple[keys]] = new InEMap(MapStep.KeyTuple(keys))

case class InEMap[keys] private (keys: MapStep.KeyTuple[?]) extends MapStep

object InMap:
  def apply[keys](keys: keys): InMap[MapStep.KeyNameTuple[keys]] = new InMap(MapStep.KeyTuple(keys))

case class InMap[keys] private (keys: MapStep.KeyTuple[?]) extends MapStep

case class Is[predicate <: P[_]](predicate: predicate) extends FilterStep

case class Label(label: Set[Key[?]]) extends MoveStep

// object Last:
case class Last() extends ReducingStep

object Limit:
  type Max = Int with Singleton
case class Limit[max <: Limit.Max](max: max) extends ClipStep

object Local:
  type EndType[X] = Traversal.EndType[X]
  def EndType[X](x: X): EndType[X] = Traversal.EndType[X](x)

  type TraversalType[X] = lspace.AnyTraversal[X]

  def apply[traversal](traversal: traversal): Local[TraversalType[traversal]] = new Local(
    lspace.AnyTraversal(traversal)
  )

case class Local[traversal] private (traversal: traversal) extends BranchStep

object Max:
  type StartType[X] = Traversal.StartType[X]
  def StartType[X](x: X): StartType[X] = Traversal.StartType(x)
  type EndType[X] = Traversal.EndType[X]
  def EndType[X](x: X): EndType[X] = Traversal.EndType(x)

  type Able[X] <: Traversal[?, ?, ?] = X match {
    case Traversal[s, e, steps] => Traversal[s, OrderP.OrderableClassType[e], steps]
  }
  def Able[X](x: X): Able[X] = x match {
    case traversal: Traversal[s, e, steps] =>
      traversal.withEndType(OrderP.OrderableClassType(traversal.et))
  }

  def apply[traversal](
    traversal: traversal
  ): Max[Able[traversal]] =
    new Max(Able(traversal))

case class Max[traversal <: Traversal[?, ?, ?]] private (traversal: traversal) extends BranchStep

case class Mean()
case class Min[traversal] private (traversal: traversal) extends BranchStep

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
  def apply[keys](keys: keys): Out[MapStep.KeyNameTuple[keys]] =
    new Out(MapStep.KeyTuple(keys))

case class Out[keys] private (keys: MapStep.KeyTuple[?]) extends MoveStep
// case class OutKeyPredicate[key <: Key, predicate <: P[_]](key: key, predicate: predicate) extends Out[key]

object OutE:
  type EndType[InX, OutX] = MoveEStep.EndType[InX, OutX]
  def EndType[InX, OutX](inX: InX, outX: OutX): EndType[InX, OutX] = MoveEStep.EndType(inX, outX)
  def apply[keys](keys: keys): OutE[MapStep.KeyNameTuple[keys]]    = new OutE(MapStep.KeyTuple(keys))

case class OutE[keys] private (keys: MapStep.KeyTuple[?]) extends MoveStep

object OutEMap:
  def apply[keys](keys: keys): OutEMap[MapStep.KeyNameTuple[keys]] = new OutEMap(MapStep.KeyTuple(keys))

case class OutEMap[keys] private (keys: MapStep.KeyTuple[?]) extends MapStep

object OutMap:
  def apply[keys](keys: keys): OutMap[MapStep.KeyNameTuple[keys]] = new OutMap(MapStep.KeyTuple(keys))

case class OutMap[keys] private (keys: MapStep.KeyTuple[?]) extends MapStep

case class Path() extends ProjectionStep

case class Project() extends ProjectionStep

case class R() extends ResourceStep

object Range:
  type N = Long with Singleton
case class Range[from <: Range.N, to <: Range.N](from: from, to: to) extends ClipStep

case class Repeat() extends BranchStep

object Select:
  type Label = String with Singleton
  type LabelLike[X] = X match {
    case String with Singleton => X
  }

  type Able[X] <: Tuple = X match {
    case label *: EmptyTuple => LabelLike[label] *: EmptyTuple
    case label *: labels     => LabelLike[label] *: Able[labels]
    case _                   => LabelLike[X] *: EmptyTuple
  }

  def Able[X](x: X): Able[X] = x match {
    case _ => ???
  }

  type TupleReverse[X <: Tuple] <: Tuple = X match {
    case EmptyTuple   => EmptyTuple
    case head *: tail => Tuple.Concat[TupleReverse[tail], head *: EmptyTuple]
  }
  def TupleReverse[X <: Tuple](x: X): TupleReverse[X] = (x match {
    case EmptyTuple   => EmptyTuple
    case head *: tail => TupleReverse(tail) ++ (head *: EmptyTuple)
  }).asInstanceOf[TupleReverse[X]]

  type SelectLabel[Steps <: Tuple, label] <: ClassType[?] = Steps match {
    case As[label, ct] *: steps => ct
    case step *: steps          => SelectLabel[steps, label]
  }

  def SelectLabel[Steps <: Tuple, label](steps: Steps, label: label): SelectLabel[Steps, label] =
    (steps match {
      case (as: As[?, ?]) *: steps if as.label == label => as.classType
      case step *: steps                                => SelectLabel(steps, label)
    }).asInstanceOf[SelectLabel[Steps, label]]

  type SelectLabels[Steps <: Tuple, Labels] <: Tuple = (Steps, Labels) match {
    // case (steps, labels) => Tuple.Fold[labels, EmptyTuple, SelectLabel]
    case (steps, EmptyTuple)      => EmptyTuple
    case (steps, label *: labels) => SelectLabel[steps, label] *: SelectLabels[steps, labels]
  }

  def SelectLabels[Steps <: Tuple, Labels](steps: Steps, labels: Labels): SelectLabels[Steps, Labels] =
    ((steps, labels) match {
      case (steps, label *: labels) => SelectLabel(steps, label) *: SelectLabels(steps, labels)
    }).asInstanceOf[SelectLabels[Steps, Labels]]

  type EndType[Steps <: Tuple, Labels] =
    TupleType[TupleType.ClassTypesToTypes[SelectLabels[TupleReverse[Steps], Able[Labels]]]]

  def EndType[Steps <: Tuple, Labels](steps: Steps, labels: Labels): EndType[Steps, Labels] =
    TupleType(SelectLabels(TupleReverse(steps), Able(labels)))

  def apply[labels](labels: labels): Select[Able[labels]] = new Select(Able(labels))

case class Select[labels](labels: labels) extends LabelStep

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
