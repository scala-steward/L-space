package lspace
package librarian
package steps

import classtypes._
import logic._
import types._

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
    case Key[name]         => name *: EmptyTuple
  }

  type KeyToTuple[X] <: Tuple = X match {
    case Key[name] => (name, List[Any])
  }

  type KeyToTupleType[X] <: TupleType[?] = X match {
    case Key[name] => TupleType[(name, List[Any])]
  }
  def KeyToTupleType[X](x: X): KeyToTupleType[X] = x match {
    case key: Key[name] =>
      TupleType(StringType.literal(key.name) -> AnyList)
  }

  type KeyValueTuples[X] <: Tuple = X match {
    // case Tuple => Tuple.Map[X, KeyToTuple]
    case EmptyTuple  => EmptyTuple
    case key *: keys => KeyToTupleType[key] *: KeyValueTuples[keys]
    // case Key[name]         => (name, List[Any]) *: EmptyTuple
  }

  def KeyValueTuples[X](x: X): KeyValueTuples[X] =
    (x match {
      case EmptyTuple => EmptyTuple
      case key *: keys =>
        KeyToTupleType(key) *: KeyValueTuples(keys)
    }).asInstanceOf[KeyValueTuples[X]]

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

  type EndType[X] = TupleType[Tuple.Map[KeyTuple[X], KeyToTuple]]
  def EndType[X](x: X): EndType[X] = TupleType(KeyValueTuples(KeyTuple(x))).asInstanceOf[EndType[X]]

sealed trait MapStep extends ProjectionStep with GroupingStep

sealed trait ResourceStep extends Step
sealed trait GraphStep    extends ResourceStep
sealed trait BranchStep   extends Step
object MoveStep:
  import Key._

  type KeyNameTuple[X] <: Tuple = KeyTuple[X] match {
    case EmptyTuple        => EmptyTuple
    case Key[name] *: keys => name *: KeyNameTuple[keys]
    case Key[name]         => name *: EmptyTuple
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

case class As[label <: As.Label, CT](label: label, classType: ClassType[CT]) extends LabelStep

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
  def EndType[X](x: X): EndType[X] = UnionType(Traversal.EndTypes(x)) // .asInstanceOf[EndType[X]]

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

  def apply[p <: Probability.P](p: p)(using Conversion[p, Probability]): Coin[p] = new Coin(p)

case class Coin[p] private (p: Probability) extends FilterStep

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

case class Count() extends BarrierStep

case class Dedup() extends GlobalFilterStep

// case class Drop()
case class E(edges: List[Edge[?, ?]] = Nil) extends ResourceStep

object From:
  type EndType[X] <: ClassType[?] = X match
    case ClassType[Edge[in, out]] => ClassType[in]
  def EndType[X](x: X): EndType[X] = x match {
    case ct: EdgeType[?, ?] => ct.in.asInstanceOf[EndType[X]]
  }
case class From() extends TraverseStep

case class G(graphs: List[Graph] = Nil) extends GraphStep

object Group:
  type By[X] = lspace.AnyTraversal[X]
  def By[X](x: X) = lspace.AnyTraversal(x)

  type Value[X] = lspace.AnyTraversal[X]
  def Value[X](x: X) = lspace.AnyTraversal(x)

  type EndType[by, value] <: ClassType[?] = Traversal.EndType[Value[value]] match {
    case ClassType[t] => TupleType[(Traversal.EndType[By[by]], ListType[List[t]])]
  }
  def EndType[by, value](by: by, value: value): EndType[by, value] =
    (Traversal.EndType(Value(value)) match {
      case ct: ClassType[t] =>
        TupleType((Traversal.EndType(By(by)), ListType(ct)))
    }).asInstanceOf[EndType[by, value]]

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

// object HasLabel:

case class HasLabel[t](label: ClassType[t]) extends FilterStep

// case class HasNot() extends FilterStep // use Not(Has)

case class HasValue() extends FilterStep

case class Has[name <: Key.Name, predicate <: P[_]](
  key: Key[name],
  predicate: Option[predicate] = None
) extends FilterStep

case class Head() extends ReducingStep
case class Id()   extends TraverseStep

// enum InStep[key <: Key] extends MoveStep:
//   case In[key <: Key](key: key)                                                   extends InStep[key]
// sealed trait In[key <: Key]                                                              extends MoveStep
object In:
  def apply[keys](keys: keys): In[MoveStep.KeyNameTuple[keys]] = new In(MoveStep.KeyTuple(keys))

case class In[keys] private (keys: MoveStep.KeyTuple[?]) extends MoveStep
// case class InKeyPredicate[key <: Key, predicate <: P[_]](key: key, predicate: predicate) extends In[key]

object InE:
  type EndType[InX, OutX] = MoveEStep.EndType[InX, OutX]
  def apply[keys](keys: keys): InE[MoveStep.KeyNameTuple[keys]] = new InE(MoveStep.KeyTuple(keys))

case class InE[keys] private (keys: MoveStep.KeyTuple[?]) extends MoveStep

object InEMap:
  def apply[keys](keys: keys): InEMap[MapStep.KeyNameTuple[keys]] = new InEMap(MapStep.KeyTuple(keys))

case class InEMap[keys] private (keys: MapStep.KeyTuple[?]) extends MapStep

object InMap:
  def apply[keys](keys: keys): InMap[MapStep.KeyNameTuple[keys]] = new InMap(MapStep.KeyTuple(keys))

case class InMap[keys] private (keys: MapStep.KeyTuple[?]) extends MapStep

case class Is[predicate <: P[_]](predicate: predicate) extends FilterStep

// object Label:
//   type EndType[X] <: ClassType[?] = X match {
//     case
//   }
case class Label() extends MoveStep

// object Last:
case class Last() extends ReducingStep

object Limit:

  def apply[max <: Singleton](max: max)(using Conversion[max, Positive[Int]]): Limit[max] = new Limit(max)

case class Limit[max] private (max: Positive[Int]) extends ClipStep

object Local:
  type EndType[X] = Traversal.EndType[X]
  def EndType[X](x: X): EndType[X] = Traversal.EndType[X](x)

  type TraversalType[X] = lspace.AnyTraversal[X]

  def apply[traversal](traversal: traversal): Local[TraversalType[traversal]] = new Local(
    lspace.AnyTraversal(traversal)
  )

case class Local[traversal] private (traversal: traversal) extends BranchStep

object Max:
  type Able[X] <: Traversal[?, ?, ?] = X match {
    case Traversal[s, ClassType[e], steps] => Traversal[s, OrderP.OrderableClassType[e], steps]
  }
  def Able[X](x: X): Able[X] = x match {
    case traversal: Traversal[s, e, steps] =>
      traversal.withEndType(OrderP.OrderableClassType(traversal.et)).asInstanceOf[Able[X]]
  }

  def apply[traversal](
    traversal: traversal
  ): Max[Able[traversal]] =
    new Max(Able(traversal))

case class Max[traversal <: Traversal[?, ?, ?]] private (traversal: traversal)
    extends FilterBarrierStep
    with ReducingStep

case class Mean()   extends ReducingBarrierStep
case class Median() extends ReducingBarrierStep

object Min:
  type Able[X] <: Traversal[?, ?, ?] = X match {
    case Traversal[s, ClassType[e], steps] => Traversal[s, OrderP.OrderableClassType[e], steps]
  }
  def Able[X](x: X): Able[X] = x match {
    case traversal: Traversal[s, e, steps] =>
      traversal.withEndType(OrderP.OrderableClassType(traversal.et)).asInstanceOf[Able[X]]
  }

  def apply[traversal](
    traversal: traversal
  ): Min[Able[traversal]] =
    new Min(Able(traversal))

case class Min[traversal <: Traversal[?, ?, ?]] private (traversal: traversal)
    extends FilterBarrierStep
    with ReducingStep

case class N() extends ResourceStep

object Not:
  def apply[traversal](traversal: traversal): Not[AnyTraversal[traversal]] = new Not(
    AnyTraversal(traversal)
  )

case class Not[traversal] private (traversal: traversal) extends FilterStep

object Or:
  type Traversals[X] = lspace.Traversals[X]
  def Traversals[X](traversals: X): Traversals[X] = lspace.Traversals(traversals)

  def apply[traversals](traversals: traversals): Or[Traversals[traversals]] = new Or(Traversals(traversals))

case class Or[traversals] private (traversals: traversals) extends FilterStep

case class Order()

object Out:
  def apply[keys](keys: keys): Out[MoveStep.KeyNameTuple[keys]] =
    new Out(MoveStep.KeyTuple(keys))

case class Out[keys] private (keys: MoveStep.KeyTuple[?]) extends MoveStep
// case class OutKeyPredicate[key <: Key, predicate <: P[_]](key: key, predicate: predicate) extends Out[key]

object OutMap:
  def apply[keys](keys: keys): OutMap[MapStep.KeyNameTuple[keys]] = new OutMap(MapStep.KeyTuple(keys))

case class OutMap[keys] private (keys: MapStep.KeyTuple[?]) extends MapStep

object OutE:
  type EndType[InX, OutX] = MoveEStep.EndType[InX, OutX]
  def EndType[InX, OutX](inX: InX, outX: OutX): EndType[InX, OutX] = MoveEStep.EndType(inX, outX)
  def apply[keys](keys: keys): OutE[MoveStep.KeyNameTuple[keys]]   = new OutE(MoveStep.KeyTuple(keys))

case class OutE[keys] private (keys: MoveStep.KeyTuple[?]) extends MoveStep

object OutEMap:
  def apply[keys](keys: keys): OutEMap[MapStep.KeyNameTuple[keys]] = new OutEMap(MapStep.KeyTuple(keys))

case class OutEMap[keys] private (keys: MapStep.KeyTuple[?]) extends MapStep

object Path:
  // type EndType[X] = Traversal.EndType[X]
  // def EndType[X](x: X): EndType[X] = Traversal.EndType[X](x)

  type TraversalType[X] = lspace.AnyTraversal[X]

  def apply[traversal](traversal: traversal): Path[TraversalType[traversal]] = new Path(
    lspace.AnyTraversal(traversal)
  )

case class Path[traversal] private (traversal: traversal) extends ProjectionStep

case class Project() extends ProjectionStep

case class R() extends ResourceStep

object Repeat:
  type UntilTraversal[X] <: Option[? <: Traversal[?, ?, ?]] = X match {
    case Some[t]   => Some[AnyTraversal[t]]
    case None.type => None.type
    case _         => Some[AnyTraversal[X]]
  }
  def UntilTraversal[X](x: X): UntilTraversal[X] =
    (x match {
      case someT @ Some(t: Traversal[?, ?, ?]) => someT
      case None                                => None
      case t: Traversal[?, ?, ?]               => Some(t)
    }).asInstanceOf[UntilTraversal[X]]

  type MaxType[X] <: Option[? <: Int] = X match
    case Int       => Some[X & Int]
    case None.type => None.type
  def MaxType[X](x: X): MaxType[X] =
    (x match {
      case x: Int  => Some(x)
      case None => None
    }).asInstanceOf[MaxType[X]]

  type ValidUntil[traversal, until] <: =:=[?, ?] = until match {
    case None.type => None.type =:= None.type
    case _         => CTtoT[Traversal.EndType[traversal]] =:= CTtoT[Traversal.StartType[until]]
  }

  def apply[traversal, until, max <: (Int with Singleton | Option[Nothing]), noloop <: Boolean](
    traversal: traversal,
    until: until = None,
    max: max = None,
    collect: Boolean = false,
    noloop: noloop = false
  )(using
    Conversion[MaxType[max], Option[Positive[Int]]]
  ): Repeat[AnyTraversal[traversal], UntilTraversal[until], max, noloop] =
    new Repeat[AnyTraversal[traversal], UntilTraversal[until], max, noloop](
      AnyTraversal(traversal),
      UntilTraversal(until),
      MaxType(max),
      collect,
      noloop
    )

case class Repeat[
  traversal <: Traversal[?, ?, ?],
  until <: Option[? <: Traversal[?, ?, ?]],
  max,
  noloop <: Boolean
] private (
  traversal: traversal,
  until: until,
  max: Option[Positive[Int]],
  collect: Boolean,
  noloop: noloop
) extends BranchStep

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
    // case EmptyTuple => EmptyTuple
    case As[label, ct] *: steps => ClassType[ct]
    case step *: steps          => SelectLabel[steps, label]
  }

  def SelectLabel[Steps <: Tuple, label](steps: Steps, label: label): SelectLabel[Steps, label] =
    (steps match {
      case EmptyTuple                                   =>
      case (as: As[?, ?]) *: steps if as.label == label => as.classType
      case step *: steps                                => SelectLabel(steps, label)
    }).asInstanceOf[SelectLabel[Steps, label]]

  type SelectLabels[Steps <: Tuple, Labels] <: Tuple = (Steps, Labels) match {
    // case (steps, labels) => Tuple.Fold[labels, EmptyTuple, SelectLabel]
    case (steps, EmptyTuple)      => EmptyTuple
    case (steps, label *: labels) => SelectLabel[steps, label] *: SelectLabels[steps, labels]
  }

  def SelectLabels[Steps <: Tuple, Labels <: Tuple](steps: Steps, labels: Labels): SelectLabels[Steps, Labels] =
    ((steps, labels) match {
      case (steps, EmptyTuple)      => EmptyTuple
      case (steps, label *: labels) => SelectLabel(steps, label) *: SelectLabels(steps, labels)
    }).asInstanceOf[SelectLabels[Steps, Labels]]

  type EndType[Steps <: Tuple, Labels] =
    TupleType[TupleType.Types[SelectLabels[TupleReverse[Steps], Able[Labels]]]]

  def EndType[Steps <: Tuple, Labels](steps: Steps, labels: Labels): EndType[Steps, Labels] =
    TupleType(SelectLabels(TupleReverse(steps), Able(labels)))

  def apply[labels](labels: labels): Select[Able[labels]] = new Select(Able(labels))

case class Select[labels](labels: labels) extends LabelStep

object Skip:
  type N = Long with Singleton
case class Skip[n <: Skip.N](n: n) extends ClipStep

object Sum:
  import squants.Quantity

  type Able[X] = X match {
    case Int         => X
    case Double      => X
    case Long        => X
    case Quantity[t] => X
  }

case class Sum() extends ReducingBarrierStep

case class Tail() extends ClipStep

case class TimeLimit() extends EnvironmentStep

object To:
  type EndType[X] <: ClassType[?] = X match
    case ClassType[Edge[in, out]] => ClassType[out]
  def EndType[X](x: X): EndType[X] = x match {
    case ct: EdgeType[?, ?] => ct.out.asInstanceOf[EndType[X]]
  }
case class To() extends TraverseStep

object Union:
  type Traversals[X] = lspace.Traversals[X]
  def Traverals[X](x: X) = lspace.Traversals(x)

  type EndType[X] = UnionType[UnionType.Able[Traversal.EndTypes[X]]]
  def EndType[X](x: X): EndType[X] = UnionType(Traversal.EndTypes(x))

  def apply[traversals](traversals: traversals): Union[Union.Traversals[traversals]] = new Union(
    Traverals(traversals)
  )

case class Union[traversals] private (traversals: traversals) extends BranchStep

// object V:
// type ValueTypes[X] = AnyValue//DataType.Types[X]
// def ValueTypes[X](x: ClassType[X]): ValueTypes[X] = AnyValue
//   x match {
//   case ct: UnionType[t] if ct.types.filterNot {
//     case _: NodeType => false
//     case _: EdgeType[?, ?] => false
//     case _: Ontology => false
//     case _: Property => false
//     case _ => true
//   }
// }
case class V() extends ResourceStep

object Where:
  def apply[traversal](traversal: traversal): Where[AnyTraversal[traversal]] = new Where(
    AnyTraversal(traversal)
  )

case class Where[traversal] private (traversal: traversal) extends FilterStep
