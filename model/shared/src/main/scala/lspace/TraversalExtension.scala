package lspace

import librarian.step._
import scala.quoted.ToExpr.EmptyTupleToExpr

extension [ST, ET, Steps <: Tuple](_traversal: Traversal[ST, ET, Steps])

  def and[traversals](
    traversals: traversals
  ): Traversal[ST, ET, Traversal.StepsConcat[Steps, And[And.Traversals[traversals]]]] =
    _traversal.addStep(And(traversals))

  def as[label <: As.Label](label: label): Traversal[ST, ET, Traversal.StepsConcat[Steps, As[label, ET]]] =
    _traversal.addStep(As(label, _traversal.et))

  def choose[by, left, right](
    by: by,
    left: left,
    right: right
  ): Traversal[ST, CTtoT[Choose.EndType[left, right]], Traversal.StepsConcat[
    Steps,
    Choose[Choose.By[by], Choose.Left[left], Choose.Right[right]]
  ]] =
    _traversal.addStep(Choose(by, left, right), Choose.EndType(left, right))

  def coalesce[traversals](
    traversals: traversals
  )(using
    ET =:= Tuple.Fold[Tuple.Map[Tuple.Map[Coalesce.Traversals[traversals], Traversal.StartType], CTtoT], Nothing, |]
  ): Traversal[ST, CTtoT[Coalesce.EndType[traversals]], Traversal.StepsConcat[
    Steps,
    Coalesce[Coalesce.Traversals[traversals]]
  ]] = _traversal.addStep(Coalesce(traversals), Coalesce.EndType(traversals))

  def coin[p <: Double with Singleton](
    p: Coin.Probability[p]
  ): Traversal[ST, ET, Traversal.StepsConcat[Steps, Coin[p]]] =
    _traversal.addStep(Coin(p))

  def constant[V](
    v: V
  ): Traversal[ST, CTtoT[ClassType.Able[V]], Traversal.StepsConcat[Steps, Constant[Constant.Value[V]]]] =
    _traversal.addStep(Constant(v), ClassType.Able(v))

  def count: Traversal[ST, Long, Traversal.StepsConcat[Steps, Count]] =
    _traversal.addStep(Count(), LongType.long)

  def dedup: Traversal[ST, ET, Traversal.StepsConcat[Steps, Dedup]] =
    _traversal.addStep(Dedup())

  def e(): Traversal[ST, Edge[Any, Any], Traversal.StepsConcat[
    Steps,
    E
  ]] = _traversal.addStep(E(), AnyEdge)

  def group[by, value](
    by: by,
    value: value
  )(using
    ET =:= CTtoT[Traversal.EndType[Group.By[by]]],
    ET =:= CTtoT[Traversal.EndType[Group.Value[value]]]
  ): Traversal[ST, CTtoT[Group.EndType[by, value]], Traversal.StepsConcat[
    Steps,
    Group[Group.By[by], Group.Value[value]]
  ]] =
    _traversal.addStep(
      Group(by, value),
      Group.EndType(by, value)
    )

  def has[name <: Key.Name](label: Key[name]): Traversal[ST, ET, Traversal.StepsConcat[Steps, Has[name, Nothing]]] =
    _traversal.addStep(Has(label))
  def has[name <: Key.Name, predicate <: P[_]](
    label: Key[name],
    predicate: predicate
  ): Traversal[ST, ET, Traversal.StepsConcat[Steps, Has[name, predicate]]] =
    _traversal.addStep(Has(label, Some(predicate)))
  // (
  //   traversal.st,
  //   traversal.et
  // )

  def hasId(id: Long): Traversal[ST, ET, Traversal.StepsConcat[Steps, HasId]] =
    _traversal.addStep(HasId(id))

  def hasIri(iri: Iri): Traversal[ST, ET, Traversal.StepsConcat[Steps, HasIri]] =
    _traversal.addStep(HasIri(iri))

  def hasLabel[t](
    label: ClassType[t]
  ): Traversal[ST, t, Traversal.StepsConcat[Steps, HasLabel[t]]] =
    _traversal.addStep(HasLabel(label), label)

  // def hasNot[]()
  // def hasValue

  def head: Traversal[ST, ET, Traversal.StepsConcat[Steps, Head]] =
    _traversal.addStep(Head())

  def id: Traversal[ST, Long, Traversal.StepsConcat[Steps, Id]] =
    _traversal.addStep(Id(), LongType.long)

  def in[keys](
    keys: keys
  ): Traversal[ST, Any, Traversal.StepsConcat[Steps, In[MoveStep.KeyNameTuple[keys]]]] =
    _traversal.addStep(In(keys), AnyResource)

  def inMap[keys](
    keys: keys
  ): Traversal[ST, CTtoT[MapStep.EndType[keys]], Traversal.StepsConcat[Steps, InMap[MapStep.KeyNameTuple[keys]]]] =
    _traversal.addStep(InMap(keys), MapStep.EndType(keys))

  def inE[keys](
    keys: keys
  ): Traversal[ST, Edge[Any, ET], Traversal.StepsConcat[Steps, InE[
    MoveStep.KeyNameTuple[keys]
  ]]] =
    _traversal.addStep(InE(keys), OutE.EndType(AnyResource, _traversal.et))

  def is[predicate <: P[_]](
    predicate: predicate
  ): Traversal[ST, ET, Traversal.StepsConcat[Steps, Is[predicate]]] =
    _traversal.addStep(Is[predicate](predicate))

  // def label: Traversal[ST, Label.EndType[ET], Traversal.StepsConcat[Steps, Label]] =
  //   _traversal.addStep(Label())

  def last: Traversal[ST, ET, Traversal.StepsConcat[Steps, Last]] =
    _traversal.addStep(Last())

  def limit[max <: Limit.Max](max: max): Traversal[ST, ET, Traversal.StepsConcat[Steps, Limit[max]]] =
    _traversal.addStep(Limit(max))

  def local[traversal](
    traversal: traversal
  )(using
    ET =:= CTtoT[Traversal.StartType[traversal]]
  ): Traversal[ST, CTtoT[Local.EndType[traversal]], Traversal.StepsConcat[
    Steps,
    Local[Local.TraversalType[traversal]]
  ]] = _traversal.addStep(Local(traversal), Local.EndType(traversal))

  def max[traversal](
    traversal: traversal
  )(using
    ET =:= Traversal.StartType[traversal]
  ): Traversal[ST, CTtoT[Traversal.EndType[traversal]], Traversal.StepsConcat[
    Steps,
    Max[Max.Able[traversal]]
  ]] =
    _traversal.addStep(Max(traversal), Traversal.EndType(traversal))

  def min[traversal](
    traversal: traversal
  )(using
    ET =:= Traversal.StartType[traversal]
  ): Traversal[ST, CTtoT[Traversal.EndType[traversal]], Traversal.StepsConcat[
    Steps,
    Min[Min.Able[traversal]]
  ]] =
    _traversal.addStep(Min(traversal), Traversal.EndType(traversal))

  def n(): Traversal[ST, Node, Traversal.StepsConcat[
    Steps,
    N
  ]] = _traversal.addStep(N(), NodeType)

  // def out[key <: Key.Name](key: key): Traversal[ST, ResourceType, Traversal.StepsConcat[Steps, Out[MapStep.KeyNameTuple[key *: EmptyTuple]]]] =
  //     _traversal.addStep(Out(key), ResourceType)

  def not[traversal](
    traversal: traversal
  )(using
    ET =:= CTtoT[Traversal.StartType[traversal]]
  ): Traversal[ST, ET, Traversal.StepsConcat[
    Steps,
    Not[AnyTraversal[traversal]]
  ]] = _traversal.addStep(Not(traversal))

  def or[traversals](
    traversals: traversals
  ): Traversal[ST, ET, Traversal.StepsConcat[Steps, Or[And.Traversals[traversals]]]] =
    _traversal.addStep(Or(traversals))

  def out[keys](
    keys: keys
  ): Traversal[ST, Any, Traversal.StepsConcat[Steps, Out[MoveStep.KeyNameTuple[keys]]]] =
    _traversal.addStep(Out(keys), AnyResource)

  def outMap[keys](
    keys: keys
  ): Traversal[ST, CTtoT[MapStep.EndType[keys]], Traversal.StepsConcat[Steps, OutMap[MapStep.KeyNameTuple[keys]]]] =
    _traversal.addStep(OutMap(keys), MapStep.EndType(keys))

  def outE[keys](
    keys: keys
  ): Traversal[ST, Edge[ET, Any], Traversal.StepsConcat[Steps, OutE[
    MoveStep.KeyNameTuple[keys]
  ]]] =
    _traversal.addStep(OutE(keys), OutE.EndType(_traversal.et, AnyResource))

// def out[key <: Key, predicate <: P[_]](
//   key: key,
//   predicate: predicate
// ): Traversal[ST, ResourceType[Any], Traversal.StepsConcat[Steps, OutKeyPredicate[key, predicate]]] =
//   _traversal.addStep(OutKeyPredicate[key, predicate](key, predicate))
// (
//   traversal.st,
//   ResourceType[Any]()
// )

  def r(): Traversal[ST, Any, Traversal.StepsConcat[
    Steps,
    R
  ]] = _traversal.addStep(R(), AnyResource)

  def repeat[traversal, until, max, noloop <: Boolean](
    traversal: traversal,
    until: until = None,
    max: max = None,
    collect: Boolean = false,
    noloop: noloop = false
  )(using
    ET =:= CTtoT[Traversal.StartType[traversal]],
    Repeat.ValidUntil[traversal, until]
  ): Traversal[ST, CTtoT[Traversal.EndType[traversal]], Traversal.StepsConcat[
    Steps,
    Repeat[AnyTraversal[traversal], Repeat.UntilTraversal[until], Repeat.MaxType[max], noloop]
  ]] =
    _traversal.addStep(Repeat(traversal, until, max, collect, noloop), Traversal.EndType(traversal))
  // (traversal: Traversal[_ <: ClassType[_], E0, _ <: HList],
  // until: Option[Traversal[E0, _ <: ClassType[_], _ <: HList]],
  // max: Option[Int] = None,
  // collect: Boolean = false,
  // noloop: Boolean = false)

  def select[labels](
    labels: labels
  ): Traversal[ST, CTtoT[Select.EndType[Steps, labels]], Traversal.StepsConcat[Steps, Select[Select.Able[labels]]]] =
    _traversal.addStep(Select(labels), Select.EndType(_traversal.steps, labels))

  def skip[n <: Skip.N](n: n): Traversal[ST, ET, Traversal.StepsConcat[Steps, Skip[n]]] =
    _traversal.addStep(Skip(n))

  def union[traversals](
    traversals: traversals
  )(using
    ET =:= Tuple.Fold[Tuple.Map[Tuple.Map[Union.Traversals[traversals], Traversal.StartType], CTtoT], Nothing, |]
  ): Traversal[ST, CTtoT[Union.EndType[traversals]], Traversal.StepsConcat[
    Steps,
    Union[Union.Traversals[traversals]]
  ]] = _traversal.addStep(Union(traversals), Union.EndType(traversals))

  def v(): Traversal[ST, Any, Traversal.StepsConcat[
    Steps,
    V
  ]] = _traversal.addStep(V(), AnyValue)

  def where[traversal](
    traversal: traversal
  )(using
    ET =:= CTtoT[Traversal.StartType[traversal]]
  ): Traversal[ST, ET, Traversal.StepsConcat[
    Steps,
    Where[AnyTraversal[traversal]]
  ]] = _traversal.addStep(Where(traversal))

extension [ST, IN, OUT, ET <: Edge[IN, OUT], Steps <: Tuple](_traversal: Traversal[ST, ET, Steps])

  def from(): Traversal[ST, IN, Traversal.StepsConcat[Steps, From]] =
    _traversal.addStep(From(), From.EndType(_traversal.et))

  def to(): Traversal[ST, OUT, Traversal.StepsConcat[Steps, To]] =
    _traversal.addStep(To(), To.EndType(_traversal.et))

extension [ST, ET, Steps <: Tuple](_traversal: Traversal[ST, ET, Steps])(using ET =:= Sum.Able[ET])
  def sum(): Traversal[ST, ET, Traversal.StepsConcat[Steps, Sum]] =
    _traversal.addStep(Sum())
