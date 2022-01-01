package lspace

import librarian.step._
import scala.quoted.ToExpr.EmptyTupleToExpr

extension [ST <: ClassType[?], ET <: ClassType[?], Steps <: Tuple](_traversal: Traversal[ST, ET, Steps])

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
  ): Traversal[ST, Choose.EndType[left, right], Traversal.StepsConcat[
    Steps,
    Choose[Choose.By[by], Choose.Left[left], Choose.Right[right]]
  ]] =
    _traversal.addStep(Choose(by, left, right), Choose.EndType(left, right))

  def coalesce[traversals](
    traversals: traversals
  ): Traversal[ST, Coalesce.EndType[traversals], Traversal.StepsConcat[
    Steps,
    Coalesce[Coalesce.Traversals[traversals]]
  ]] = _traversal.addStep(Coalesce(traversals), Coalesce.EndType(traversals))

  def coin[p <: Double with Singleton](
    p: Coin.Probability[p]
  ): Traversal[ST, ET, Traversal.StepsConcat[Steps, Coin[p]]] =
    _traversal.addStep(Coin(p))

  def constant[V](
    v: V
  ): Traversal[ST, ClassType.Able[V], Traversal.StepsConcat[Steps, Constant[Constant.Value[V]]]] =
    _traversal.addStep(Constant(v), ClassType.Able(v))

  def count: Traversal[ST, LongType[Long], Traversal.StepsConcat[Steps, Count.type]] =
    _traversal.addStep(Count, LongType)

  def dedup: Traversal[ST, ET, Traversal.StepsConcat[Steps, Dedup.type]] =
    _traversal.addStep(Dedup)

  def group[by, value](
    by: by,
    value: value
  ): Traversal[ST, Group.EndType[by, value], Traversal.StepsConcat[
    Steps,
    Group[Group.By[by], Group.Value[value]]
  ]] =
    _traversal.addStep(Group(by, value), Group.EndType(by, value))

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

  def hasLabel[label <: ClassType[?]](
    label: label
  ): Traversal[ST, HasLabel.EndType[label], Traversal.StepsConcat[Steps, HasLabel[HasLabel.EndType[label]]]] =
    _traversal.addStep(HasLabel(HasLabel.EndType(label)), HasLabel.EndType(label))

  // def hasNot[]()
  // def hasValue

  def head: Traversal[ST, ET, Traversal.StepsConcat[Steps, Head.type]] =
    _traversal.addStep(Head)

  def id: Traversal[ST, LongTyped, Traversal.StepsConcat[Steps, Id.type]] =
    _traversal.addStep(Id, LongType)

  def in[keys](
    keys: keys
  ): Traversal[ST, ResourceType, Traversal.StepsConcat[Steps, In[MapStep.KeyNameTuple[keys]]]] =
    _traversal.addStep(In(keys), ResourceType)

  def inE[keys](
    keys: keys
  ): Traversal[ST, InE.EndType[ResourceType, ET], Traversal.StepsConcat[Steps, InE[MapStep.KeyNameTuple[keys]]]] =
    _traversal.addStep(InE(keys), OutE.EndType(ResourceType, _traversal.et))

  def is[predicate <: P[_]](
    predicate: predicate
  ): Traversal[ST, ET, Traversal.StepsConcat[Steps, Is[predicate]]] =
    _traversal.addStep(Is[predicate](predicate))

  def last: Traversal[ST, ET, Traversal.StepsConcat[Steps, Last]] =
    _traversal.addStep(Last())

  def limit[max <: Limit.Max](max: max): Traversal[ST, ET, Traversal.StepsConcat[Steps, Limit[max]]] =
    _traversal.addStep(Limit(max))

  def local[traversal](traversal: traversal): Traversal[ST, Local.EndType[traversal], Traversal.StepsConcat[
    Steps,
    Local[Local.TraversalType[traversal]]
  ]] = _traversal.addStep(Local(traversal), Local.EndType(traversal))

  def max[traversal](
    traversal: traversal
  )(using ET =:= Max.StartType[traversal]): Traversal[ST, Max.EndType[traversal], Traversal.StepsConcat[
    Steps,
    Max[Max.Able[traversal]]
  ]] =
    _traversal.addStep(Max(traversal), Max.EndType(traversal))

  // def out[key <: Key.Name](key: key): Traversal[ST, ResourceType, Traversal.StepsConcat[Steps, Out[MapStep.KeyNameTuple[key *: EmptyTuple]]]] =
  //     _traversal.addStep(Out(key), ResourceType)

  def out[keys](
    keys: keys
  ): Traversal[ST, ResourceType, Traversal.StepsConcat[Steps, Out[MapStep.KeyNameTuple[keys]]]] =
    _traversal.addStep(Out(keys), ResourceType)

  def outE[keys](
    keys: keys
  ): Traversal[ST, OutE.EndType[ET, ResourceType], Traversal.StepsConcat[Steps, OutE[MapStep.KeyNameTuple[keys]]]] =
    _traversal.addStep(OutE(keys), OutE.EndType(_traversal.et, ResourceType))

// def out[key <: Key, predicate <: P[_]](
//   key: key,
//   predicate: predicate
// ): Traversal[ST, ResourceType[Any], Traversal.StepsConcat[Steps, OutKeyPredicate[key, predicate]]] =
//   _traversal.addStep(OutKeyPredicate[key, predicate](key, predicate))
// (
//   traversal.st,
//   ResourceType[Any]()
// )

  def select[labels](
    labels: labels
  ): Traversal[ST, Select.EndType[Steps, labels], Traversal.StepsConcat[Steps, Select[Select.Able[labels]]]] =
    _traversal.addStep(Select(labels), Select.EndType(_traversal.steps, labels))
