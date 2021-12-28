package lspace

import librarian.step._
import scala.quoted.ToExpr.EmptyTupleToExpr

extension [ST <: ClassType[?], ET <: ClassType[?], Steps <: Tuple](traversal: Traversal[ST, ET, Steps])

  def and[traversals](
    traversals: traversals
  ): Traversal[ST, ET, Traversal.StepsConcat[Steps, And[And.Traversals[traversals]]]] =
    traversal.addStep(And(traversals)) // (traversal.st, traversal.et)

  def as[label <: As.Label](label: label): Traversal[ST, ET, Traversal.StepsConcat[Steps, As[label]]] =
    traversal.addStep(As[label](label)) // (traversal.st, traversal.et)

  def choose[by, left, right](
    by: by,
    left: left,
    right: right
  ): Traversal[ST, Choose.EndType[left, right], Traversal.StepsConcat[
    Steps,
    Choose[Choose.By[by], Choose.Left[left], Choose.Right[right]]
  ]] =
    traversal.addStep(Choose(by, left, right)) // (traversal.st, traversal.et)

  def coalesce[traversals](
    traversals: traversals
  ): Traversal[ST, Coalesce.EndType[traversals], Traversal.StepsConcat[
    Steps,
    Coalesce[Coalesce.Traversals[traversals]]
  ]] = traversal.addStep(Coalesce(traversals))

  def coin[p <: Double with Singleton](
    p: Coin.Probability[p]
  ): Traversal[ST, ET, Traversal.StepsConcat[Steps, Coin[p]]] =
    traversal.addStep(Coin(p))

  def constant[V](
    v: V
  ): Traversal[ST, ClassType.Apply[V], Traversal.StepsConcat[Steps, Constant[Constant.Value[V]]]] =
    traversal.addStep(Constant(v))

  def count: Traversal[ST, LongType[Long], Traversal.StepsConcat[Steps, Count.type]] =
    traversal.addStep(Count)

  def dedup: Traversal[ST, ET, Traversal.StepsConcat[Steps, Dedup.type]] =
    traversal.addStep(Dedup)

  def group[by, value](
    by: by,
    value: value
  ): Traversal[ST, Group.EndType[by, value], Traversal.StepsConcat[
    Steps,
    Group[Group.By[by], Group.Value[value]]
  ]] =
    traversal.addStep(Group(by, value)) // (traversal.st, traversal.et)

  def has(label: Key[?]): Traversal[ST, ET, Traversal.StepsConcat[Steps, Has[Nothing]]] =
    traversal.addStep(Has(label)) // (traversal.st, traversal.et)
  def has[predicate <: P[_]](
    label: Key[?],
    predicate: predicate
  ): Traversal[ST, ET, Traversal.StepsConcat[Steps, Has[predicate]]] =
    traversal.addStep(Has[predicate](label, Some(predicate)))
  // (
  //   traversal.st,
  //   traversal.et
  // )

  def hasId(id: Long): Traversal[ST, NodeType, Traversal.StepsConcat[Steps, HasId]] =
    traversal.addStep(HasId(id))

  def hasIri(iri: Iri): Traversal[ST, NodeType, Traversal.StepsConcat[Steps, HasIri]] =
    traversal.addStep(HasIri(iri))

  def hasLabel[L <: ClassType[?]](l: L): Traversal[ST, HasLabel.EndType[L], Traversal.StepsConcat[Steps, HasLabel[L]]] =
    traversal.addStep(HasLabel(l))

  // def hasNot[]()
  // def hasValue

  def head: Traversal[ST, ET, Traversal.StepsConcat[Steps, Head.type]] =
    traversal.addStep(Head)

  def id: Traversal[ST, LongType[Long], Traversal.StepsConcat[Steps, Id.type]] =
    traversal.addStep(Id)

  def in[keys](
    keys: keys
  ): Traversal[ST, ResourceType[Any], Traversal.StepsConcat[Steps, In[MapStep.KeyTuple[keys]]]] =
    traversal.addStep(In(keys)) // (traversal.st, ResourceType[Any]())

  def inE[keys](
    keys: keys
  ): Traversal[ST, InE.EndType[ClassType[Any], ET], Traversal.StepsConcat[Steps, InE[MapStep.KeyTuple[keys]]]] =
    traversal.addStep(InE(keys))

  def is[predicate <: P[_]](
    predicate: predicate
  ): Traversal[ST, ET, Traversal.StepsConcat[Steps, Is[predicate]]] =
    traversal.addStep(Is[predicate](predicate))

  def out[keys](
    keys: keys
  ): Traversal[ST, ResourceType[Any], Traversal.StepsConcat[Steps, Out[MapStep.KeyTuple[keys]]]] =
    traversal.addStep(Out(keys)) // (traversal.st, ResourceType[Any]())

  def outE[keys](
    keys: keys
  ): Traversal[ST, OutE.EndType[ET], Traversal.StepsConcat[Steps, OutE[MapStep.KeyTuple[keys]]]] =
    traversal.addStep(OutE(keys))

// def out[key <: Key, predicate <: P[_]](
//   key: key,
//   predicate: predicate
// ): Traversal[ST, ResourceType[Any], Traversal.StepsConcat[Steps, OutKeyPredicate[key, predicate]]] =
//   traversal.addStep(OutKeyPredicate[key, predicate](key, predicate))
// (
//   traversal.st,
//   ResourceType[Any]()
// )
