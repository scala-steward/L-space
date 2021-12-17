package lspace

import librarian.step._
import scala.quoted.ToExpr.EmptyTupleToExpr

extension [ST <: ClassType[?], ET <: ClassType[?], Steps <: Tuple](
  traversal: Traversal[ST, ET, Steps]
)
  // def and[traversals](
  //   traversals: ToTraversals[ET, traversals]
  // ): Traversal[ST, ET, Tuple.Concat[Steps, And[Traversals[traversals]] *: EmptyTuple]] = {
  //   traversals
  //     .map[[e0, e, steps] =>> Traversal[e0, e, steps]](
  //       [e0, e, steps] =>
  //         (t: (Traversal[e0, e0, EmptyTuple] => Traversal[e0, e, steps])) =>
  //           t(Traversal.empty[e0, e0])
  //     )
  //   traversals
  //     .map[[t] =>> t](
  //       [t] => (t: t) => t
  //     )
  //   Traversal(
  //     traversal.steps ++ (And[Traversals[traversals]](
  //       traversals
  //         .map[[e0, e, steps] =>> Traversal[e0, e, steps]](
  //           [e0, e, steps] =>
  //             (t: (Traversal[e0, e0, EmptyTuple] => Traversal[e0, e, steps])) => t(Traversal.empty[e0, e0])
  //         )
  //       // .TraversalsApply(Traversal[ET, ET, EmptyTuple](EmptyTuple), traversals)
  //       // (Traversals(traversals.map { f => f(Traversal[ET, ET, EmptyTuple](EmptyTuple)) }))
  //       // ToTraversals[ET, traversals](traversals.map(_.apply(Traversal[ET, ET, EmptyTuple](EmptyTuple)))): Traversals[traversals]
  //     ) *: EmptyTuple)
  //   ) // (traversal.st, traversal.et)
  // }

  def and[traversals](
    traversals: And.Traversals[traversals]
  ): Traversal[ST, ET, Tuple.Concat[Steps, And[Traversals[traversals]] *: EmptyTuple]] =
    Traversal(
      traversal.steps ++ (And(
        Traversals(traversals)
      ) *: EmptyTuple)
    ) // (traversal.st, traversal.et)

  def as[label <: As.Label](label: label): Traversal[ST, ET, Tuple.Concat[Steps, As[label] *: EmptyTuple]] =
    Traversal(traversal.steps ++ (As[label](label) *: EmptyTuple)) // (traversal.st, traversal.et)

  def choose[ByTraversal, LeftTraversal, RightTraversal](
    by: AnyTraversal[ByTraversal],
    left: AnyTraversal[LeftTraversal],
    right: AnyTraversal[RightTraversal]
  ): Traversal[ST, Choose.EndType[LeftTraversal, RightTraversal], Tuple.Concat[
    Steps,
    Choose[ByTraversal, LeftTraversal, RightTraversal] *: EmptyTuple
  ]] =
    Traversal(traversal.steps ++ (Choose(by, left, right) *: EmptyTuple)) // (traversal.st, traversal.et)

  def coalesce[traversals](traversals: traversals)
  : Traversal[ST, Coalesce.EndType[traversals], Tuple.Concat[
    Steps,
    Coalesce[traversals] *: EmptyTuple
  ]] = Traversal(traversal.steps ++ (Coalesce(Traversals(traversals)) *: EmptyTuple))

  def has[key <: Key](key: key): Traversal[ST, ET, Tuple.Concat[Steps, Has[key] *: EmptyTuple]] =
    Traversal(traversal.steps ++ (HasKey[key](key) *: EmptyTuple)) // (traversal.st, traversal.et)
  def has[key <: Key, predicate <: P[_]](
    key: key,
    predicate: predicate
  ): Traversal[ST, ET, Tuple.Concat[Steps, Has[key] *: EmptyTuple]] =
    Traversal(traversal.steps ++ (HasKeyWithValue[key, predicate](key, predicate) *: EmptyTuple))
  // (
  //   traversal.st,
  //   traversal.et
  // )
  def hasLabel[L <: ClassType[?]]: Traversal[ST, HasLabel.EndType[L], Tuple.Concat[Steps, HasLabel[L] *: EmptyTuple]] = 
    Traversal(traversal.steps ++ (HasLabel[L]() *: EmptyTuple))

  def in[key <: Key](
    key: key
  ): Traversal[ST, ResourceType[Any], Tuple.Concat[Steps, InKey[key] *: EmptyTuple]] =
    Traversal(traversal.steps ++ (InKey[key](key) *: EmptyTuple)) // (traversal.st, ResourceType[Any]())

  def out[key <: Key](
    key: key
  ): Traversal[ST, ResourceType[Any], Tuple.Concat[Steps, OutKey[key] *: EmptyTuple]] =
    Traversal(traversal.steps ++ (OutKey[key](key) *: EmptyTuple)) // (traversal.st, ResourceType[Any]())
  def out[key <: Key, predicate <: P[_]](
    key: key,
    predicate: predicate
  ): Traversal[ST, ResourceType[Any], Tuple.Concat[Steps, OutKeyPredicate[key, predicate] *: EmptyTuple]] =
    Traversal(traversal.steps ++ (OutKeyPredicate[key, predicate](key, predicate) *: EmptyTuple))
// (
//   traversal.st,
//   ResourceType[Any]()
// )
