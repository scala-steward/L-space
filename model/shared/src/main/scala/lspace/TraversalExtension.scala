package lspace

extension [ST <: ClassType[Any], ET <: ClassType[Any], Steps <: Tuple](
  traversal: Traversal[ST, ET, Steps]
)
  // def and: Traversal[ST, ET, StepsLeaf[Step.And *: Steps]] =
  //   Traversal(Step.And() *: traversal.steps)(traversal.st, traversal.et)
  def has[key <: String](key: key): Traversal[ST, ET, Tuple.Concat[Steps, Tuple1[Has[key.type]]]] =
    Traversal(traversal.steps ++ Tuple1(Has(key)))(traversal.st, traversal.et)
  def has[key <: String, predicate <: P[_]](
    key: key,
    predicate: predicate
  ): Traversal[ST, ET, Tuple.Concat[Steps, Tuple1[Has[key.type]]]] =
    Traversal(traversal.steps ++ Tuple1(Has(key, predicate)))(traversal.st, traversal.et)
  def out[key <: String, predicate <: P[_]](
    key: key,
    predicate: predicate
  ): Traversal[ST, ResourceType[Any], Tuple.Concat[Steps, Tuple1[Out[key.type, predicate]]]] =
    Traversal(traversal.steps ++ Tuple1(Out(key, predicate)))(traversal.st, ResourceType[Any]())
