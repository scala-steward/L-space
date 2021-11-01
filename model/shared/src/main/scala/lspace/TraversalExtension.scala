package lspace

extension [ST <: ClassType[Any], ET <: ClassType[Any], Steps <: Tuple](
  traversal: Traversal[ST, ET, Steps]
)
  // def and: Traversal[ST, ET, StepsLeaf[Step.And *: Steps]] =
  //   Traversal(Step.And() *: traversal.steps)(traversal.st, traversal.et)
  def has[p <: String](predicate: p): Traversal[ST, ET, Tuple.Concat[Steps, Tuple1[Has[predicate.type]]]] =
    Traversal(traversal.steps ++ Tuple1(Has(predicate)))(traversal.st, traversal.et)
  def out[P <: String](
    predicate: P
  ): Traversal[ST, ResourceType[Any], Tuple.Concat[Steps, Tuple1[Out[predicate.type]]]] =
    Traversal(traversal.steps ++ Tuple1(Out(predicate)))(traversal.st, ResourceType[Any]())
