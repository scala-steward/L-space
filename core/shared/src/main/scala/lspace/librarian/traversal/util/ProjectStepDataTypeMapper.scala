package lspace.librarian.traversal.util

import lspace.librarian.traversal.Traversal
import lspace.structure.ClassType
import shapeless.{HList, Poly1}

object ProjectStepDataTypeMapper extends Poly1 {
  implicit def traversal[ST <: ClassType[Any], ET <: ClassType[Any], Steps <: HList, Out, COut <: ClassType[Any]](
      implicit
      out: OutTweaker.Aux[ET, Steps, Out, COut]
  ): Case.Aux[Traversal[ST, ET, Steps], COut] = at[Traversal[ST, ET, Steps]](t => out.tweak(t.et))
}
