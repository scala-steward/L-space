package lspace.librarian.traversal.util

import lspace.librarian.traversal.Traversal
import lspace.structure.ClassType
import shapeless.{HList, Poly1}

object ProjectStepTypeMapper extends Poly1 {
  implicit def traversal[ST <: ClassType[Any], ET <: ClassType[Any], Steps <: HList, Out, COut <: ClassType[Any]](
    implicit out: EndMapper.Aux[ET, Steps, Out, COut]
  ): Case.Aux[Traversal[ST, ET, Steps], Out] =
    at[Traversal[ST, ET, Steps]](_ => 1.asInstanceOf[out.Out])
}
