package lspace.librarian.traversal.util

import lspace.librarian.traversal.step.{InMap, OutMap}
import lspace.librarian.traversal.{MapStep, Traversal}
import lspace.structure.ClassType
import shapeless.{::, HList, HNil, Poly1}

object ProjectStepDataTypeMapper extends Poly1 {
  implicit def traversal[ST <: ClassType[Any], ET <: ClassType[Any], Step, Steps <: HList, Out, COut <: ClassType[Any]](
      implicit
      out: OutTweaker.Aux[ET, Step :: Steps, Out, COut]
  ): Case.Aux[Traversal[ST, ET, Step :: Steps], COut] = at[Traversal[ST, ET, Step :: Steps]](t => out.tweak(t.et))
  implicit def empty[ST <: ClassType[Any], ET <: ClassType[Any]]: Case.Aux[Traversal[ST, ET, HNil], ET] =
    at[Traversal[ST, ET, HNil]](t => t.et)

  //TODO: why does OutTweaker not resolve this?
  implicit def mapstep1[ST <: ClassType[Any], ET <: ClassType[Any]]: Case.Aux[Traversal[ST, ET, InMap :: HNil], ET] =
    at[Traversal[ST, ET, InMap :: HNil]](t => t.et)
  implicit def mapstep2[ST <: ClassType[Any], ET <: ClassType[Any]]: Case.Aux[Traversal[ST, ET, OutMap :: HNil], ET] =
    at[Traversal[ST, ET, OutMap :: HNil]](t => t.et)
}
