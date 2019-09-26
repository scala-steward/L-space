package lspace.librarian.traversal.util

import lspace.librarian.traversal.Traversal
import lspace.librarian.traversal.step.{InMap, OutMap}
import lspace.structure.ClassType
import shapeless.{::, HList, HNil, Poly1}

object ProjectStepTypeMapper extends Poly1 {
  implicit def traversal[ST <: ClassType[Any], ET <: ClassType[Any], Step, Steps <: HList, Out, COut <: ClassType[Any]](
      implicit
      out: OutTweaker.Aux[ET, Step :: Steps, Out, COut]
  ): Case.Aux[Traversal[ST, ET, Step :: Steps], Out] =
    at[Traversal[ST, ET, Step :: Steps]](t => 1.asInstanceOf[out.Out])
  implicit def empty[ST <: ClassType[Any], E, ET[+Z] <: ClassType[Z]]: Case.Aux[Traversal[ST, ET[E], HNil], E] =
    at[Traversal[ST, ET[E], HNil]](t => 1.asInstanceOf[E])

  //TODO: why does OutTweaker not resolve this?
  implicit def mapstep1[ST <: ClassType[Any], E, ET[+Z] <: ClassType[Z]]
    : Case.Aux[Traversal[ST, ET[E], InMap :: HNil], E] =
    at[Traversal[ST, ET[E], InMap :: HNil]](t => 1.asInstanceOf[E])
  implicit def mapstep2[ST <: ClassType[Any], E, ET[+Z] <: ClassType[Z]]
    : Case.Aux[Traversal[ST, ET[E], OutMap :: HNil], E] =
    at[Traversal[ST, ET[E], OutMap :: HNil]](t => 1.asInstanceOf[E])
}
