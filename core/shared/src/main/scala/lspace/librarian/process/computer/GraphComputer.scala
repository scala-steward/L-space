package lspace.librarian.process.computer

import lspace.librarian.process.traversal.Traversal
import lspace.librarian.structure.{ClassType, Graph, IriResource}
import shapeless.{::, =:!=, HList}
import shapeless.ops.hlist.{LeftFolder, Tupler}

trait GraphComputer extends IriResource {
  def traverse[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList, Out, GT <: Graph](
      traversal: Traversal[ST, ET, Steps])(implicit
                                           graph: GT): Stream[Out]

}
