package lspace.librarian.process.computer

import java.time.Instant

import lspace.librarian.process.traversal.{Traversal, TraversalPath, Traverser}
import lspace.librarian.structure.{ClassType, Graph, IriResource}
import shapeless.{::, =:!=, HList}
import shapeless.ops.hlist.{LeftFolder, Tupler}

trait GraphComputer extends IriResource {
  private case class GraphComputerTraverser[+T](get: T,
                                                path: TraversalPath = TraversalPath(),
                                                loops: Int = 0,
                                                mit: Option[Instant] = None,
                                                permissions: List[String] = List())
      extends Traverser[T] {
    def apply[V](get: V,
                 path: TraversalPath = TraversalPath(),
                 loops: Int = 0,
                 mit: Option[Instant] = None,
                 permissions: List[String] = List()): Traverser[V] =
      GraphComputerTraverser(get, path, loops, mit, permissions)
  }
//  def createTraverser[T](get: T): Traverser[T] = GraphComputerTraverser(get)

  def createTraverser[T](get: T,
                         path: TraversalPath = TraversalPath(),
                         loops: Int = 0,
                         mit: Option[Instant] = None,
                         permissions: List[String] = List()): Traverser[T] =
    GraphComputerTraverser(get, path, loops, mit, permissions)

  def traverse[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList, Out, GT <: Graph](
      traversal: Traversal[ST, ET, Steps])(implicit
                                           graph: GT): Stream[Out]

}
