package lspace.librarian.process.computer

import java.time.Instant

import lspace.librarian.process.traversal.{TraversalPath, Traverser}
import lspace.librarian.structure.{Graph, Resource}

/**
  *
  * @param tempGraph a temporary graph with modifications on top of the original graph
  */
class TransactionStreamComputer(tempGraph: Graph) extends DefaultStreamComputer {
  private case class TransactionStreamComputerTraverser[+T](path: TraversalPath = TraversalPath(),
                                                            loops: Int = 0,
                                                            mit: Option[Instant] = None,
                                                            permissions: List[String] = List())(_get: T)
      extends Traverser[T] {
    lazy val get: T = (_get match {
      case r: Resource[_] =>
        tempGraph.resources.hasId(r.id).getOrElse(throw new Exception("traverser at foreign resource"))
      case _ => _get
    }).asInstanceOf[T]

    def apply[V](get: V,
                 path: TraversalPath = TraversalPath(),
                 loops: Int = 0,
                 mit: Option[Instant] = None,
                 permissions: List[String] = List()): Traverser[V] =
      TransactionStreamComputerTraverser(path, loops, mit, permissions)(get)

  }

  override def createTraverser[T](get: T,
                                  path: TraversalPath = TraversalPath(),
                                  loops: Int = 0,
                                  mit: Option[Instant] = None,
                                  permissions: List[String] = List()): Traverser[T] =
    TransactionStreamComputerTraverser(path, loops, mit, permissions)(get)
}
