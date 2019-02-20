package lspace.librarian.task

import lspace.structure.Graph

case class TraversalTask[T](f: Graph => T /*, librarianSummoner: Any => Coeval[Librarian[Any]]*/ ) {
  def run(graph: Graph): T = f(graph)
}
