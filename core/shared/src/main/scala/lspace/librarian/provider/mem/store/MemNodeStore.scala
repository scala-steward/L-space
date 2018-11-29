package lspace.librarian.provider.mem.store

import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure.Node
import lspace.librarian.structure.Property.default.{`@id`, `@ids`}
import lspace.librarian.structure.store.NodeStore

class MemNodeStore[G <: MemGraph](val iri: String, val graph: G) extends MemStore[G] with NodeStore[G] {
  def byIri(iri: String): Stream[T] =
    graph.`@idStore`.byValue(iri)
      .flatMap(_.in(`@id`, `@ids`).filter(_.isInstanceOf[T]))
      .asInstanceOf[Stream[T]]
      .distinct
}

object MemNodeStore {
  def apply[G <: MemGraph](iri: String, graph: G): MemNodeStore[G] = new MemNodeStore(iri, graph)
}
