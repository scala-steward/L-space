package lspace.librarian.provider.mem.store

import lspace.librarian.datatype.DataType
import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure.Property.default.{`@id`, `@ids`}
import lspace.librarian.structure.store.NodeStore

class MemNodeStore[G <: MemGraph](val iri: String, val graph: G) extends MemStore[G] with NodeStore[G] {
  def hasIri(iri: String): Stream[T] =
    graph.`@idStore`.byValue(iri, DataType.default.`@string`)
      .flatMap(_.in(`@id`, `@ids`).filter(_.isInstanceOf[T]))
      .asInstanceOf[Stream[T]]
      .distinct
}

object MemNodeStore {
  def apply[G <: MemGraph](iri: String, graph: G): MemNodeStore[G] = new MemNodeStore(iri, graph)
}
