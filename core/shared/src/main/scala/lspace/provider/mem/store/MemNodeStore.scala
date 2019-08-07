package lspace.provider.mem.store

import lspace.datatype.DataType
import lspace.provider.mem.MemGraph
import lspace.structure.Node
import lspace.structure.Property.default.{`@id`, `@ids`}
import lspace.structure.store.NodeStore
import monix.reactive.Observable

class MemNodeStore[G <: MemGraph](val iri: String, val graph: G) extends MemStore[G] with NodeStore[G] {
  def hasIri(iri: String): Observable[T2] =
    graph.`@idStore`.byValue(iri, DataType.default.`@string`)
      .map(
        _.in(`@id`, `@ids`)
          .filter(_.isInstanceOf[Node])
          .asInstanceOf[List[T2]]
          .distinct)
      .flatMap(Observable.fromIterable(_))

  def hasIri(iri: Set[String]): Observable[T2] =
    Observable
      .fromTask(
        Observable
          .fromIterable(iri)
          .mergeMap(graph.`@idStore`.byValue(_, DataType.default.`@string`).map(
            _.in(`@id`, `@ids`).filter(_.isInstanceOf[Node])))
          .flatMap(Observable.fromIterable)
          .toListL
          .map(_.asInstanceOf[List[T2]].distinct))
      .flatMap(Observable.fromIterable(_))
}

object MemNodeStore {
  def apply[G <: MemGraph](iri: String, graph: G): MemNodeStore[G] = new MemNodeStore(iri, graph)
}
