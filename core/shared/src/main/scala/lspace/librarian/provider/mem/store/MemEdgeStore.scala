package lspace.librarian.provider.mem.store

import lspace.librarian.provider.mem.{MemGraph, MemResource}
import lspace.librarian.structure.Property.default.{`@id`, `@ids`}
import lspace.librarian.structure.store.EdgeStore
import lspace.librarian.structure.{DataType, Edge, Property}

class MemEdgeStore[G <: MemGraph](val iri: String, val graph: G) extends MemStore[G] with EdgeStore[G] {
  override def store(edge: T): Unit = {
    edge.from
      .asInstanceOf[MemResource[Any]]
      ._addOut(edge.asInstanceOf[Edge[Any, _]])

    edge.to
      .asInstanceOf[MemResource[Any]]
      ._addIn(edge.asInstanceOf[Edge[_, Any]])

    if ((edge.key == `@id` || edge.key == `@ids`) && edge.to.isInstanceOf[graph._Value[String]])
      graph.`@idStore`.store(edge.to.asInstanceOf[graph._Value[String]])

    super.store(edge)
  }

  def byIri(iri: String): Stream[T] =
    graph.`@idStore`.byValue(iri, DataType.default.`@string`)
      .flatMap(_.in(`@id`, `@ids`).filter(_.isInstanceOf[Edge[_, _]]))
      .asInstanceOf[Stream[T]]
      .distinct

  def byId(fromId: Option[Long] = None, key: Option[Property] = None, toId: Option[Long] = None): Stream[T] = {
    val edges = fromId match {
      case None =>
        key match {
          case None =>
            data.toStream.map(_._2)
          case Some(key) =>
            data.toStream.collect { case e if e._2.key == key => e._2 }
        }
      case Some(fromId) =>
        val fromResources = graph.nodeStore.byId(fromId).toStream
        key match {
          case None =>
            fromResources.flatMap(_.outE())
          case Some(key) =>
            fromResources.flatMap(_.outE(key))
        }
    }
    toId match {
      case None =>
        edges.asInstanceOf[Stream[T]]
      case Some(toId) =>
        edges.filter(_.to.id == toId).asInstanceOf[Stream[T]]
    }
  }

  def byIri(fromIri: Option[String] = None, key: Option[Property] = None, toIri: Option[String] = None): Stream[T] = {
    val edges = fromIri match {
      case None =>
        key match {
          case None =>
            data.toStream.map(_._2)
          case Some(key) =>
            data.toStream.collect { case e if e._2.key == key => e._2 }
        }
      case Some(fromIri) =>
        val fromResources =
          graph.`@idStore`.byValue(fromIri, DataType.default.`@string`).flatMap(_.inE(`@id`).map(_.from))
        key match {
          case None =>
            fromResources.flatMap(_.outE())
          case Some(key) =>
            fromResources.flatMap(_.outE(key))
        }
    }
    toIri match {
      case None =>
        edges.asInstanceOf[Stream[T]]
      case Some(toIri) =>
        edges.filter(_.to.iris.contains(toIri)).asInstanceOf[Stream[T]]
    }
  }
}

object MemEdgeStore {
  def apply[G <: MemGraph](iri: String, graph: G): MemEdgeStore[G] = new MemEdgeStore(iri, graph)
}
