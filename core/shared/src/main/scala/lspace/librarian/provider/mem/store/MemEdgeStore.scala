package lspace.librarian.provider.mem.store

import lspace.librarian.provider.mem.{MemGraph, MemResource}
import lspace.librarian.structure.Property.default.{`@id`, `@ids`}
import lspace.librarian.structure.store.EdgeStore
import lspace.librarian.structure.{Edge, Property}

import scala.collection.mutable

class MemEdgeStore[G <: MemGraph](val iri: String, val graph: G) extends MemStore[G] with EdgeStore[G] {
  override def store(resource: T): Unit = {
    resource.from
      .asInstanceOf[MemResource[Any]]
      .linksOut += resource.key -> (resource.from
      .asInstanceOf[MemResource[Any]]
      .linksOut
      .getOrElse(resource.key, mutable.LinkedHashSet[Edge[Any, Any]]()) += resource)

    resource.to.asInstanceOf[MemResource[Any]].linksIn += resource.key -> (resource.to
      .asInstanceOf[MemResource[Any]]
      .linksIn
      .getOrElse(resource.key, mutable.LinkedHashSet[Edge[Any, Any]]()) += resource)

    if ((resource.key == `@id` || resource.key == `@ids`) && resource.to.isInstanceOf[graph._Value[String]])
      graph.`@idStore`.store(resource.to.asInstanceOf[graph._Value[String]])

    super.store(resource)
  }

  def byIri(iri: String): Stream[T] =
    graph.`@idStore`.byValue(iri)
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
        val fromResources = graph.`@idStore`.byValue(fromIri).flatMap(_.inE(`@id`).map(_.from))
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
