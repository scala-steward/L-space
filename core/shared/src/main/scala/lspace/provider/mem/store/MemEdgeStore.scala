package lspace.provider.mem.store

import lspace.Label.D._
import lspace.datatype.DataType
import lspace.provider.mem.{MemGraph, MemResource}
import lspace.structure.Property.default.{`@id`, `@ids`}
import lspace.structure.store.EdgeStore
import lspace.structure.{Edge, Property}
import monix.eval.Task
import monix.reactive.Observable

class MemEdgeStore[G <: MemGraph](val iri: String, val graph: G) extends MemStore[G] with EdgeStore[G] {
  override def store(edge: T): Task[Unit] = Task { cache(edge) }
  override def cache(edge: T): Unit = {
    super.cache(edge)
    edge.from
      .asInstanceOf[MemResource[Any]]
      ._addOut(edge.asInstanceOf[Edge[Any, _]])
    edge.to
      .asInstanceOf[MemResource[Any]]
      ._addIn(edge.asInstanceOf[Edge[_, Any]])
    if ((edge.key == `@id` || edge.key == `@ids`) && edge.to
          .isInstanceOf[graph._Value[_]] && edge.to.asInstanceOf[graph._Value[_]].label == `@string`)
      graph.`@idStore`.cache(edge.to.asInstanceOf[graph.GValue[_]])
//    for {
//      _ <- if ((edge.key == `@id` || edge.key == `@ids`) && edge.to.isInstanceOf[graph._Value[String]])
//        graph.`@idStore`.cache(edge.to.asInstanceOf[graph.GValue[_]])
//      else Task.unit
//      edge <- super.cache(edge)
//    } yield edge
  }

  def hasIri(iri: String): Observable[T2] =
    graph.`@idStore`.byValue(iri, DataType.default.`@string`)
      .map(
        _.in(`@id`, `@ids`)
          .filter(_.isInstanceOf[Edge[_, _]])
          .asInstanceOf[List[T2]]
          .distinct)
      .flatMap(Observable.fromIterable(_))

  def hasIri(iri: Set[String]): Observable[T2] =
    Observable
      .fromTask(
        Observable
          .fromIterable(iri)
          .mergeMap(graph.`@idStore`.byValue(_, DataType.default.`@string`).filter(_.isInstanceOf[Edge[_, _]]))
          .toListL
          .map(_.asInstanceOf[List[T2]].distinct))
      .flatMap(Observable.fromIterable(_))

  def byId(fromId: Option[Long] = None, key: Option[Property] = None, toId: Option[Long] = None): Observable[T2] = {
    val edges = fromId match {
      case None =>
        key match {
          case None =>
            Observable.fromIterable(data.toStream.map(_._2))
          case Some(key) =>
            Observable.fromIterable(data.toStream.collect { case e if e._2.key == key => e._2 })
        }
      case Some(fromId) =>
        val fromResources = Observable.fromTask(graph.nodeStore.hasId(fromId)).flatMap(Observable.fromIterable(_))
        key match {
          case None =>
            fromResources.map(_.outE()).flatMap(Observable.fromIterable(_))
          case Some(key) =>
            fromResources.map(_.outE(key)).flatMap(Observable.fromIterable(_))
        }
    }
    toId match {
      case None =>
        edges.asInstanceOf[Observable[T2]]
      case Some(toId) =>
        edges.filter(_.to.id == toId).asInstanceOf[Observable[T2]]
    }
  }

  def byIri(fromIri: Option[String] = None,
            key: Option[Property] = None,
            toIri: Option[String] = None): Observable[T2] = {
    val edges = fromIri match {
      case None =>
        key match {
          case None =>
            Observable.fromIterable(data.toStream.map(_._2))
          case Some(key) =>
            Observable.fromIterable(data.toStream.collect { case e if e._2.key == key => e._2 })
        }
      case Some(fromIri) =>
        val fromResources =
          graph.`@idStore`.byValue(fromIri, DataType.default.`@string`)
            .map(_.inE(`@id`).map(_.from))
            .flatMap(Observable.fromIterable(_))
        key match {
          case None =>
            fromResources.map(_.outE()).flatMap(Observable.fromIterable(_))
          case Some(key) =>
            fromResources.map(_.outE(key)).flatMap(Observable.fromIterable(_))
        }
    }
    toIri match {
      case None =>
        edges.asInstanceOf[Observable[T2]]
      case Some(toIri) =>
        edges.filter(_.to.iris.contains(toIri)).asInstanceOf[Observable[T2]]
    }
  }
}

object MemEdgeStore {
  def apply[G <: MemGraph](iri: String, graph: G): MemEdgeStore[G] = new MemEdgeStore(iri, graph)
}
