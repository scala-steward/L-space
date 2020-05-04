package lspace.provider.mem

import java.util.concurrent.ConcurrentHashMap

import lspace.datatype.{DataType, TextType}
import lspace.structure.Property.default
import lspace.structure._
import monix.eval.Task
import monix.reactive.Observable

import scala.collection.{concurrent, mutable}
import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

object MemResource {}

trait MemResource[T] extends Resource[T] {
  val graph: MemGraph

  override def iri: String =
    linksOut
      .get(default.`@id`)
      .flatMap(_.headOption)
      .flatMap(_.to.hasLabel(TextType.datatype).map(_.value))
      .getOrElse("")

  override def iris: Set[String] =
    linksOut.get(default.`@id`).map(_.flatMap(_.to.hasLabel(TextType.datatype).map(_.value)).toSet).getOrElse(Set()) ++
      linksOut.get(default.`@ids`).map(_.flatMap(_.to.hasLabel(TextType.datatype).map(_.value)).toSet).getOrElse(Set())

  private val linksOut: concurrent.Map[Property, HashSet[Edge[T, _]]] =
    new ConcurrentHashMap[Property, HashSet[Edge[T, _]]](16, 0.9f, 32).asScala
//private val linksOut: concurrent.TrieMap[Property, List[Edge[T, _]]] =
//    new concurrent.TrieMap[Property, List[Edge[T, _]]]()

  object Lock

  protected[lspace] def _addOut(edge: Edge[T, _]): Unit = Lock.synchronized {
    if (edge.from != this) throw new Exception("edge.from != this, cannot add out-link")
    linksOut += edge.key ->
      (linksOut
        .getOrElse(edge.key, HashSet[Edge[T, _]]()) + edge)
  }

  private val linksIn: concurrent.Map[Property, HashSet[Edge[_, T]]] =
    new ConcurrentHashMap[Property, HashSet[Edge[_, T]]](2, 0.9f, 4).asScala
//  private val linksIn: concurrent.TrieMap[Property, List[Edge[_, T]]] =
//    new concurrent.TrieMap[Property, List[Edge[_, T]]]() //.asScala

  protected[lspace] def _addIn(edge: Edge[_, T]): Unit = Lock.synchronized {
    if (edge.to != this) throw new Exception("edge.to != this, cannot add in-link")
    linksIn += edge.key -> (linksIn
      .getOrElse(edge.key, HashSet[Edge[_, T]]()) + edge)
  }

  override def keys: Set[Property] = (linksOut.keySet ++ linksIn.keySet).toSet

  def out(key: Property*): List[Any] =
    if (key.nonEmpty)
      (key.toSet ++ key.toList.flatMap(_.extendedBy.all())).toList
        .flatMap(key => linksOut.get(key).toList.flatten)
        .map(_.to.value)
    else linksOut.values.flatten.map(_.to.value).toList

  def outMap(key: Property*): Map[Property, List[Any]] =
    if (key.isEmpty) linksOut.toMap.mapValues(_.map(_.to.value).toList).toMap
    else outE(key: _*).groupBy(_.key).mapValues(_.map(_.to.value)).toMap

  def outE(key: Property*): List[Edge[T, Any]] =
    if (key.nonEmpty)
      (key.toSet ++ key.toList.flatMap(_.extendedBy.all())).toList
        .flatMap(key => linksOut.get(key).toList.flatten)
        .asInstanceOf[List[Edge[T, Any]]]
    else linksOut.values.toList.flatten.asInstanceOf[List[Edge[T, Any]]]

  def outEMap(key: Property*): Map[Property, List[Edge[T, Any]]] =
    if (key.isEmpty) linksOut.toMap.mapValues(_.asInstanceOf[HashSet[Edge[T, Any]]].toList).toMap
    else outE(key: _*).groupBy(_.key)

  def in(key: Property*): List[Any] =
    if (key.nonEmpty)
      (key.toSet ++ key.toList.flatMap(_.extendedBy.all())).toList
        .flatMap(key => linksIn.get(key).toList.flatten)
        .map(_.from.value)
    else linksIn.values.toList.flatten.map(_.from.value)

  def inMap(key: Property*): Map[Property, List[Any]] =
    if (key.isEmpty) linksIn.toMap.mapValues(_.map(_.from.value).toList).toMap
    else inE(key: _*).groupBy(_.key).mapValues(_.map(_.from.value)).toMap

  def inE(key: Property*): List[Edge[Any, T]] =
    if (key.nonEmpty)
      (key.toSet ++ key.toList.flatMap(_.extendedBy.all())).toList
        .flatMap(key => linksIn.get(key).toList.flatten)
        .asInstanceOf[List[Edge[Any, T]]]
    else linksIn.values.toList.flatten.asInstanceOf[List[Edge[Any, T]]]

  def inEMap(key: Property*): Map[Property, List[Edge[Any, T]]] =
    if (key.isEmpty) linksIn.toMap.mapValues(_.asInstanceOf[HashSet[Edge[Any, T]]].toList).toMap
    else inE(key.toList: _*).groupBy(_.key)

  private def validateDT[V](dt: DataType[V], value: V) =
    if (dt.iri.nonEmpty) dt else ClassType.detect(value)

  def removeIn[V >: T](edge: Edge[_, V]): Task[Unit] = Task.defer {
    Lock.synchronized {
      Observable
        .fromIterable(linksIn.get(edge.key))
        .map { links =>
          if (links.contains(edge.asInstanceOf[Edge[_, T]])) {
            val newSet = links - edge.asInstanceOf[Edge[_, T]]
            if (newSet.isEmpty) linksIn -= edge.key
            else linksIn += edge.key -> newSet
//            edge.remove()
          } else Task.unit
        }
        .completedL
    }
  }
  def removeOut[V >: T](edge: Edge[V, _]): Task[Unit] = Task.defer {
    Lock.synchronized {
      Observable
        .fromIterable(linksOut.get(edge.key))
        .map { links =>
          if (links.contains(edge.asInstanceOf[Edge[T, _]])) {
            val newSet = links - edge.asInstanceOf[Edge[T, _]]
            if (newSet.isEmpty) linksOut -= edge.key
            else linksOut += edge.key -> newSet
//            edge.remove()
          } else Task.unit
        }
        .completedL
    }
  }
  def removeIn(key: Property): Task[Unit] = Task.defer {
    Lock.synchronized {
      val toRemove = inE(key)
      linksIn -= key
      for { _ <- Task.parSequence(toRemove.map(_.remove())) } yield ()
    }
  }
  def removeOut(key: Property): Task[Unit] = Task.defer {
    Lock.synchronized {
      val toRemove = outE(key)
      linksOut -= key
      for { _ <- Task.parSequence(toRemove.map(_.remove())) } yield ()
    }
  }
}
