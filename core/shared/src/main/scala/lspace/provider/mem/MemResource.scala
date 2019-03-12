package lspace.provider.mem

import java.util.concurrent.ConcurrentHashMap

import lspace.datatype.{DataType, TextType}
import lspace.structure.Property.default
import lspace.structure._

import scala.collection.{concurrent, mutable}
import scala.collection.JavaConverters._

object MemResource {}

trait MemResource[T] extends Resource[T] {
  val graph: MemGraph

  override def iri: String =
    linksOut
      .get(default.`@id`)
      .flatMap(_.headOption)
      .flatMap(_.inV.hasLabel(TextType.datatype).map(_.value))
      .getOrElse("")

  override def iris: Set[String] =
    linksOut.get(default.`@id`).map(_.flatMap(_.inV.hasLabel(TextType.datatype).map(_.value)).toSet).getOrElse(Set()) ++
      linksOut.get(default.`@ids`).map(_.flatMap(_.inV.hasLabel(TextType.datatype).map(_.value)).toSet).getOrElse(Set())

//  private val linksOut: concurrent.Map[Property, List[Edge[T, _]]] =
//    new ConcurrentHashMap[Property, List[Edge[T, _]]](16, 0.9f, 32).asScala
//  private val linksOut: mutable.OpenHashMap[Property, List[Edge[T, _]]] =
  private val linksOut: concurrent.Map[Property, List[Edge[T, _]]] =
//    mutable.OpenHashMap[Property, List[Edge[T, _]]]()
    new ConcurrentHashMap[Property, List[Edge[T, _]]]().asScala

  object Lock

  protected[lspace] def _addOut(edge: Edge[T, _]): Unit = Lock.synchronized {
    if (edge.from != this) throw new Exception("edge.from != this, cannot add out-link")
    linksOut += edge.key -> (edge :: linksOut
      .getOrElse(edge.key, List[Edge[T, _]]())
      .reverse).distinct.reverse
  }

//  private val linksIn: concurrent.Map[Property, List[Edge[_, T]]] =
//    new ConcurrentHashMap[Property, List[Edge[_, T]]](2, 0.9f, 4).asScala
  private val linksIn: concurrent.Map[Property, List[Edge[_, T]]] =
    new ConcurrentHashMap[Property, List[Edge[_, T]]]().asScala

  protected[lspace] def _addIn(edge: Edge[_, T]): Unit = Lock.synchronized {
    if (edge.to != this) throw new Exception("edge.from != this, cannot add in-link")
    linksIn += edge.key -> (edge :: linksIn
      .getOrElse(edge.key, List[Edge[_, T]]())
      .reverse).distinct.reverse
  }

  override def keys: Set[Property] = linksOut.keySet ++ linksIn.keySet toSet

  def out(key: Property*): List[Any] =
    if (key.nonEmpty) key.toList.flatMap(key => linksOut.get(key).toList.flatten).map(_.to.value)
    else linksOut.values.flatten.map(_.to.value).toList

  def outMap(key: Property*): Map[Property, List[Any]] = {
    if (key.isEmpty) linksOut.toMap.mapValues(_.map(_.to.value).toList)
    else outE(key: _*).groupBy(_.key).mapValues(_.map(_.to.value))
  }

  def outE(key: Property*): List[Edge[T, Any]] =
    if (key.nonEmpty) key.toList.flatMap(key => linksOut.get(key).toList.flatten).asInstanceOf[List[Edge[T, Any]]]
    else linksOut.values.toList.flatten.asInstanceOf[List[Edge[T, Any]]]

  def outEMap(key: Property*): Map[Property, List[Edge[T, Any]]] = {
    if (key.isEmpty) linksOut.toMap.mapValues(_.asInstanceOf[List[Edge[T, Any]]])
    else outE(key: _*).groupBy(_.key)
  }

  def in(key: Property*): List[Any] =
    if (key.nonEmpty) key.toList.flatMap(key => linksIn.get(key).toList.flatten).map(_.from.value)
    else linksIn.values.toList.flatten.map(_.from.value)

  def inMap(key: Property*): Map[Property, List[Any]] = {
    if (key.isEmpty) linksIn.toMap.mapValues(_.map(_.from.value).toList)
    else inE(key: _*).groupBy(_.key).mapValues(_.map(_.from.value))
  }

  def inE(key: Property*): List[Edge[Any, T]] =
    if (key.nonEmpty) key.toList.flatMap(key => linksIn.getOrElse(key, List())).asInstanceOf[List[Edge[Any, T]]]
    else linksIn.values.toList.flatten.asInstanceOf[List[Edge[Any, T]]]

  def inEMap(key: Property*): Map[Property, List[Edge[Any, T]]] = {
    if (key.isEmpty) linksIn.toMap.mapValues(_.asInstanceOf[List[Edge[Any, T]]])
    else inE(key.toList: _*).groupBy(_.key)
  }

  private def validateDT[V](dt: DataType[V], value: V) =
    if (dt.iri.nonEmpty) dt else ClassType.valueToOntologyResource(value)

  def removeIn[V >: T](edge: Edge[_, V]): Unit = Lock.synchronized {
    linksIn.get(edge.key).foreach { links =>
      if (links.contains(edge)) {
        val newSet = links.filterNot(_ == edge)
        if (newSet.isEmpty) linksIn -= edge.key
        else linksIn += edge.key -> newSet
        edge.remove()
      }
    }
  }
  def removeOut[V >: T](edge: Edge[V, _]): Unit = Lock.synchronized {
    linksOut.get(edge.key).foreach { links =>
      if (links.contains(edge)) {
        val newSet = links.filterNot(_ == edge)
        if (newSet.isEmpty) linksOut -= edge.key
        else linksOut += edge.key -> newSet
        edge.remove()
      }
    }
  }
  def removeIn(key: Property): Unit = Lock.synchronized {
    val toRemove = inE(key)
    linksIn -= key
    toRemove.foreach(_.remove())
  }
  def removeOut(key: Property): Unit = Lock.synchronized {
    val toRemove = outE(key)
    linksOut -= key
    toRemove.foreach(_.remove())
  }
}
