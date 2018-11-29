package lspace.librarian.provider.mem

import java.util.concurrent.atomic.AtomicLong

import lspace.NS
import lspace.librarian.structure.Property.default
import lspace.librarian.structure._

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Try

object MemResource {}

trait MemResource[T] extends Resource[T] {
  implicit def graph: Graph
//  @transient lazy val id: Long = graph.idGenerator.next

  override def iri: String =
    linksOut
      .get(default.`@id`)
      .flatMap(_.headOption)
      .map(_.inV.value.asInstanceOf[String])
      .getOrElse("")

  override def iris: Set[String] =
    linksOut.get(default.`@id`).map(_.map(_.inV.value.asInstanceOf[String]).toSet).getOrElse(Set()) ++
      linksOut.get(default.`@ids`).map(_.map(_.inV.value.asInstanceOf[String]).toSet).getOrElse(Set())

  protected[librarian] val linksOut
    : mutable.OpenHashMap[Property, mutable.LinkedHashSet[Edge[T, _]]] = //what about mutable.LinkedHashMap[Property, mutable.LinkedHashSet[Edge[T, _]]]
    mutable.OpenHashMap[Property, mutable.LinkedHashSet[Edge[T, _]]]()

  protected[librarian] val linksIn: mutable.OpenHashMap[Property, mutable.LinkedHashSet[Edge[_, T]]] =
    mutable.OpenHashMap[Property, mutable.LinkedHashSet[Edge[_, T]]]()

  //  override def keys: Set[Property] = linksOut.keySet ++ linksIn.keySet toSet

  def out(key: Property*): List[Any] =
    if (key.nonEmpty) key.flatMap(key => linksOut.getOrElse(key, List())).map(_.inV.value).toList
    else linksOut.values.flatten.map(_.inV.value).toList

  def outMap(key: Property*): Map[Property, List[Any]] = {
    if (key.isEmpty) linksOut.toMap.mapValues(_.toList.map(_.inV.value))
    else outE(key.toList: _*).groupBy(_.key).mapValues(_.map(_.inV.value))
  }

  def outE(key: Property*): List[Edge[T, Any]] =
    if (key.nonEmpty) key.flatMap(key => linksOut.getOrElse(key, List())).toList
    else linksOut.values.flatten.toList

  def outEMap(key: Property*): Map[Property, List[Edge[T, Any]]] = {
    if (key.isEmpty) linksOut.toMap.mapValues(_.toList)
    else outE(key.toList: _*).groupBy(_.key)
  }

  def in(key: Property*): List[Any] =
    if (key.nonEmpty) key.flatMap(key => linksIn.getOrElse(key, List())).map(_.outV.value).toList
    else linksIn.values.flatten.map(_.outV.value).toList

  def inMap(key: Property*): Map[Property, List[Any]] = {
    if (key.isEmpty) linksIn.toMap.mapValues(_.toList.map(_.outV.value))
    else inE(key.toList: _*).groupBy(_.key).mapValues(_.map(_.outV.value))
  }

  def inE(key: Property*): List[Edge[Any, T]] =
    if (key.nonEmpty) key.flatMap(key => linksIn.getOrElse(key, List())).toList
    else linksIn.values.flatten.toList

  def inEMap(key: Property*): Map[Property, List[Edge[Any, T]]] = {
    if (key.isEmpty) linksIn.toMap.mapValues(_.toList)
    else inE(key.toList: _*).groupBy(_.key)
  }

  private def validateDT[V](dt: DataType[V], value: V) =
    if (dt.iri.nonEmpty) dt else ClassType.valueToOntologyResource(value)

  def removeInE(edge: Edge[_, _]): Unit = {
    val properties         = linksIn.getOrElse(edge.key, List())
    val (toRemove, toKeep) = properties.partition(_ == edge)
    linksIn += edge.key -> (mutable.LinkedHashSet[Edge[_, T]]() ++ toKeep)
    toRemove.foreach(_.remove())
  }
  def removeOutE(edge: Edge[_, _]): Unit = {
    val properties         = linksOut.getOrElse(edge.key, List())
    val (toRemove, toKeep) = properties.partition(_ == edge)
    linksOut += edge.key -> (mutable.LinkedHashSet[Edge[T, _]]() ++ toKeep)
    toRemove.foreach(_.remove())
  }
  def removeInE(key: Property): Unit = {
    val toRemove = inE(key)
    linksIn -= key
    toRemove.foreach(_.remove())
  }
  def removeOutE(key: Property): Unit = {
    val toRemove = outE(key)
    linksOut -= key
    toRemove.foreach(_.remove())
  }
}
