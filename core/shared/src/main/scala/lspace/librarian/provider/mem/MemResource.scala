package lspace.librarian.provider.mem

import lspace.librarian.structure.Property.default
import lspace.librarian.structure._

import scala.collection.mutable

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

  private val linksOut
    : mutable.OpenHashMap[Property, List[Edge[T, _]]] = //what about mutable.LinkedHashMap[Property, mutable.LinkedHashSet[Edge[T, _]]]
    mutable.OpenHashMap[Property, List[Edge[T, _]]]()

  protected[librarian] def _addOut(edge: Edge[T, _]): Unit = synchronized {
    if (edge.from != this) throw new Exception("edge.from != this, cannot add out-link")
    linksOut += edge.key -> (edge :: linksOut
      .getOrElse(edge.key, List[Edge[T, _]]())
      .reverse).distinct.reverse
  }

  private val linksIn: mutable.OpenHashMap[Property, List[Edge[_, T]]] =
    mutable.OpenHashMap[Property, List[Edge[_, T]]]()

  protected[librarian] def _addIn(edge: Edge[_, T]): Unit = synchronized {
    if (edge.to != this) throw new Exception("edge.from != this, cannot add in-link")
    linksIn += edge.key -> (edge :: linksIn
      .getOrElse(edge.key, List[Edge[_, T]]())
      .reverse).distinct.reverse
  }

  //  override def keys: Set[Property] = linksOut.keySet ++ linksIn.keySet toSet

  def out(key: Property*): List[Any] =
    if (key.nonEmpty) key.toList.flatMap(key => linksOut.get(key).toList.flatten).map(_.inV.value)
    else linksOut.values.flatten.map(_.inV.value).toList

  def outMap(key: Property*): Map[Property, List[Any]] = {
    if (key.isEmpty) linksOut.toMap.mapValues(_.map(_.inV.value).toList)
    else outE(key: _*).groupBy(_.key).mapValues(_.map(_.inV.value))
  }

  def outE(key: Property*): List[Edge[T, Any]] =
    if (key.nonEmpty) key.toList.flatMap(key => linksOut.get(key).toList.flatten).asInstanceOf[List[Edge[T, Any]]]
    else linksOut.values.toList.flatten.asInstanceOf[List[Edge[T, Any]]]

  def outEMap(key: Property*): Map[Property, List[Edge[T, Any]]] = {
    if (key.isEmpty) linksOut.toMap.mapValues(_.asInstanceOf[List[Edge[T, Any]]])
    else outE(key: _*).groupBy(_.key)
  }

  def in(key: Property*): List[Any] =
    if (key.nonEmpty) key.toList.flatMap(key => linksIn.get(key).toVector.flatten).map(_.outV.value)
    else linksIn.values.toList.flatten.map(_.outV.value)

  def inMap(key: Property*): Map[Property, List[Any]] = {
    if (key.isEmpty) linksIn.toMap.mapValues(_.map(_.outV.value).toList)
    else inE(key: _*).groupBy(_.key).mapValues(_.map(_.outV.value))
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

  def removeIn(edge: Edge[_, T]): Unit = synchronized {
    linksIn.get(edge.key).foreach { links =>
      if (links.contains(edge)) {
        val newSet = links.filterNot(_ == edge)
        if (newSet.isEmpty) linksIn -= edge.key
        else linksIn += edge.key -> newSet
        edge.remove()
      }
    }
  }
  def removeOut(edge: Edge[T, _]): Unit = synchronized {
    linksOut.get(edge.key).foreach { links =>
      if (links.contains(edge)) {
        val newSet = links.filterNot(_ == edge)
        if (newSet.isEmpty) linksOut -= edge.key
        else linksOut += edge.key -> newSet
        edge.remove()
      }
    }
  }
  def removeIn(key: Property): Unit = synchronized {
    val toRemove = inE(key)
    linksIn -= key
    toRemove.foreach(_.remove())
  }
  def removeOut(key: Property): Unit = synchronized {
    val toRemove = outE(key)
    linksOut -= key
    toRemove.foreach(_.remove())
  }
}
