package lspace.lgraph

import java.time.Instant

import lspace.lgraph.LResource.LinksSet
import lspace.librarian.structure.{Edge, Property, Resource}

import scala.collection.mutable

object LResource {
  case class LinksSet[S, E](lastsync: Option[Instant] = None,
                            links: mutable.LinkedHashSet[Edge[S, E]] = new mutable.LinkedHashSet[Edge[S, E]]())
}

trait LResource[T] extends Resource[T] {
  def graph: LGraph

  protected[lgraph] var _lastoutsync: Option[Instant] = None
  val linksOut: mutable.OpenHashMap[Property, LinksSet[T, _]] =
    mutable.OpenHashMap[Property, LinksSet[T, _]]()

  protected[lgraph] var _lastinsync: Option[Instant] = None
  val linksIn: mutable.OpenHashMap[Property, LinksSet[_, T]] =
    mutable.OpenHashMap[Property, LinksSet[_, T]]()

  private def _outE(key: Property*): List[Edge[T, Any]] =
    if (key.nonEmpty)
      key
        .flatMap { key =>
          val linksSet = linksOut.getOrElse(key, LinksSet())
          if (linksSet.lastsync.isDefined) linksSet.links.toList.asInstanceOf[List[Edge[T, Any]]]
          else {
            val edges = graph.storeManager.edgesByFromIdAndKey(id, key).toList.asInstanceOf[List[Edge[T, Any]]]
            linksOut += key -> LinksSet[T, Any](Some(Instant.now), mutable.LinkedHashSet(edges: _*))
            edges
          }
        }
        .toList
        .asInstanceOf[List[Edge[T, Any]]]
    else {
      if (_lastoutsync.isDefined)
        linksOut.flatMap(_._2.links.toList.asInstanceOf[List[Edge[T, Any]]]).toList.asInstanceOf[List[Edge[T, Any]]]
      else {
        _lastoutsync = Some(Instant.now())
        graph.storeManager.edgesByFromId(id).toList.asInstanceOf[List[Edge[T, Any]]]
      }
    }

  def out(key: Property*): List[Any] =
    _outE(key: _*)
      .map(_.to.value)

  def outMap(key: Property*): Map[Property, List[Any]] = {
    _outE(key.toList: _*).groupBy(_.key).mapValues(_.map(_.to.value))
  }

  def outE(key: Property*): List[Edge[T, Any]] = _outE(key: _*)

  def outEMap(key: Property*): Map[Property, List[Edge[T, Any]]] = {
    _outE(key.toList: _*).groupBy(_.key)
  }

  private def _inE(key: Property*): List[Edge[Any, T]] =
    if (key.nonEmpty) key.flatMap { key =>
      val linksSet = linksIn.getOrElse(key, LinksSet())
      if (linksSet.lastsync.isDefined) linksSet.links.toList.asInstanceOf[List[Edge[Any, T]]]
      else {
        val edges = graph.storeManager.edgesByToIdAndKey(id, key).toList.asInstanceOf[List[Edge[Any, T]]]
        linksIn += key -> LinksSet[Any, T](Some(Instant.now), mutable.LinkedHashSet(edges: _*))
        edges
      }
    }.toList
    else {
      if (_lastoutsync.isDefined)
        linksIn.flatMap(_._2.links.toList.asInstanceOf[List[Edge[Any, T]]]).toList.asInstanceOf[List[Edge[Any, T]]]
      else {
        _lastoutsync = Some(Instant.now())
        graph.storeManager.edgesByToId(id).toList.asInstanceOf[List[Edge[Any, T]]]
      }
    }

  def in(key: Property*): List[Any] =
    _inE(key: _*)
      .map(_.to.value)

  def inMap(key: Property*): Map[Property, List[Any]] = _inE(key.toList: _*).groupBy(_.key).mapValues(_.map(_.to.value))

  def inE(key: Property*): List[Edge[Any, T]] = _inE(key: _*)

  def inEMap(key: Property*): Map[Property, List[Edge[Any, T]]] = _inE(key.toList: _*).groupBy(_.key)

  def removeInE(edge: Edge[_, T]): Unit = {
    linksIn.get(edge.key).foreach { linksset =>
      if (linksset.links.asInstanceOf[mutable.LinkedHashSet[Edge[_, T]]].contains(edge)) {
        val newSet = linksset.links.asInstanceOf[mutable.LinkedHashSet[Edge[Any, T]]] - edge.asInstanceOf[Edge[Any, T]]
        if (newSet.isEmpty) linksIn -= edge.key
        else linksIn += edge.key -> linksset.asInstanceOf[LinksSet[Any, T]].copy(links = newSet)
        edge.remove()
      }
    }
  }
  def removeOutE(edge: Edge[T, _]): Unit = {
    linksOut.get(edge.key).foreach { linksset =>
      if (linksset.links.asInstanceOf[mutable.LinkedHashSet[Edge[T, _]]].contains(edge)) {
        val newSet = linksset.links.asInstanceOf[mutable.LinkedHashSet[Edge[T, Any]]] - edge.asInstanceOf[Edge[T, Any]]
        if (newSet.isEmpty) linksOut -= edge.key
        else linksOut += edge.key -> linksset.asInstanceOf[LinksSet[T, Any]].copy(links = newSet)
        edge.remove()
      }
    }
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

  protected def _remove(): Unit = {}
}
