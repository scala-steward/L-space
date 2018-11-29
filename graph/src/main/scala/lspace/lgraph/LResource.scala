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
    if (key.nonEmpty) key.flatMap { key =>
      val linksSet = linksOut.getOrElse(key, LinksSet())
      if (linksSet.lastsync.isDefined) linksSet.links.toList
      else {
        val edges = graph.storeManager.edgesByFromIdAndKey(id, key).toList.asInstanceOf[List[Edge[T, Any]]]
        linksOut += key -> LinksSet[T, Any](Some(Instant.now), mutable.LinkedHashSet(edges: _*))
        edges
      }
    }.toList
    else {
      if (_lastoutsync.isDefined) linksOut.flatMap(_._2.links.toList).toList
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
      if (linksSet.lastsync.isDefined) linksSet.links.toList
      else {
        val edges = graph.storeManager.edgesByToIdAndKey(id, key).toList.asInstanceOf[List[Edge[Any, T]]]
        linksIn += key -> LinksSet[Any, T](Some(Instant.now), mutable.LinkedHashSet(edges: _*))
        edges
      }
    }.toList
    else {
      if (_lastoutsync.isDefined) linksIn.flatMap(_._2.links.toList).toList
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

  def removeInE(edge: Edge[_, _]): Unit  = {}
  def removeOutE(edge: Edge[_, _]): Unit = {}
  def removeInE(key: Property): Unit     = {}
  def removeOutE(key: Property): Unit    = {}

  protected def _remove(): Unit = {}
}
