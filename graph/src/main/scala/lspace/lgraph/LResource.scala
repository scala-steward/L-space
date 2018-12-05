package lspace.lgraph

import java.time.Instant

import lspace.lgraph.LResource.LinksSet
import lspace.librarian.structure.{Edge, Property, Resource}

import scala.collection.mutable

object LResource {
  case class LinksSet[S, E](lastsync: Option[Instant] = None, links: List[Edge[S, E]] = List[Edge[S, E]]())
}

trait LResource[T] extends Resource[T] {
  def graph: LGraph

  protected[lgraph] var _lastoutsync: Option[Instant] = None
  private val linksOut: mutable.OpenHashMap[Property, LinksSet[T, _]] =
    mutable.OpenHashMap[Property, LinksSet[T, _]]()

  def _addOut(edge: Edge[T, _]): Unit = synchronized {
    val time = Instant.now()
    val linksset = linksOut
      .getOrElse(edge.key, LResource.LinksSet[T, Any]())
    if (_lastoutsync
          .exists(_.isAfter(time.minusSeconds(120)))) {
      linksOut += edge.key -> LinksSet[T, Any](lastsync = Some(time),
                                               links = (linksset.links
                                                 .asInstanceOf[List[Edge[T, Any]]] :+ edge.asInstanceOf[Edge[T, Any]])
                                                 .sortBy(_.id)
                                                 .reverse
                                                 .distinct
                                                 .reverse)
    } else
      linksOut += edge.key -> LinksSet[T, Any](
        links = (linksset.links
          .asInstanceOf[List[Edge[T, Any]]] :+ edge.asInstanceOf[Edge[T, Any]])
          .sortBy(_.id)
          .reverse
          .distinct
          .reverse)
  }

  protected[lgraph] var _lastinsync: Option[Instant] = None
  private val linksIn: mutable.OpenHashMap[Property, LinksSet[_, T]] =
    mutable.OpenHashMap[Property, LinksSet[_, T]]()

  def _addIn(edge: Edge[_, T]): Unit = synchronized {
    val time = Instant.now()
    val linksset = linksIn
      .getOrElse(edge.key, LResource.LinksSet[Any, T]())
    if (_lastinsync
          .exists(_.isAfter(time.minusSeconds(120)))) {
      linksIn += edge.key -> LinksSet[Any, T](
        lastsync = Some(time),
        links = (edge.asInstanceOf[Edge[Any, T]] :: linksset.links
          .asInstanceOf[List[Edge[Any, T]]]).distinct
          .sortBy(_.id)
          .reverse
      )
    } else
      linksIn += edge.key -> LinksSet[Any, T](
        links = (edge.asInstanceOf[Edge[Any, T]] :: linksset.links
          .asInstanceOf[List[Edge[Any, T]]]).distinct
          .sortBy(_.id)
          .reverse)
  }

  private def _outE(key: Property*): List[Edge[T, Any]] =
    if (key.nonEmpty)
      key
        .flatMap { key =>
          val linksSet = linksOut.getOrElse(key, LinksSet())
          if (linksSet.lastsync.isDefined) linksSet.links.asInstanceOf[List[Edge[T, Any]]]
          else {
            val edges = graph.storeManager.edgesByFromIdAndKey(id, key).toList.asInstanceOf[List[Edge[T, Any]]]
            linksOut += key -> LinksSet[T, Any](Some(Instant.now), List(edges: _*))
            edges
          }
        }
        .toList
        .asInstanceOf[List[Edge[T, Any]]]
    else {
      if (_lastoutsync.isDefined)
        linksOut.flatMap(_._2.links.asInstanceOf[List[Edge[T, Any]]]).toList
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
      if (linksSet.lastsync.isDefined) linksSet.links.asInstanceOf[List[Edge[Any, T]]]
      else {
        val edges = graph.storeManager.edgesByToIdAndKey(id, key).toList.asInstanceOf[List[Edge[Any, T]]]
        linksIn += key -> LinksSet[Any, T](Some(Instant.now), List(edges: _*))
        edges
      }
    }.toList
    else {
      if (_lastoutsync.isDefined)
        linksIn.flatMap(_._2.links.asInstanceOf[List[Edge[Any, T]]]).toList
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

  def removeIn(edge: Edge[_, T]): Unit = synchronized {
    linksIn.get(edge.key).foreach { linksset =>
      linksset.links.asInstanceOf[List[Edge[Any, T]]].filterNot(_ == edge.asInstanceOf[Edge[Any, T]]) match {
        case set if set.isEmpty =>
          linksIn -= edge.key
        case set =>
          linksIn += edge.key -> LinksSet[Any, T](linksset.lastsync, set)
      }
    }
  }
  def removeOut(edge: Edge[T, _]): Unit = synchronized {
    linksOut.get(edge.key).foreach { linksset =>
      linksset.links.asInstanceOf[List[Edge[T, Any]]].filterNot(_ == edge.asInstanceOf[Edge[T, Any]]) match {
        case set if set.isEmpty =>
          linksOut -= edge.key
        case set =>
          linksOut += edge.key -> LinksSet[T, Any](linksset.lastsync, set)
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

  protected def _remove(): Unit = {}
}
