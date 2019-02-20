package lspace.lgraph

import java.time.Instant

import lspace.lgraph.LResource.LinksSet
import lspace.structure.{Edge, Property, Resource}
import monix.execution.atomic.AtomicLong

import scala.collection.mutable

object LResource {
  case class LinksSet[S, E](lastsync: Option[Long] = None, links: List[Edge[S, E]] = List[Edge[S, E]]())

  private[this] val getLastAccessStampLock = new Object
  private[this] val lastaccessStamp        = AtomicLong(Instant.now().getEpochSecond)

  def getLastAccessStamp() = getLastAccessStampLock.synchronized {
    val lastAccess = lastaccessStamp.get()
    val now        = Instant.now().getEpochSecond
    if (lastAccess != now) lastaccessStamp.addAndGet(now) else lastaccessStamp.get()
  }
}

trait LResource[T] extends Resource[T] {
  val graph: LGraph

  protected[lgraph] var _lastused: Long            = LResource.getLastAccessStamp()
  protected[lgraph] var _lastoutsync: Option[Long] = None
  private val linksOut: mutable.OpenHashMap[Property, LinksSet[T, _]] =
    mutable.OpenHashMap[Property, LinksSet[T, _]]()
  private val linksOutWriteLock = new Object

  def _addOut(edge: Edge[T, _]): Unit = linksOutWriteLock.synchronized {
    val time = LResource.getLastAccessStamp()
    _lastused = LResource.getLastAccessStamp()
    val linksset = linksOut
      .getOrElse(edge.key, LResource.LinksSet[T, Any]())
    if (_lastoutsync.exists(_ + 120 > time)) {
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

  protected[lgraph] var _lastinsync: Option[Long] = None
  private val linksIn: mutable.OpenHashMap[Property, LinksSet[_, T]] =
    mutable.OpenHashMap[Property, LinksSet[_, T]]()
  private val linksInWriteLock = new Object

  def _addIn(edge: Edge[_, T]): Unit = linksInWriteLock.synchronized {
    val time = LResource.getLastAccessStamp()
    _lastused = LResource.getLastAccessStamp()
    val linksset = linksIn
      .getOrElse(edge.key, LResource.LinksSet[Any, T]())
    if (_lastinsync.exists(_ + 120 > time)) {
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

  private def _outE(key: Property*): List[Edge[T, Any]] = {
    _lastused = LResource.getLastAccessStamp()
    if (key.nonEmpty)
      key
//        .flatMap(key => key :: graph.ns.getExtendedByProperties(key))
//        .distinct
        .flatMap { key =>
          val linksSet = linksOut.getOrElse(key, LinksSet())
          if (linksSet.lastsync.isDefined) linksSet.links.asInstanceOf[List[Edge[T, Any]]]
          else {
            val edges = (linksSet.links.asInstanceOf[List[Edge[T, Any]]] ++ graph.storeManager
              .edgesByFromIdAndKey(id, key)
              .toList
              .asInstanceOf[List[Edge[T, Any]]]).distinct
            linksOut += key -> LinksSet[T, Any](Some(_lastused), edges)
            edges
          }
        }
        .toList
        .asInstanceOf[List[Edge[T, Any]]]
    else {
      if (_lastoutsync.isDefined)
        linksOut.flatMap(_._2.links.asInstanceOf[List[Edge[T, Any]]]).toList
      else {
        _lastoutsync = Some(_lastused)
        val edges = (linksOut
          .flatMap(_._2.links.asInstanceOf[List[Edge[T, Any]]])
          .toList ++ graph.storeManager.edgesByFromId(id).toList.asInstanceOf[List[Edge[T, Any]]]).distinct
        edges.groupBy(_.key).foreach {
          case (key, edges) =>
            linksOut += key -> LinksSet[T, Any](Some(_lastused), edges)
        }
        edges
      }
    }
  }

  override def keys: Set[Property] =
    linksOut.keySet ++ linksIn.keySet toSet //TODO: this returns only from cached edges, query LStore

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

  private def _inE(key: Property*): List[Edge[Any, T]] = {
    _lastused = LResource.getLastAccessStamp()
    if (key.nonEmpty)
      key
      //        .flatMap(key => key :: graph.ns.getExtendedByProperties(key))
      //        .distinct
      .flatMap { key =>
        val linksSet = linksIn.getOrElse(key, LinksSet())
        if (linksSet.lastsync.isDefined) linksSet.links.asInstanceOf[List[Edge[Any, T]]]
        else {
          val edges = (linksSet.links.asInstanceOf[List[Edge[Any, T]]] ++ graph.storeManager
            .edgesByToIdAndKey(id, key)
            .toList
            .asInstanceOf[List[Edge[Any, T]]]).distinct
          linksIn += key -> LinksSet[Any, T](Some(_lastused), List(edges: _*))
          edges
        }
      }.toList
    else {
      if (_lastinsync.isDefined)
        linksIn.flatMap(_._2.links.asInstanceOf[List[Edge[Any, T]]]).toList
      else {
        _lastinsync = Some(_lastused)
        val edges = (linksIn
          .flatMap(_._2.links.asInstanceOf[List[Edge[Any, T]]])
          .toList ++ graph.storeManager.edgesByToId(id).toList.asInstanceOf[List[Edge[Any, T]]]).distinct
        edges.groupBy(_.key).foreach {
          case (key, edges) =>
            linksIn += key -> LinksSet[Any, T](Some(_lastused), edges)
        }
        edges
      }
    }
  }

  def in(key: Property*): List[Any] =
    _inE(key: _*)
      .map(_.from.value)

  def inMap(key: Property*): Map[Property, List[Any]] =
    _inE(key.toList: _*).groupBy(_.key).mapValues(_.map(_.from.value))

  def inE(key: Property*): List[Edge[Any, T]] = _inE(key: _*)

  def inEMap(key: Property*): Map[Property, List[Edge[Any, T]]] = _inE(key.toList: _*).groupBy(_.key)

  def removeIn[V >: T](edge: Edge[_, V]): Unit = synchronized {
    linksIn.get(edge.key).foreach { linksset =>
      linksset.links.asInstanceOf[List[Edge[Any, T]]].filterNot(_ == edge.asInstanceOf[Edge[Any, T]]) match {
        case set if set.isEmpty =>
          linksIn -= edge.key
        case set =>
          linksIn += edge.key -> LinksSet[Any, T](linksset.lastsync, set)
      }
    }
  }
  def removeOut[V >: T](edge: Edge[V, _]): Unit = synchronized {
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
}
