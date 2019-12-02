package lspace.lgraph

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap

import lspace.lgraph.LResource.LinksSet
import lspace.structure.{Edge, Property, Resource}
import monix.eval.Task
import monix.execution.atomic.AtomicLong

import scala.collection.{concurrent, mutable}
import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

object LResource {
  case class LinksSet[S, E](lastsync: Option[Long] = None, links: HashSet[Edge[S, E]] = HashSet[Edge[S, E]]())

  private[this] val getLastAccessStampLock = new Object
  private[this] val lastaccessStamp        = AtomicLong(Instant.now().getEpochSecond)

  /**
    * This method is used to create timestamps with fixed intervals and reuses equal values.
    * @return
    */
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
  private val linksOut: concurrent.Map[Property, LinksSet[T, _]] =
    new ConcurrentHashMap[Property, LinksSet[T, _]](16, 0.9f, 32).asScala
  private val linksOutWriteLock = new Object

  def _addOut(edge: Edge[T, _]): Unit = {
    linksOutWriteLock.synchronized {
      val time = LResource.getLastAccessStamp()
      _lastused = LResource.getLastAccessStamp()
      val linksset = linksOut
        .getOrElse(edge.key, LResource.LinksSet[T, Any]())
      if (_lastoutsync.exists(_ + 120 > time)) {
        linksOut += edge.key -> LinksSet[T, Any](
          lastsync = Some(time),
          links = linksset.links
            .asInstanceOf[HashSet[Edge[T, Any]]] + edge.asInstanceOf[Edge[T, Any]]
        )
      } else
        linksOut += edge.key -> LinksSet[T, Any](
          links = linksset.links
            .asInstanceOf[HashSet[Edge[T, Any]]] + edge.asInstanceOf[Edge[T, Any]]
        )
    }
  }

  protected[lgraph] var _lastinsync: Option[Long] = None
  private val linksIn: concurrent.Map[Property, LinksSet[_, T]] =
    new ConcurrentHashMap[Property, LinksSet[_, T]](2, 0.9f, 4).asScala
  private val linksInWriteLock = new Object

  def _addIn(edge: Edge[_, T]): Unit = {
    linksInWriteLock.synchronized {
      val time = LResource.getLastAccessStamp()
      _lastused = LResource.getLastAccessStamp()
      val linksset = linksIn
        .getOrElse(edge.key, LResource.LinksSet[Any, T]())
      if (_lastinsync.exists(_ + 120 > time)) {
        linksIn += edge.key -> LinksSet[Any, T](
          lastsync = Some(time),
          links = linksset.links
            .asInstanceOf[HashSet[Edge[Any, T]]] + edge.asInstanceOf[Edge[Any, T]]
        )
      } else
        linksIn += edge.key -> LinksSet[Any, T](
          links = linksset.links
            .asInstanceOf[HashSet[Edge[Any, T]]] + edge.asInstanceOf[Edge[Any, T]]
        )
    }
  }

  /*private def _outE(key: Property*): List[Edge[T, Any]] = {
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
  }*/

  override def keys: Set[Property] =
    linksOut.keySet ++ linksIn.keySet toSet //TODO: this returns only from cached edges, query LStore

  def out(key: Property*): List[Any] =
    if (key.nonEmpty)
      (key.toSet ++ key.toList.flatMap(_.extendedBy.all())).toList
        .flatMap(key => linksOut.get(key).toList.flatMap(_.links.toList))
        .map(_.to.value)
    else linksOut.values.flatMap(_.links.toList).map(_.to.value).toList
//    _outE(key: _*)
//      .map(_.to.value)

  def outMap(key: Property*): Map[Property, List[Any]] = {
    if (key.isEmpty) linksOut.toMap.mapValues(_.links.map(_.to.value).toList).toMap
    else
      linksOut.toMap
        .filterKeys((key.toSet ++ key.toList.flatMap(_.extendedBy.all())).contains(_))
        .mapValues(_.links.map(_.to.value).toList)
        .toMap
//    _outE(key.toList: _*).groupBy(_.key).mapValues(_.map(_.to.value))
  }

  def outE(key: Property*): List[Edge[T, Any]] = {
    if (key.nonEmpty)
      (key.toSet ++ key.toList.flatMap(_.extendedBy.all())).toList
        .flatMap(key => linksOut.get(key).toList.flatMap(_.links.toList))
        .asInstanceOf[List[Edge[T, Any]]]
    else linksOut.values.toList.flatMap(_.links.toList).asInstanceOf[List[Edge[T, Any]]]
//    _outE(key: _*)
  }

  def outEMap(key: Property*): Map[Property, List[Edge[T, Any]]] = {
    if (key.isEmpty) linksOut.toMap.mapValues(_.links.toList).toMap
    else
      linksOut.toMap
        .filterKeys((key.toSet ++ key.toList.flatMap(_.extendedBy.all())).contains(_))
        .mapValues(_.links.toList)
        .toMap
//    _outE(key.toList: _*).groupBy(_.key)
  }

  /*private def _inE(key: Property*): List[Edge[Any, T]] = {
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
  }*/

  def in(key: Property*): List[Any] =
    if (key.nonEmpty)
      (key.toSet ++ key.toList.flatMap(_.extendedBy.all())).toList
        .flatMap(key => linksIn.get(key).toList.flatMap(_.links.toList))
        .map(_.from.value)
    else linksIn.values.flatMap(_.links.toList).map(_.from.value).toList
//    _inE(key: _*)
//      .map(_.from.value)

  def inMap(key: Property*): Map[Property, List[Any]] =
    if (key.isEmpty) linksIn.toMap.mapValues(_.links.map(_.from.value).toList).toMap
    else
      linksIn.toMap
        .filterKeys((key.toSet ++ key.toList.flatMap(_.extendedBy.all())).contains(_))
        .mapValues(_.links.map(_.from.value).toList)
        .toMap
//    _inE(key.toList: _*).groupBy(_.key).mapValues(_.map(_.from.value))

  def inE(key: Property*): List[Edge[Any, T]] = {
    if (key.nonEmpty)
      (key.toSet ++ key.toList.flatMap(_.extendedBy.all())).toList
        .flatMap(key => linksIn.get(key).toList.flatMap(_.links.toList))
        .asInstanceOf[List[Edge[Any, T]]]
    else linksIn.values.toList.flatMap(_.links.toList).asInstanceOf[List[Edge[Any, T]]]
//    _inE(key: _*)
  }

  def inEMap(key: Property*): Map[Property, List[Edge[Any, T]]] = {
    if (key.isEmpty) linksIn.toMap.mapValues(_.links.toList).toMap
    else
      linksIn.toMap
        .filterKeys((key.toSet ++ key.toList.flatMap(_.extendedBy.all())).contains(_))
        .mapValues(_.links.toList)
        .toMap
//    _inE(key.toList: _*).groupBy(_.key)
  }

  def removeIn[V >: T](edge: Edge[_, V]): Task[Unit] = Task {
    synchronized {
      linksIn.get(edge.key).foreach { linksset =>
        linksset.links.asInstanceOf[HashSet[Edge[Any, T]]] - edge.asInstanceOf[Edge[Any, T]] match {
          case set if set.isEmpty =>
            linksIn -= edge.key
          case set =>
            linksIn += edge.key -> LinksSet[Any, T](linksset.lastsync, set)
        }
      }
    }
  }
  def removeOut[V >: T](edge: Edge[V, _]): Task[Unit] = Task {
    synchronized {
      linksOut.get(edge.key).foreach { linksset =>
        linksset.links.asInstanceOf[HashSet[Edge[T, Any]]] - edge.asInstanceOf[Edge[T, Any]] match {
          case set if set.isEmpty =>
            linksOut -= edge.key
          case set =>
            linksOut += edge.key -> LinksSet[T, Any](linksset.lastsync, set)
        }
      }
    }
  }
  def removeIn(key: Property): Task[Unit] = Task {
    synchronized {
      val toRemove = inE(key)
      linksIn -= key
      toRemove.foreach(_.remove())
    }
  }
  def removeOut(key: Property): Task[Unit] = Task {
    synchronized {
      val toRemove = outE(key)
      linksOut -= key
      toRemove.foreach(_.remove())
    }
  }
}
