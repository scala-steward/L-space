package lspace.lgraph.store

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap

import lspace.lgraph.LGraph
import lspace.structure.store.Store
import monix.eval.Task
import monix.reactive.Observable

import scala.collection.concurrent
import scala.jdk.CollectionConverters._

object LStore {
//  def apply[T <: Resource[_]](iri: String, graph: LGraph): LStore[T] =
//    new LStore[T](iri, graph)
  implicit val ec = monix.execution.Scheduler.io("lstore-io")
}

trait LStore[G <: LGraph] extends Store[G] {

  protected[store] lazy val _cache: concurrent.Map[Long, T2] =
    new ConcurrentHashMap[Long, T2]().asScala
  protected[store] lazy val _cacheByIri: concurrent.Map[String, Set[T2]] =
    new ConcurrentHashMap[String, Set[T2]]().asScala
  protected[store] lazy val _deleted: concurrent.Map[Long, Instant] =
    new ConcurrentHashMap[Long, Instant]().asScala

  def store(resource: T): Task[Unit] = Task {
    cache(resource)
  }

  def isDeleted(id: Long) = _deleted.contains(id)
  def markDeleted(ids: Set[Long]): Unit = {
    val delTime = Instant.now()
    _deleted ++= ids.map(_ -> delTime)
  }
  def dropDeletedMarks(seconds: Int): Unit = {
    val now = Instant.now()
    _deleted --= _deleted.filter(_._2.plusSeconds(seconds).isBefore(now)).keys
  }

  def cachedById(id: Long)     = if (!isDeleted(id)) _cache.get(id) else None
  def cachedByIri(iri: String) = _cacheByIri.get(iri).toList.flatten.filterNot(r => isDeleted(r.id)).to(LazyList)
  def countids                 = _cache.size
  def countiris                = _cacheByIri.size

  def cache(resource: T): Unit = {
    cacheById(resource)
    cacheByIri(resource)
  }

  def cache(resources: List[T]): Unit = {
    cacheById(resources)
    cacheByIri(resources)
  }

  def cacheById(resource: T): Unit = {
    //    resource.status = CacheStatus.CACHED
    _cache += resource.id -> resource.asInstanceOf[T2]
    if (_cache.get(resource.id).isEmpty) throw new Exception(s"id ${resource.id} cached but not retrievable?")
  }
  def cacheById(resources: List[T]): Unit = {
    //    resource.status = CacheStatus.CACHED
    _cache ++= resources.map(resource => resource.id -> resource.asInstanceOf[T2])
  }

  private[this] val byIriLock = new Object

  def cacheByIri(resource: T): Unit = byIriLock.synchronized {
    if (resource.iri.nonEmpty)
      _cacheByIri += resource.iri -> (_cacheByIri.getOrElse(resource.iri, Set()) + resource.asInstanceOf[T2])
    resource.iris.foreach(iri => _cacheByIri += iri -> (_cacheByIri.getOrElse(iri, Set()) + resource.asInstanceOf[T2]))
  }

  def cacheByIri(resources: List[T]): Unit = byIriLock.synchronized {
    _cacheByIri ++= resources.map(resource =>
      resource.iri -> (_cacheByIri.getOrElse(resource.iri, Set()) + resource.asInstanceOf[T2]))
    _cacheByIri ++= resources.foldLeft(Map[String, Set[T2]]()) {
      case (result, resource) =>
        result ++ resource.iris.map(iri =>
          iri -> (_cacheByIri.getOrElse(iri, Set()) ++ result.getOrElse(iri, Set()) + resource.asInstanceOf[T2]))
    }
  }

  def uncache(resource: T): Unit = {
    uncacheById(resource)
    uncacheByIri(resource)
    resource.outE().foreach(e => graph.edgeStore.uncache(e.asInstanceOf[graph.edgeStore.T]))
    resource.inE().foreach(e => graph.edgeStore.uncache(e.asInstanceOf[graph.edgeStore.T]))
  }
  def uncacheById(resource: T): Unit = {
    _cache -= resource.id
  }
  def uncacheByIri(resource: T): Unit = {
    if (resource.iri.nonEmpty) _cacheByIri.getOrElse(resource.iri, Set()) - resource.asInstanceOf[T2] match {
      case set if set.isEmpty => _cacheByIri -= resource.iri
      case set                => _cacheByIri += resource.iri -> set
    }
    resource.iris.foreach(iri =>
      _cacheByIri.getOrElse(iri, Set()) - resource.asInstanceOf[T2] match {
        case set if set.isEmpty => _cacheByIri -= iri
        case set                => _cacheByIri += iri -> set
    })
  }

  def hasId(id: Long): Task[Option[T2]]   = Task { cachedById(id) }
  def hasIri(iri: String): Observable[T2] = Observable.fromIterable(cachedByIri(iri))

  def delete(resource: T): Task[Unit] = Task {
    uncache(resource)
  }

  def purge: Task[Unit] = Task.delay {
    _cache.clear()
    _cacheByIri.clear()
    _deleted.clear()
  }

  def all(): Observable[T2]

  def cached: Cached = new Cached {
    def all(): LazyList[T2]           = _cache.values.to(LazyList)
    def hasId(id: Long): Option[T2] = _cache.get(id)
    def count: Long                 = _cache.size
  }
}
