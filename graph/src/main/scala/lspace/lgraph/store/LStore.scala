package lspace.lgraph.store

import lspace.lgraph.LGraph
import lspace.librarian.structure.store.Store

import scala.collection.mutable

object LStore {
//  def apply[T <: Resource[_]](iri: String, graph: LGraph): LStore[T] =
//    new LStore[T](iri, graph)
}

trait LStore[G <: LGraph] extends Store[G] {

  protected[store] lazy val _cache: mutable.OpenHashMap[Long, T] =
    mutable.OpenHashMap[Long, T]()
  protected[store] lazy val _cacheByIri: mutable.OpenHashMap[String, Set[T]] =
    mutable.OpenHashMap[String, Set[T]]()

  def store(resource: T): Unit = {
    cache(resource)
  }

  def cachedById(id: Long)     = _cache.get(id)
  def cachedByIri(iri: String) = _cacheByIri.get(iri).toList.flatten
  def countids                 = _cache.size
  def countiris                = _cacheByIri.size

  def cache(resource: T): Unit = {
    //    resource.status = CacheStatus.CACHED
    _cache += resource.id -> resource
    if (_cache.get(resource.id).isEmpty) throw new Exception(s"id ${resource.id} cached but not retrievable?")
    if (resource.iri.nonEmpty) _cacheByIri += resource.iri -> (_cacheByIri.getOrElse(resource.iri, Set()) + resource)
    resource.iris.foreach(iri => _cacheByIri += iri -> (_cacheByIri.getOrElse(iri, Set()) + resource))
  }

  def uncache(resource: T): Unit = _cache -= resource.id
  def uncacheByIri(resource: T): Unit = {
    if (resource.iri.nonEmpty) _cacheByIri.getOrElse(resource.iri, Set()) - resource match {
      case set: Set[T] if set.isEmpty => _cacheByIri -= resource.iri
      case set: Set[T]                => _cacheByIri += resource.iri -> set
    }
    resource.iris.foreach(iri =>
      _cacheByIri.getOrElse(iri, Set()) - resource match {
        case set: Set[T] if set.isEmpty => _cacheByIri -= iri
        case set: Set[T]                => _cacheByIri += iri -> set
    })
  }

  def byId(id: Long): Option[T]     = cachedById(id)
  def byIri(iri: String): Stream[T] = _cacheByIri.get(iri).map(_.toStream).getOrElse(Stream())

  def delete(resource: T): Unit = _cache -= resource.id

  def all(): Stream[T]
}
