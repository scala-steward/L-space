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

  def cachedById(id: Long) = _cache.get(id)

  def cache(resource: T): Unit = {
    //    resource.status = CacheStatus.CACHED
    _cache += resource.id -> resource
    if (resource.iri.nonEmpty) _cacheByIri += resource.iri -> (_cacheByIri.getOrElse(resource.iri, Set()) + resource)
    resource.iris.foreach(iri => _cacheByIri += iri -> (_cacheByIri.getOrElse(iri, Set()) + resource))
  }

  def byId(id: Long): Option[T]     = _cache.get(id)
  def byIri(iri: String): Stream[T] = _cacheByIri.get(iri).map(_.toStream).getOrElse(Stream())

  def delete(id: Long): Unit = _cache -= id

  def all(): Stream[T]
}
