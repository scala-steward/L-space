package lspace.structure

import monix.eval.Task
import monix.reactive.Observable

trait RApi[T <: Resource[Any]] {

  def apply(): Observable[T]

  def hasIri(iri: String, iris: String*): Observable[T] = hasIri(iri :: iris.toList)

  /**
    *
    * @param iris a set of uri's to get T for
    * @return
    */
  def hasIri(iris: List[String]): Observable[T]

  def hasId(id: Long): Task[Option[T]]
  trait Cached {
    def hasId(id: Long): Option[T]
    def dereferenceValue(t: Any): Any
    def count: Long
  }
  def cached: Cached

}
