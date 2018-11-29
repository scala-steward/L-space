package lspace.librarian.provider.transaction

import lspace.librarian.provider.mem.MemResource
import lspace.librarian.structure._

import scala.collection.mutable

object TResource {}
trait TResource[T] extends MemResource[T] {
  implicit def graph: Transaction
  def id: Long = self.id

  val deletedEdges: mutable.Set[Long] = mutable.Set[Long]()

  override def iri: String       = self.iri
  override def iris: Set[String] = self.iris

  override def out(key: Property*): List[Any] =
    super.out(key: _*) ++ self
      .outE(key: _*)
      .filterNot(e => deletedEdges.contains(e.id))
      .map(_.to)
      .map(graph.wrapTR(_))
      .map(_.value)
  override def outMap(key: Property*): Map[Property, List[Any]] =
    Seq(
      super.outMap(key: _*),
      self
        .outE(key: _*)
        .filterNot(e => deletedEdges.contains(e.id))
        .groupBy(_.key)
        .mapValues(_.map(_.to).map(graph.wrapTR(_)).map(_.value))
    ).reduceLeft((r, m) =>
      m.foldLeft(r) {
        case (dict, (k, v)) => dict + (k -> (v ++ dict.getOrElse(k, List())))
    })
  override def outE(key: Property*): List[Edge[T, Any]] =
    super.outE(key: _*) ++ self
      .outE(key: _*)
      .filterNot(e => deletedEdges.contains(e.id))
      .map(graph.wrapTR(_))
      .asInstanceOf[List[Edge[T, Any]]]
  override def outEMap(key: Property*): Map[Property, List[Edge[T, Any]]] =
    Seq(super.outEMap(key: _*),
        self
          .outE(key: _*)
          .filterNot(e => deletedEdges.contains(e.id))
          .map(graph.wrapTR(_).asInstanceOf[TEdge[T, Any]])
          .groupBy(_.key))
      .reduceLeft((r, m) =>
        m.foldLeft(r) {
          case (dict, (k, v)) => dict + (k -> (v ++ dict.getOrElse(k, List())))
      })
  override def in(key: Property*): List[Any] =
    super.in(key: _*) ++ self
      .inE(key: _*)
      .filterNot(e => deletedEdges.contains(e.id))
      .map(_.from)
      .map(graph.wrapTR(_))
      .map(_.value)
  override def inMap(key: Property*): Map[Property, List[Any]] =
    Seq(
      super.inMap(key: _*),
      self
        .inE(key: _*)
        .filterNot(e => deletedEdges.contains(e.id))
        .groupBy(_.key)
        .mapValues(_.map(_.from).map(graph.wrapTR(_)).map(_.value))
    ).reduceLeft((r, m) =>
      m.foldLeft(r) {
        case (dict, (k, v)) => dict + (k -> (v ++ dict.getOrElse(k, List())))
    })
  override def inE(key: Property*): List[Edge[Any, T]] =
    super.inE(key: _*) ++ self.inE(key: _*).filterNot(e => deletedEdges.contains(e.id))
  override def inEMap(key: Property*): Map[Property, List[Edge[Any, T]]] =
    Seq(super.inEMap(key: _*),
        self
          .inE(key: _*)
          .filterNot(e => deletedEdges.contains(e.id))
          .map(graph.wrapTR(_).asInstanceOf[TEdge[Any, T]])
          .groupBy(_.key))
      .reduceLeft((r, m) =>
        m.foldLeft(r) {
          case (dict, (k, v)) => dict + (k -> (v ++ dict.getOrElse(k, List())))
      })
}
