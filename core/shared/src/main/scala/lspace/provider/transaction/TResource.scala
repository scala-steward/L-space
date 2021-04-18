package lspace.provider.transaction

import lspace.provider.mem.MemResource
import lspace.structure._

import scala.collection.mutable

object TResource {}
trait TResource[T] extends MemResource[T] {
  val graph: Transaction
  def id: Long = self.id

  val deletedEdges: mutable.Set[Long] = mutable.Set[Long]()

  override def iri: String       = self.iri
  override def iris: Set[String] = self.iris

  override def out(key: Property*): List[Any] =
    super.out(key: _*) ++ self
      .outE(key: _*)
      .filterNot(e => deletedEdges.contains(e.id))
      .map(_.to.asInstanceOf[graph.parent._Resource[Any]])
      .map(graph.wrapTR(_).map(_.value).value())

  override def outMap(key: Property*): Map[Property, List[Any]] =
    Seq(
      super.outMap(key: _*),
      self
        .outE(key: _*)
        .filterNot(e => deletedEdges.contains(e.id))
        .groupBy(_.key)
        .view.mapValues(
          _.map(_.to.asInstanceOf[graph.parent._Resource[Any]]).map(graph.wrapTR(_).map(_.value).value())
        ).toMap //wrapTR brings nested resources (aka within collections) in transaction context
    ).reduceLeft((r, m) =>
      m.foldLeft(r) { case (dict, (k, v)) =>
        dict.+(k -> (v ++ dict.getOrElse(k, List())))
      }
    )
  override def outE(key: Property*): List[Edge[T, Any]] =
    super.outE(key: _*) ++ self
      .outE(key: _*)
      .filterNot(e => deletedEdges.contains(e.id))
      .map(_.asInstanceOf[graph.parent._Resource[Edge[T, Any]]])
      .map(graph.wrapTR(_).value())
      .asInstanceOf[List[Edge[T, Any]]]
  override def outEMap(key: Property*): Map[Property, List[Edge[T, Any]]] =
    Seq(
      super.outEMap(key: _*),
      self
        .outE(key: _*)
        .filterNot(e => deletedEdges.contains(e.id))
        .map(_.asInstanceOf[graph.parent._Resource[Edge[T, Any]]])
        .map(graph.wrapTR(_).value().asInstanceOf[Edge[T, Any]])
        .groupBy(_.key)
    ).reduceLeft((r, m) =>
      m.foldLeft(r) { case (dict, (k, v)) =>
        dict + (k -> (v ++ dict.getOrElse(k, List())))
      }
    )
  override def in(key: Property*): List[Any] =
    super.in(key: _*) ++ self
      .inE(key: _*)
      .filterNot(e => deletedEdges.contains(e.id))
      .map(_.from.asInstanceOf[graph.parent._Resource[Edge[Any, T]]])
      .map(graph.wrapTR(_).map(_.value).value())
  override def inMap(key: Property*): Map[Property, List[Any]] =
    Seq(
      super.inMap(key: _*),
      self
        .inE(key: _*)
        .filterNot(e => deletedEdges.contains(e.id))
        .groupBy(_.key)
        .view
        .mapValues(_.map(_.from.asInstanceOf[graph.parent._Resource[Any]]).map(graph.wrapTR(_).map(_.value).value()))
        .toMap
    ).reduceLeft((r, m) =>
      m.foldLeft(r) { case (dict, (k, v)) =>
        dict + (k -> (v ++ dict.getOrElse(k, List())))
      }
    )
  override def inE(key: Property*): List[Edge[Any, T]] =
    super.inE(key: _*) ++ self
      .inE(key: _*)
      .filterNot(e => deletedEdges.contains(e.id))
      .map(_.asInstanceOf[graph.parent._Resource[Edge[Any, T]]])
      .map(graph.wrapTR(_).value())
      .asInstanceOf[List[Edge[Any, T]]]
  override def inEMap(key: Property*): Map[Property, List[Edge[Any, T]]] =
    Seq(
      super.inEMap(key: _*),
      self
        .inE(key: _*)
        .filterNot(e => deletedEdges.contains(e.id))
        .map(_.asInstanceOf[graph.parent._Resource[Edge[Any, T]]])
        .map(graph.wrapTR(_).value().asInstanceOf[Edge[Any, T]])
        .groupBy(_.key)
    ).reduceLeft((r, m) =>
      m.foldLeft(r) { case (dict, (k, v)) =>
        dict + (k -> (v ++ dict.getOrElse(k, List())))
      }
    )
}
