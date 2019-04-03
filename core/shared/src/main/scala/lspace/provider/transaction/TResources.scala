package lspace.provider.transaction

import java.util.concurrent.ConcurrentHashMap

import lspace.structure.{Resource, Resources}
import monix.eval.Task
import monix.reactive.Observable

import scala.collection.JavaConverters._

abstract class TResources[G <: Transaction](override val graph: G) extends Resources(graph) {
  import graph._

  override def apply(): Observable[Resource[_]] = {
    val tresources = super.apply()
    import scala.collection.JavaConverters._
    val idResourceMap: scala.collection.concurrent.Map[Long, Resource[_]] =
      new ConcurrentHashMap[Long, Resource[_]]().asScala
    tresources.map { resource =>
      idResourceMap += resource.id -> resource; resource
    } ++ parent.resources().filter(n => !idResourceMap.contains(n.id))
  }

  override def hasIri(iris: List[String]): Observable[Resource[_]] = {
    val fromTransaction = super.hasIri(iris)
    val fromParent = parent.resources
      .hasIri(iris)
      .mapEval {
        case n: parent._Node           => _TNode(n).task
        case e: parent._Edge[Any, Any] => _TEdge(e).task
        case v: parent._Value[Any]     => _TValue(v).task
      }
      .filter(n => nodes.deleted.contains(n.id) || edges.deleted.contains(n.id) || values.deleted.contains(n.id))
    val ids = fromTransaction.map(_.id)

    val idResourceMap: scala.collection.concurrent.Map[Long, Resource[_]] =
      new ConcurrentHashMap[Long, Resource[_]]().asScala
    fromTransaction.map { resource =>
      idResourceMap += resource.id -> resource; resource
    } ++ fromParent.filter(n => !idResourceMap.contains(n.id))
  }

  override def hasId(id: Long): Task[Option[Resource[_]]] = {
    if (nodes.deleted.contains(id) || edges.deleted.contains(id) || values.deleted.contains(id)) Task.now(None)
    else {
      for {
        r <- super
          .hasId(id)
        r1 <- if (r.nonEmpty) Task.now(r)
        else
          parent.resources
            .hasId(id)
            .flatMap {
              case Some(value) =>
                (value match {
                  case n: parent._Node           => _TNode(n).task
                  case e: parent._Edge[Any, Any] => _TEdge(e).task
                  case v: parent._Value[Any]     => _TValue(v).task
                }) map (Some(_))
              case None => Task.now(None)
            }
      } yield r1
    }
  }
}
