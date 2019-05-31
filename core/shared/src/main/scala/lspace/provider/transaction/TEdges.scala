package lspace.provider.transaction

import java.util.concurrent.ConcurrentHashMap

import lspace.structure.{Edge, Edges}
import monix.eval.Task
import monix.reactive.Observable

import scala.collection.JavaConverters._
import scala.collection.mutable

abstract class TEdges[G <: Transaction](override val graph: G) extends Edges(graph) {
  import graph._

  val added: mutable.HashSet[GEdge[_, _]] = mutable.HashSet[GEdge[_, _]]()
  val deleted: scala.collection.concurrent.Map[Long, parent.GEdge[_, _]] =
    new ConcurrentHashMap[Long, parent.GEdge[_, _]]().asScala

  override def apply(): Observable[Edge[_, _]] = {
    val tedges = super.apply()
    val idSet: scala.collection.concurrent.Map[Long, Edge[_, _]] =
      new ConcurrentHashMap[Long, Edge[_, _]]().asScala
    tedges.map { edge =>
      idSet += edge.id -> edge; edge
    } ++ parent.edges().filter(n => !idSet.contains(n.id))
  }
  override def count(): Task[Long] = edgeStore.count().map(_ + added.size - deleted.size)

  override def hasIri(iris: List[String]): Observable[Edge[_, _]] = {
    val fromTransaction = super.hasIri(iris)
    val fromParent = parent.edges
      .hasIri(iris)
      .asInstanceOf[Observable[parent._Edge[Any, Any]]]
      .mapEval(_TEdge(_).task)
      .filter(n => !deleted.contains(n.id))
    val idSet: scala.collection.concurrent.Map[Long, Edge[_, _]] =
      new ConcurrentHashMap[Long, Edge[_, _]]().asScala
    fromTransaction.map { edge =>
      idSet += edge.id -> edge; edge
    } ++ fromParent.filter(n => idSet.contains(n.id))
  }

  override def hasId(id: Long): Task[Option[Edge[_, _]]] = {
    if (deleted.contains(id)) Task.now(None)
    else
      for {
        r <- super
          .hasId(id)
        r1 <- if (r.nonEmpty) Task.now(r)
        else
          parent.edges
            .hasId(id)
            .flatMap {
              case Some(edge) => _TEdge(edge.asInstanceOf[parent._Edge[Any, Any]]).task.map(Some(_))
              case None       => Task.now(None)
            }
      } yield r1
  }
}
